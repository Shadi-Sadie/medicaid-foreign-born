# Title: Merge the ACS years with the state data (step 2 of the data preparation)
# Author: Shadi Seyedi
# Description: Builds one dataset: every ACS person from 01_get_data.R (2011-2019) with the
#              state-by-year variables the analysis needs added to each person.
#              This script ONLY merges. It recodes no ACS variable and drops no person; recoding
#              happens in 03 and the sample restrictions (ages 26-64, income <=138% of poverty,
#              no group quarters, non-citizens in the US 5+ years, no US citizens born abroad or
#              in the territories) in make_sample() in code/00_setup.R.
#
# How it works:
#   Part 1 builds a table with one row per state and year (51 states incl. DC x 9 years = 459
#   rows) from the sources below, checks it, and saves it as data/state_year.csv.
#   Part 2 stacks the nine yearly ACS files, adds the state table to each person (matched on the
#   state FIPS code ST and YEAR, never on state names), checks the result and saves it.
#
# State variables and their sources:
#   StateN, expansion, ExpansionY, Expan_STS, expansion_date, expansion_year_earliest
#                 data/expansion.csv: Medicaid expansion status and dates (KFF, see README)
#   treat         1 from the calendar year of expansion on (the year the ACS records it), else 0
#   treat_frac    share of the year with expansion in effect: 0 before, the share of the
#                 expansion year after the start date (e.g. 0.33 for Alaska, 1 September 2015),
#                 1 after. For a sensitivity check of treat.
#   UnempR        annual average unemployment rate (%), BLS Local Area Unemployment Statistics,
#                 not seasonally adjusted, series LAUST<FIPS>0000000000003, period M13
#                 (the annual average BLS publishes). Downloaded by this script.
#   ADA           average ADA (Americans for Democratic Action) score of the state's members of
#                 Congress, data/Pol.xlsx sheet "Sheet4" (built from the yearly sheets)
#   IPC           Immigrant Policy Climate index without the health-care items, data/IPC.csv
#   IPCINDX       full Immigrant Policy Climate index, data/IPC_Final.csv (annual_score)
#                 Both IPC files are from Goleen Samari (https://www.goleensamari.com/data).
#   ipc_imputed   1 for North Carolina 2016, whose IPC and IPCINDX are imputed (see below)
#
# Gaps in the state data:
#   - ADA for DC, all years: DC has no voting member of Congress, so it has no ADA score.
#     Left as NA here.
#   - IPC and IPCINDX for North Carolina 2016: missing in both of Samari's files. Imputed by
#     carrying 2015 forward (Part 1, step 6, gives the reason) and flagged with ipc_imputed.
#
# Inputs:  data/raw/acs_YYYY.rds (from 01_get_data.R), data/expansion.csv, data/Pol.xlsx,
#          data/IPC.csv, data/IPC_Final.csv,
#          data/raw/bls/la.data.2.AllStatesU (downloaded here if missing, see below)
# Outputs: data/state_year.csv        one row per state and year (small, tracked in git)
#          data/clean/acs_merged.rds  one row per person: all ACS variables + state variables
# Usage:   from the project root,  Rscript code/data_prep/02_merge.R
#          BLS refuses downloads that do not name a contact. If the BLS file is not yet in
#          data/raw/bls/, set your email first:  BLS_CONTACT=you@example.org Rscript ...


####################
## Setup
####################

suppressPackageStartupMessages({
        library(data.table)   # fast stacking and merging of the large ACS files
        library(readxl)       # reads Pol.xlsx
})

# All paths are relative to the project root, so the script must be run from there.
if (!file.exists("run_all.R")) stop("Run this script from the project root folder.")

raw_dir   <- "data/raw"
bls_dir   <- file.path(raw_dir, "bls")
clean_dir <- "data/clean"
dir.create(bls_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

years <- 2011:2019

# Saves an object without ever leaving a half-written file behind (as in 01_get_data.R).
save_safely <- function(object, path) {
        tmp <- paste0(path, ".tmp")
        saveRDS(object, tmp)
        if (!file.rename(tmp, path)) stop("Could not rename ", tmp, " to ", path)
}


####################
## Part 1: the state-by-year table
####################

## 1. States and Medicaid expansion

# ST is written as two-digit text ("01"), exactly as in the ACS files, so the two match directly.
expan <- fread("data/expansion.csv")
expan[, ST := sprintf("%02d", ST)]
stopifnot(nrow(expan) == 51, !anyDuplicated(expan$ST))

# ExpansionY is the expansion year the analysis uses (treat, ttot, year_treated). expansion_date
# is the exact date, kept for reference only. It is a base R Date, not data.table's IDate: an IDate
# column read without data.table loaded crashes survey::svydesign() on the full sample (table4.R).
expan[, `:=`(expansion      = as.integer(ExpansionY > 0),          # expanded by 2019
             ExpansionY     = ifelse(ExpansionY > 0, ExpansionY, NA_integer_),
             expansion_date = as.Date(aca_expansion_date))]

# One row per state and year
state_year <- CJ(ST = expan$ST, YEAR = years)
state_year <- merge(state_year,
                    expan[, .(ST, StateN, expansion, ExpansionY, Expan_STS, expansion_date,
                              expansion_year_earliest, aca_exposure_frac_first_year)],
                    by = "ST")
state_year[, treat := as.integer(!is.na(ExpansionY) & YEAR >= ExpansionY)]
state_year[, treat_frac := fcase(is.na(ExpansionY) | YEAR < ExpansionY, 0,
                                 YEAR == ExpansionY,                    aca_exposure_frac_first_year,
                                 default = 1)]
state_year[, aca_exposure_frac_first_year := NULL]

# Turns state names into FIPS codes, for the sources that only have names.
name_to_st <- function(name) {
        name <- ifelse(name == "DC", "District of Columbia", name)   # spelling in the IPC files
        st <- expan$ST[match(tolower(trimws(name)), tolower(expan$StateN))]
        if (anyNA(st)) stop("Unknown state name(s): ", paste(unique(name[is.na(st)]), collapse = ", "))
        st
}

## 2. Unemployment rate (BLS)

bls_file <- file.path(bls_dir, "la.data.2.AllStatesU")
if (!file.exists(bls_file)) {
        contact <- Sys.getenv("BLS_CONTACT")
        if (contact == "") stop("Set BLS_CONTACT to your email to download the BLS file (see Usage).")
        url <- "https://download.bls.gov/pub/time.series/la/la.data.2.AllStatesU"
        cat("Downloading", url, "\n")
        tmp <- paste0(bls_file, ".tmp")
        status <- download.file(url, tmp, mode = "wb", quiet = TRUE,
                                headers = c("User-Agent" = paste("Research replication script,", contact)))
        if (status != 0) stop("Download failed: ", url)
        if (!file.rename(tmp, bls_file)) stop("Could not rename ", tmp, " to ", bls_file)
}
cat("BLS file:", bls_file, " MD5", unname(tools::md5sum(bls_file)), "\n")

bls <- fread(bls_file, colClasses = "character", strip.white = TRUE)
# Statewide unemployment rate series: LAUST + state FIPS + 0000000000003
unemp <- bls[grepl("^LAUST[0-9]{2}0000000000003$", series_id) & period == "M13" &
             year %in% years,
             .(ST = substr(series_id, 6, 7), YEAR = as.integer(year), UnempR = as.numeric(value))]
unemp <- unemp[ST %in% expan$ST]                  # drops Puerto Rico (72)
rm(bls)

## 3. ADA scores

ada <- as.data.table(read_excel("data/Pol.xlsx", sheet = "Sheet4"))
setnames(ada, c("YEAR", "StateN", "ST", "ADA"))
ada <- ada[YEAR %in% years, .(ST = sprintf("%02d", as.integer(ST)), YEAR = as.integer(YEAR), ADA)]

# California 2012: replace the Sheet4 value by the average of the 2012 sheet's member scores
# (column 1 state, column 3 score), which is also the sheet's own average.
sheet12 <- as.data.table(read_excel("data/Pol.xlsx", sheet = "2012", col_names = FALSE,
                                    .name_repair = "minimal"))[, 1:3]
setnames(sheet12, c("state", "member", "score"))
ca_2012 <- sheet12[state == "CALIFORNIA", mean(as.numeric(score), na.rm = TRUE)]
stopifnot(abs(ca_2012 - 0.5796) < 0.001)
ada[ST == "06" & YEAR == 2012, ADA := ca_2012]

## 4. Immigrant Policy Climate index (Samari)

ipc      <- fread("data/IPC.csv")
ipc_full <- fread("data/IPC_Final.csv")
ipc      <- ipc[year %in% years,      .(ST = name_to_st(state), YEAR = as.integer(year), IPC = annual_score)]
ipc_full <- ipc_full[year %in% years, .(ST = name_to_st(state), YEAR = as.integer(year), IPCINDX = annual_score)]

## 5. Put the pieces together and check

for (piece in list(unemp, ada, ipc, ipc_full)) {
        stopifnot(!anyDuplicated(piece[, .(ST, YEAR)]))
        state_year <- merge(state_year, piece, by = c("ST", "YEAR"), all.x = TRUE)
}

# 51 states x 9 years, one row each
stopifnot(nrow(state_year) == 51 * length(years),
          !anyDuplicated(state_year[, .(ST, YEAR)]))

# The only missing values are the known gaps listed in the header.
stopifnot(!anyNA(state_year$UnempR),
          identical(state_year[is.na(ADA), unique(ST)], "11"),
          state_year[is.na(ADA), .N] == length(years),
          identical(state_year[is.na(IPC),     paste(ST, YEAR)], "37 2016"),
          identical(state_year[is.na(IPCINDX), paste(ST, YEAR)], "37 2016"))

## 6. Impute North Carolina 2016 in the IPC indexes

# North Carolina 2016 is missing in both of Samari's files. We carry 2015 forward (IPC -2,
# IPCINDX -3). Reason: in IPC_Final.csv, the only policy item that differs between North
# Carolina's 2015 and 2017 rows is secure_communities (0 -> -1). In every other state that item
# is the same in 2016 as in 2015, and never the same as in 2017 (the federal Secure Communities
# program was ended in late 2014 and restarted in January 2017). The overall IPC score is also the
# same in 2016 as in 2015 in 96% of the other states. ipc_imputed marks the row, so a sensitivity
# check can drop it.
# (The old 03_clean.R set IPC to -3 with no stated source; -3 is North Carolina's 2017 value.)
nc_2015 <- state_year[ST == "37" & YEAR == 2015]
stopifnot(nc_2015$IPC == -2, nc_2015$IPCINDX == -3,
          state_year[ST == "37" & YEAR == 2017, IPC == -3 & IPCINDX == -4])
state_year[, ipc_imputed := as.integer(ST == "37" & YEAR == 2016)]
state_year[ipc_imputed == 1, `:=`(IPC = nc_2015$IPC, IPCINDX = nc_2015$IPCINDX)]
stopifnot(!anyNA(state_year$IPC), !anyNA(state_year$IPCINDX))

# Values in plausible ranges
stopifnot(state_year[, all(UnempR > 1 & UnempR < 20)],
          state_year[!is.na(ADA), all(ADA >= 0 & ADA <= 1)],
          state_year[, all(treat_frac >= 0 & treat_frac <= 1)],
          state_year[, all((treat == 1) == (treat_frac > 0))])

setcolorder(state_year, c("ST", "StateN", "YEAR"))
fwrite(state_year, "data/state_year.csv")
cat("State table saved: data/state_year.csv,", nrow(state_year), "rows\n")


####################
## Part 2: stack the ACS years and add the state variables
####################

acs <- rbindlist(lapply(years, function(year) {
        f <- file.path(raw_dir, sprintf("acs_%d.rds", year))
        if (!file.exists(f)) stop(f, " is missing. Run code/data_prep/01_get_data.R first.")
        cat(year, ": reading", f, "\n")
        as.data.table(readRDS(f))
}), use.names = TRUE, fill = FALSE)    # fill = FALSE: every year must have the same variables

n_people <- nrow(acs)
stopifnot(!anyDuplicated(acs[, .(YEAR, SERIALNO, SPORDER)]))

acs <- merge(acs, state_year, by = c("ST", "YEAR"), all.x = TRUE, sort = FALSE)

# Every person kept, and every person found their state and year
stopifnot(nrow(acs) == n_people, !anyNA(acs$StateN))

setcolorder(acs, c("YEAR", "SERIALNO", "SPORDER", "ST", "StateN"))
setorder(acs, YEAR, SERIALNO, SPORDER)
cat("Saving", format(n_people, big.mark = ","), "people to data/clean/acs_merged.rds\n")
save_safely(as.data.frame(acs), file.path(clean_dir, "acs_merged.rds"))

# A short summary to compare with the paper: people per year, and states by expansion status
print(acs[, .N, by = YEAR])
print(state_year[YEAR == 2019, .N, by = .(expansion, Expan_STS)])
