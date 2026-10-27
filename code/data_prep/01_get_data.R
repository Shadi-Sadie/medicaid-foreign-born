# Title: Download the ACS microdata (step 1 of the data preparation)
# Author: Shadi Seyedi
# Description: Downloads the American Community Survey (ACS) 1-year Public Use Microdata Sample
#              (PUMS) for 2011-2019. It uses the national files the Census Bureau publishes on
#              its website (one person file and one housing file per year), the same files that
#              IPUMS and most published studies start from.
#              This script ONLY downloads and reads. It makes no sample decisions beyond a broad
#              filter, recodes nothing and merges no outside data. Merging happens in 02, cleaning
#              and flags in 03, and the sample restrictions in make_sample() in code/00_setup.R.
#
# Filter applied when reading (and why):
#   - Ages 18 and older, no upper limit. The main sample is 26-64, but the wider range lets us
#     test other age groups without downloading again. People aged 65+ are also kept because the
#     likely-undocumented rule looks at each person's spouse, and a spouse can be over 64.
#   - Income-to-poverty ratio (POVPIP) 0-400%. The main sample is <=138%, but the wider range lets
#     us test other income cutoffs (e.g. 100% or 200%) without downloading again.
#   - Note: the ACS does not compute poverty status for people in institutional group quarters
#     (prisons, nursing homes) and most people in other group quarters (e.g. college dorms), so
#     the POVPIP filter already leaves them out.
#
# How it works, for each year:
#   1. Download the person file (about 600 MB) and the housing file (about 250 MB) as zip files
#      into data/raw/census_files/. A file already there is not downloaded again.
#   2. Unzip each file to a temporary folder, read only the columns we need, apply the filter.
#   3. Add the household variables (group-quarters type, region, household type) from the
#      housing file, matched on the household id SERIALNO.
#   4. Check the result, then save the people and their main weight to one file and the 80
#      replicate weights to a separate file (they are needed only for standard errors of
#      descriptive statistics, and keeping them out makes the main file much smaller).
#   A year whose output files already exist (with all the variables) is skipped, so a rerun after
#   an interruption continues where it stopped.
#
# Inputs:  none (the Census Bureau website, https://www2.census.gov/programs-surveys/acs/data/pums/).
#          No API key is needed.
# Outputs: data/raw/census_files/csv_pus_YYYY.zip, csv_hus_YYYY.zip   the original Census files
#          data/raw/acs_YYYY.rds        one row per person: all variables + main weight PWGTP
#          data/raw/repwt_YYYY.rds      SERIALNO, SPORDER, YEAR, PWGTP1-PWGTP80
#          data/raw/download_log.csv    one row per year: rows, file checksums, date, versions
# Usage:   from the project root,  Rscript code/data_prep/01_get_data.R
# Run time: about 15 minutes for all nine years (measured on 27 Oct 2026: downloading the 18 zip
#          files took about 2 minutes, reading and saving took 11 minutes, about 1.2 minutes per
#          year). Peak memory use was about 5 GB. Disk space: about 8 GB for the zip files plus
#          2 GB for the output. The script prints each year's time and an estimate of the time
#          left. Rerunning after all years are saved takes about a minute (it only checks the
#          saved files).
#
# Validation: on 27 Oct 2026 the output for Vermont and Wyoming, 2018 and 2019, was compared
#          person by person with the same data downloaded from the Census API (tidycensus).
#          The same people were selected, and all 57 variables were identical, except that the
#          API writes three "not applicable" values as placeholder codes (YOEP 1928, LANP 0999,
#          OCCP 0009) where these files have a blank.


####################
## Setup
####################

suppressPackageStartupMessages(library(data.table))   # fread() reads large CSV files quickly

# All paths are relative to the project root, so the script must be run from there.
if (!file.exists("run_all.R")) stop("Run this script from the project root folder.")

raw_dir  <- "data/raw"
zip_dir  <- file.path(raw_dir, "census_files")
log_file <- file.path(raw_dir, "download_log.csv")
dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)

# Large downloads can take longer than R's default limit of 60 seconds per file.
options(timeout = max(3600, getOption("timeout")))

years <- 2011:2019

# Address of each year's files on the Census Bureau website.
census_url <- function(year, file) {
        sprintf("https://www2.census.gov/programs-surveys/acs/data/pums/%d/1-Year/%s", year, file)
}

# The 50 states plus DC, as two-digit FIPS codes. The national person file contains exactly
# these (Puerto Rico has its own file, which we do not download: it is covered by a separate
# survey, the Puerto Rico Community Survey, and has a different Medicaid financing system).
# The list is used to check that every state is present. The codes skip numbers on purpose
# (03, 07, 14, 43 and 52 are not assigned to any state).
states <- c("01",  # Alabama
            "02",  # Alaska
            "04",  # Arizona
            "05",  # Arkansas
            "06",  # California
            "08",  # Colorado
            "09",  # Connecticut
            "10",  # Delaware
            "11",  # District of Columbia
            "12",  # Florida
            "13",  # Georgia
            "15",  # Hawaii
            "16",  # Idaho
            "17",  # Illinois
            "18",  # Indiana
            "19",  # Iowa
            "20",  # Kansas
            "21",  # Kentucky
            "22",  # Louisiana
            "23",  # Maine
            "24",  # Maryland
            "25",  # Massachusetts
            "26",  # Michigan
            "27",  # Minnesota
            "28",  # Mississippi
            "29",  # Missouri
            "30",  # Montana
            "31",  # Nebraska
            "32",  # Nevada
            "33",  # New Hampshire
            "34",  # New Jersey
            "35",  # New Mexico
            "36",  # New York
            "37",  # North Carolina
            "38",  # North Dakota
            "39",  # Ohio
            "40",  # Oklahoma
            "41",  # Oregon
            "42",  # Pennsylvania
            "44",  # Rhode Island
            "45",  # South Carolina
            "46",  # South Dakota
            "47",  # Tennessee
            "48",  # Texas
            "49",  # Utah
            "50",  # Vermont
            "51",  # Virginia
            "53",  # Washington
            "54",  # West Virginia
            "55",  # Wisconsin
            "56")  # Wyoming
stopifnot(length(states) == 51, !anyDuplicated(states))


####################
## Variables to read
####################

# Names follow the ACS PUMS data dictionary for each year:
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/

# From the person file
person_vars <- c(
        # Identifiers and weight
        "SERIALNO",  # household id (unique only within a year)
        "SPORDER",   # person number within the household
        "ST",        # state (FIPS code)
        "PWGTP",     # person weight
        # Demographics
        "AGEP",      # age
        "SEX",       # sex
        "MAR",       # marital status
        "RAC1P",     # race
        "HISP",      # Hispanic origin
        "DIS",       # disability
        "FER",       # gave birth in the past 12 months
        "SCHL",      # educational attainment
        "SCH",       # school enrollment
        # Geography
        "PUMA",      # public use microdata area (2011 uses 2000-based PUMAs, 2012+ 2010-based)
        "MIG",       # lived in the same house one year ago
        "MIGSP",     # state of residence one year ago (to test dropping people who moved state)
        # Immigration
        "NATIVITY",  # native or foreign-born
        "CIT",       # citizenship status
        "YOEP",      # year of entry to the US
        "POBP",      # place of birth
        "ENG",       # ability to speak English
        "LANX",      # speaks a language other than English at home
        "LANP",      # language spoken at home
        # Work and income
        "ESR",       # employment status
        "COW",       # class of worker (used for the government-job rule in the undocumented flag)
        "OCCP",      # occupation (2010 codes in 2011-2017, 2018 codes in 2018-2019)
        "SOCP",      # SOC occupation code (2010 SOC in 2011-2017, 2018 SOC in 2018-2019)
        "MIL",       # military service (used in the undocumented flag; codes changed in 2013, see below)
        "POVPIP",    # income-to-poverty ratio
        "SSP",       # Social Security income      (benefit receipt rules in the undocumented flag;
        "SSIP",      # Supplemental Security Income  these are linked to Medicaid eligibility, so they
        "PAP",       # public assistance income      are kept for a sensitivity version, see 03)
        # Health insurance (the outcomes)
        "HICOV",     # any health insurance coverage
        "HINS1",     # employer or union insurance
        "HINS2",     # insurance bought directly
        "HINS3",     # Medicare
        "HINS4",     # Medicaid or other means-tested public coverage
        "HINS5",     # TRICARE or other military health care
        "HINS6",     # VA health care
        "HINS7",     # Indian Health Service
        # Allocation (imputation) flags: 1 = the Census Bureau filled in the answer because it
        # was missing. Used to test dropping people whose answers were imputed.
        "FHINS1P", "FHINS2P", "FHINS3P", "FHINS4P", "FHINS5P", "FHINS6P", "FHINS7P",
        "FCITP",     # citizenship imputed
        "FYOEP",     # year of entry imputed
        "FPOBP",     # place of birth imputed
        # Coverage-edit flags: 1 = the Census Bureau set this coverage to "yes" based on other
        # answers (e.g. Medicaid for people receiving SSI or public assistance). This is not the
        # same as imputation, and FHINS4C matters because Medicaid (HINS4) is an outcome.
        "FHINS3C",   # Medicare given by the coverage edit
        "FHINS4C",   # Medicaid given by the coverage edit
        "FHINS5C"    # TRICARE given by the coverage edit
)
# FHICOVP (any-coverage imputation flag) is left out on purpose: it only exists from 2014 on.

# The 80 replicate weights (PWGTP1-PWGTP80), used for standard errors.
repwt_vars <- paste0("PWGTP", 1:80)

# The relationship-to-householder variable changed name and codes in 2019 (RELP before, RELSHIPP
# from 2019 on). Both are stored in one column, REL_RAW, with the original codes. 03_clean.R turns
# them into one coding that means the same in every year.
#   RELP     (2011-2018): 00 reference person, 01 husband/wife, 13 unmarried partner,
#                         16 institutional / 17 noninstitutional group quarters
#   RELSHIPP (2019):      20 reference person, 21 opposite-sex spouse, 23 same-sex spouse,
#                         22/24 unmarried partners, 37/38 group quarters
relationship_var <- function(year) if (year >= 2019) "RELSHIPP" else "RELP"

# From the housing file (one row per household or group-quarters person), matched on SERIALNO
housing_vars <- c(
        "TYPE",      # 1 housing unit, 2 institutional group quarters, 3 noninstitutional group quarters
        "REGION",    # census region (in the person file only from 2017, so always taken from here)
        "HHT",       # household/family type
        "WGTP",      # household weight
        "ADJINC"     # inflation adjustment factor for the income amounts (SSP, SSIP, PAP)
)

# KNOWN CODING FEATURES OF THESE FILES
# Checked on 27 Oct 2026 by reading every variable above in every year's file (all people, not a
# sample) and comparing with the Census data dictionaries for each year. The files saved here
# keep every code exactly as written in the Census files, except that blank fields are read as
# NA (missing). 03_clean.R must handle each point below, so that a value means the same in
# every year.
#
# 1. "Not applicable" is a blank field (read as NA) in every year and every variable, e.g. YOEP
#    for people born in the US, LANP for people who speak only English, MIGSP for people who
#    lived in the same house a year ago, OCCP and SOCP for people not working, FHINS4C for
#    people without Medicaid, POVPIP for most people in group quarters.
# 2. Numbers are written with leading zeros up to 2018 but not in 2019, e.g. POVPIP "086" in
#    2011-2018 and "86" in 2019, SSP "00000" vs "0". Codes such as ST "01", SCHL "01", HISP "01",
#    POBP "006" and OCCP "0430" keep their zeros in every year. Always compare as numbers.
# 3. YOEP is missing exactly for people born in the US (CIT == 1). People born in Puerto Rico or
#    the island areas, or abroad to American parents (CIT 2 and 3), are also "native" (NATIVITY
#    == 1) but do have a year of entry.
# 4. MIL changed meaning in 2013:
#      2011-2012: 1 active duty now, 2 active duty in past 12 months, 3 active duty more than
#                 12 months ago, 4 training only (Reserves/National Guard), 5 never served
#      2013-2019: 1 active duty now, 2 active duty in the past, 3 training only (Reserves/
#                 National Guard), 4 never served
#    So "ever on active duty" is codes 1-3 before 2013 but 1-2 from 2013 on.
# 5. FER has an extra code 8 ("suppressed") in 2012 only (15,793 people). Treat it as missing.
# 6. Code lists that change: LANP switched from 3-digit to 4-digit language codes in 2016;
#    OCCP and SOCP switched to the 2018 occupation codes in 2018; some POBP country codes exist
#    only in some years (e.g. 155 Estonia and 251 "Eastern Asia, not specified" only in 2011;
#    208 Cyprus only in 2012-2016; 167 Kosovo, 219 Kyrgyzstan, 228 Mongolia only from 2017).
#    Any list of codes (licensed occupations, world regions) must be checked for each code set.
# 7. PUMA uses 2000 boundaries in 2011 and 2010 boundaries from 2012 on.
# 8. SERIALNO changes format by year (e.g. "19" in 2011, "000000064" in 2016, "2017000000016"
#    in 2017, "2018GQ0000049" in 2018). It is unique only within a year, so always join on
#    YEAR too. SPORDER (person number) is written with a leading zero ("01"), while other
#    sources such as the Census API write "1"; compare it as a number when joining.
# 9. ADJINC has one value per year (e.g. 1018237 in 2011, meaning multiply by 1.018237). It is
#    needed only to compare dollar amounts across years; "any income > 0" needs no adjustment.
# 10. In the Census files the replicate weights are written in lower case (pwgtp1) in
#     2011-2016 and upper case (PWGTP1) from 2017; this script names them PWGTP1-PWGTP80 in
#     every year. The CSV file names inside the zip files also change in 2017 (ss11pusa.csv
#     before, psam_pusa.csv after); the script finds them by their .csv ending.


####################
## Helper functions
####################

# Saves an object without ever leaving a half-written file behind: it writes to a temporary file
# in the same folder first and renames it only once writing has finished. A file with the final
# name is therefore always complete, which is what lets a rerun safely skip it.
save_safely <- function(object, path) {
        tmp <- paste0(path, ".tmp")
        saveRDS(object, tmp)
        if (!file.rename(tmp, path)) stop("Could not rename ", tmp, " to ", path)
}

# Downloads one Census zip file unless it is already there. Like save_safely(), it downloads to
# a temporary name first, so an interrupted download never looks finished. It then checks that
# the zip can be opened and holds the two CSV files (the a and b halves of the country).
download_zip <- function(year, file) {
        dest <- file.path(zip_dir, sub("\\.zip$", sprintf("_%d.zip", year), file))
        if (!file.exists(dest)) {
                cat(year, ": downloading", file, "\n")
                tmp <- paste0(dest, ".tmp")
                status <- download.file(census_url(year, file), tmp, mode = "wb", quiet = TRUE)
                if (status != 0) stop("Download failed: ", census_url(year, file))
                if (!file.rename(tmp, dest)) stop("Could not rename ", tmp, " to ", dest)
        }
        csvs <- tryCatch(grep("\\.csv$", unzip(dest, list = TRUE)$Name, value = TRUE),
                         error = function(e) character(0))
        if (length(csvs) != 2) {
                stop(dest, " is damaged or incomplete. Delete it and rerun the script.")
        }
        dest
}

# Reads the wanted columns from both CSV halves inside a Census zip file.
#   - Column names are matched without regard to case (see note 10 above) and returned in
#     upper case.
#   - Every column is read as text, so codes keep their exact form (e.g. "01", "0430").
#   - Blank fields, which mean "not applicable" in these files, are read as NA.
#   - keep_rows, if given, is a function that returns TRUE for the rows to keep. It is applied
#     to each half right after reading, so memory use stays low.
read_census_zip <- function(zip, vars, keep_rows = NULL) {
        tmp_dir <- tempfile("acs_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE))     # delete the unzipped CSVs when done
        csvs <- grep("\\.csv$", unzip(zip, list = TRUE)$Name, value = TRUE)
        halves <- lapply(csvs, function(csv) {
                unzip(zip, files = csv, exdir = tmp_dir, junkpaths = TRUE)
                path <- file.path(tmp_dir, basename(csv))
                header <- names(fread(path, nrows = 0))
                wanted <- header[toupper(header) %in% vars]
                d <- fread(path, select = wanted, colClasses = "character",
                           na.strings = "", showProgress = FALSE)
                setnames(d, toupper(names(d)))
                unlink(path)                           # free the disk space right away
                if (!is.null(keep_rows)) d <- d[keep_rows(d)]
                d
        })
        rbindlist(halves, use.names = TRUE)
}

# The broad filter (see the header): ages 18+, income-to-poverty ratio 0-400%.
# People with no POVPIP (most group-quarters residents) are left out by the second condition.
keep_filter <- function(d) {
        age <- as.integer(d$AGEP)
        pov <- as.integer(d$POVPIP)
        !is.na(age) & age >= 18 & !is.na(pov) & pov >= 0 & pov <= 400
}


####################
## Download and read, year by year
####################

run_start  <- Sys.time()
year_times <- c()        # minutes each processed year took, to estimate the time left

for (year in years) {

        year_start <- Sys.time()
        acs_file   <- file.path(raw_dir, sprintf("acs_%d.rds", year))
        repwt_file <- file.path(raw_dir, sprintf("repwt_%d.rds", year))
        all_vars   <- c(person_vars, relationship_var(year), repwt_vars)

        # The variables each finished file must contain (REL_RAW is the renamed relationship).
        expected_acs <- c(person_vars, "REL_RAW", housing_vars, "YEAR")

        # This year is already finished in an earlier run: skip it, but first make sure the saved
        # file has every variable in the current list. If a variable was added to the lists above
        # after the file was made, stop and say which file to delete.
        if (file.exists(acs_file) && file.exists(repwt_file)) {
                missing_vars <- setdiff(expected_acs, names(readRDS(acs_file)))
                if (length(missing_vars) > 0) {
                        stop(acs_file, " was made with an older variable list and lacks: ",
                             paste(missing_vars, collapse = ", "),
                             ". Delete ", acs_file, " and ", repwt_file, " and rerun.")
                }
                cat(year, ": already done, skipping.\n")
                next
        }

        ## Step 1: download the two Census files (skipped if already downloaded)

        person_zip  <- download_zip(year, "csv_pus.zip")
        housing_zip <- download_zip(year, "csv_hus.zip")

        ## Step 2: read the person file, keeping only the filtered people

        cat(year, ": reading the person file\n")
        acs <- read_census_zip(person_zip, all_vars, keep_rows = keep_filter)

        # Store the relationship variable under one name for every year (see above).
        setnames(acs, relationship_var(year), "REL_RAW")
        acs[, YEAR := year]

        ## Step 3: add the household variables from the housing file

        cat(year, ": reading the housing file\n")
        housing <- read_census_zip(housing_zip, c("SERIALNO", housing_vars))
        stopifnot(!anyDuplicated(housing$SERIALNO))        # one row per household
        if ("REGION" %in% names(acs)) acs[, REGION := NULL] # 2017+ person files also have it
        n_before <- nrow(acs)
        acs <- merge(acs, housing, by = "SERIALNO", all.x = TRUE, sort = FALSE)
        rm(housing)

        ## Step 4: checks before saving

        # The merge kept every person and found a household for each one.
        stopifnot(nrow(acs) == n_before, !anyNA(acs$TYPE))

        # Every requested variable is there.
        missing_vars <- setdiff(c(expected_acs, repwt_vars), names(acs))
        if (length(missing_vars) > 0) stop(year, ": missing variables: ", paste(missing_vars, collapse = ", "))

        # Exactly the 50 states and DC are present.
        stopifnot(setequal(unique(acs$ST), states))

        # Each person appears once (a person is household id + person number).
        stopifnot(!anyDuplicated(acs[, .(SERIALNO, SPORDER)]))

        # The filter worked, and the inflation factor has one value for the year.
        stopifnot(all(as.integer(acs$AGEP) >= 18),
                  all(as.integer(acs$POVPIP) >= 0 & as.integer(acs$POVPIP) <= 400),
                  uniqueN(acs$ADJINC) == 1)

        ## Step 5: split off the replicate weights and save both files

        # Weights are numbers; everything else stays as text, as written in the Census files.
        weight_cols <- c("PWGTP", "WGTP", repwt_vars)
        acs[, (weight_cols) := lapply(.SD, as.numeric), .SDcols = weight_cols]

        repwt <- acs[, c("SERIALNO", "SPORDER", "YEAR", repwt_vars), with = FALSE]  # joined back on SERIALNO + SPORDER + YEAR
        acs[, (repwt_vars) := NULL]
        setcolorder(acs, c("YEAR", "SERIALNO", "SPORDER", "ST"))

        save_safely(as.data.frame(acs),   acs_file)
        save_safely(as.data.frame(repwt), repwt_file)

        ## Step 6: record the year in the log
        # The MD5 checksums identify the exact Census files used. Anyone can compute the
        # checksum of their own download (tools::md5sum) and compare.

        log_row <- data.frame(year               = year,
                              n_rows             = nrow(acs),
                              n_states           = uniqueN(acs$ST),
                              person_zip_md5     = unname(tools::md5sum(person_zip)),
                              housing_zip_md5    = unname(tools::md5sum(housing_zip)),
                              processed_at       = format(Sys.time(), "%Y-%m-%d %H:%M"),
                              data_table_version = as.character(packageVersion("data.table")),
                              r_version          = R.version.string)
        write.table(log_row, log_file, sep = ",", row.names = FALSE,
                    append = file.exists(log_file), col.names = !file.exists(log_file))

        # Report how long this year took and estimate the time left from the average so far.
        mins <- as.numeric(difftime(Sys.time(), year_start, units = "mins"))
        year_times <- c(year_times, mins)
        years_left <- sum(years > year)
        cat(sprintf("%d : done, %s people saved, %.1f min. About %.0f min left for %d more year(s).\n",
                    year, format(nrow(acs), big.mark = ","), mins,
                    mean(year_times) * years_left, years_left))
        rm(acs, repwt)
        gc(verbose = FALSE)
}

cat(sprintf("All years done in %.0f min. Files are in %s\n",
            as.numeric(difftime(Sys.time(), run_start, units = "mins")), raw_dir))
# The zip files in data/raw/census_files/ are the original Census files. They can be deleted
# to save disk space (about 8 GB), but keeping them lets a rerun rebuild the yearly files
# without downloading again.
