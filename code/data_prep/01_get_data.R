# Title: Download the ACS microdata (step 1 of the data preparation)
# Author: Shadi Seyedi
# Description: Downloads the American Community Survey (ACS) 1-year Public Use Microdata Sample
#              (PUMS) for 2011-2019 from the Census Bureau API, using the tidycensus package.
#              This script ONLY downloads. It makes no sample decisions beyond a broad filter,
#              recodes nothing and merges nothing. Merging happens in 02, cleaning and flags in
#              03, and the sample restrictions in make_sample() in code/00_setup.R.
#
# Filter applied at the source (and why):
#   - Ages 18 and older, no upper limit. The main sample is 26-64, but the wider range lets us
#     test other age groups without downloading again. People aged 65+ are also kept because the
#     likely-undocumented rule looks at each person's spouse, and a spouse can be over 64.
#   - Income-to-poverty ratio (POVPIP) 0-400%. The main sample is <=138%, but the wider range lets
#     us test other income cutoffs (e.g. 100% or 200%) without downloading again.
#   - Note: the ACS does not compute poverty status for people in institutional group quarters
#     (prisons, nursing homes) and some others (e.g. college dorms), so the POVPIP filter already
#     leaves them out of the download.
#
# How the download works:
#   - The data are downloaded one state at a time, for each year (51 states incl. DC x 9 years).
#     Each state-year is saved to its own small file as soon as it arrives. If a request fails,
#     it is tried again a few times. If the script stops (lost connection, computer restart),
#     rerunning it skips everything already saved and continues where it stopped.
#   - Once all 51 states of a year are saved, they are stacked into one file per year, and the
#     80 replicate weights are split off into a separate file (they are needed only for standard
#     errors of descriptive statistics, and keeping them out makes the main file much smaller).
#
# Inputs:  none (the Census API). Requires the environment variable CENSUS_API_KEY,
#          set in ~/.Renviron (a free key: https://api.census.gov/data/key_signup.html).
# Outputs: data/raw/by_state/YYYY/SS.rds   one file per state-year (download cache)
#          data/raw/acs_YYYY.rds           one row per person: all variables + main weight PWGTP
#          data/raw/repwt_YYYY.rds         SERIALNO, SPORDER, YEAR, PWGTP1-PWGTP80
#          data/raw/download_log.csv       one row per year: rows, states, date, versions
# Usage:   from the project root,  Rscript code/data_prep/01_get_data.R
# Run time: about 15-20 minutes per year, 2.5-3 hours for all nine years (measured on 27 Oct 2026:
#          California 2019 took 89 s, Texas 68 s, a small state about 6 s). The Census API's
#          speed varies, so it can take longer. The script prints each year's time and an
#          estimate of the time left. Rerunning after all years are saved takes seconds.


####################
## Setup
####################

library(tidycensus)
library(dplyr)

# All paths are relative to the project root, so the script must be run from there.
if (!file.exists("run_all.R")) stop("Run this script from the project root folder.")

# The Census API key is read from the environment, never written in the code.
if (Sys.getenv("CENSUS_API_KEY") == "") {
        stop("CENSUS_API_KEY is not set. Add CENSUS_API_KEY=<your key> to ~/.Renviron and restart R.")
}

raw_dir   <- "data/raw"
state_dir <- file.path(raw_dir, "by_state")
log_file  <- file.path(raw_dir, "download_log.csv")
dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

years <- 2011:2019

# The 50 states plus DC, as two-digit FIPS codes. Written out explicitly so the study area does
# not depend on any package's lookup table. Puerto Rico (72) and the island areas are not
# included: they have their own survey (the Puerto Rico Community Survey) and a different
# Medicaid financing system, so they are outside this study.
# The codes skip numbers on purpose (03, 07, 14, 43 and 52 are not assigned to any state).
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

# How many times to try one state-year before giving up, and how long to wait between tries.
max_tries  <- 5
wait_secs  <- 30     # wait 30 s after the 1st failure, 60 s after the 2nd, and so on


####################
## Variables to download
####################

# Names follow the ACS PUMS data dictionary. tidycensus always adds SERIALNO (household id),
# SPORDER (person number in the household), PWGTP (person weight), WGTP (household weight)
# and ST (state), so they are not listed here.

person_vars <- c(
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
        # Household
        "HHT",       # household/family type
        "TYPE",      # housing unit or group quarters
        # Geography
        "REGION",    # census region
        "PUMA",      # public use microdata area (2011 uses 2000-based PUMAs, 2012+ 2010-based)
        "MIG",       # lived in the same house one year ago
        "MIGSP",     # state of residence one year ago (to test dropping people who moved state)
        # Immigration
        "NATIVITY",  # US-born or foreign-born
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

# The relationship-to-householder variable changed name and codes in 2019 (RELP before, RELSHIPP
# from 2019 on). Both are stored in one column, REL_RAW, with the original codes. 03_clean.R turns
# them into one coding that means the same in every year.
#   RELP     (2011-2018): 00 reference person, 01 husband/wife, 13 unmarried partner,
#                         16/17 group quarters
#   RELSHIPP (2019):      20 reference person, 21 opposite-sex spouse, 23 same-sex spouse,
#                         22/24 unmarried partners, 37/38 group quarters
relationship_var <- function(year) if (year >= 2019) "RELSHIPP" else "RELP"

# KNOWN CODING DIFFERENCES ACROSS YEARS
# The files saved here keep the codes exactly as the API sends them. The differences below were
# checked against the Census API data dictionaries for every year 2011-2019 and against real
# downloaded data. 03_clean.R must handle each one, so that a value means the same in every year.
#
# 1. "Not applicable" is written differently before and after 2017:
#      ENG, FER, COW:  "0" in 2011-2016, "b" in 2017-2019
#      FHINS4C (also 3C, 5C), no coverage of that type: "" (blank) in 2011, "-1" in 2012-2016,
#                       "b" in 2017-2019
#      MIGSP, lived in the same house a year ago: "0" in 2011-2016, "bbb" in 2017-2019
#      OCCP, not working: "9" in 2011-2016, "0009" in 2017-2019
#      SOCP, not working: "-1" in 2011-2016, "0000-1" in 2017-2019
# 2. Leading zeros appear from 2017 on (e.g. SCHL "1" before vs "01" after, OCCP "430" vs "0430").
#    Compare these codes as numbers, not as text.
# 3. Some "not applicable" codes look like real values. Never use the code alone; use the
#    variable that says who the question applies to:
#      YOEP for people born in the US is a year, not missing: 1918 in 2011, 1920 in 2012-2016,
#        1924 in 2017, 1928 in 2018-2019. Set YOEP to NA when NATIVITY == 1.
#      LANP for people who speak only English is 600 in 2011-2015 and 999 in 2016-2019.
#        Set LANP to NA when LANX == 2.
# 4. MIL changed meaning in 2013:
#      2011-2012: 1 active duty now, 2 active duty in past 12 months, 3 active duty more than
#                 12 months ago, 4 training only (Reserves/National Guard), 5 never served
#      2013-2019: 1 active duty now, 2 active duty in the past, 3 training only (Reserves/
#                 National Guard), 4 never served
#    So "ever on active duty" is codes 1-3 before 2013 but 1-2 from 2013 on.
# 5. FER has an extra code 8 ("suppressed") in 2012 only. Treat it as missing.
# 6. Code lists that change: LANP switched from 3-digit to 4-digit language codes in 2016;
#    OCCP and SOCP switched to the 2018 occupation codes in 2018; some POBP country codes exist
#    only in some years (e.g. 155 Estonia and 251 "Eastern Asia, not specified" only in 2011;
#    208 Cyprus only in 2012-2016; 167 Kosovo, 219 Kyrgyzstan, 228 Mongolia only from 2017).
#    Any list of codes (licensed occupations, world regions) must be checked for each code set.
# 7. PUMA uses 2000 boundaries in 2011 and 2010 boundaries from 2012 on.
# 8. SSP, SSIP and PAP write zero as "00000" in some years and "0" in others; compare as numbers.
# 9. SERIALNO changes format by year (e.g. "5151" in 2011, "2017000000258" in 2017,
#    "2018GQ0008626" in 2018). It is unique only within a year, so always join on YEAR too.

# Broad filter applied by the Census API, so unwanted rows are never downloaded (see the header).
row_filter <- list(AGEP = 18:99, POVPIP = 0:400)


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

# Downloads one state for one year, trying again if the request fails.
# (Inside get_pums(), the ~130 columns are fetched as three requests of up to 45 variables each,
# so a single state-year is itself a few API calls. Any of them can fail; a retry redoes all.)
download_state_year <- function(state, year) {
        for (attempt in 1:max_tries) {
                result <- tryCatch(
                        get_pums(
                                variables        = c(person_vars, relationship_var(year)),
                                state            = state,
                                year             = year,
                                survey           = "acs1",
                                variables_filter = row_filter,
                                rep_weights      = "person",   # the 80 replicate person weights
                                recode           = FALSE       # keep the original ACS codes
                        ),
                        error = function(e) e
                )
                if (!inherits(result, "error")) return(result)

                # Failed: report it and wait longer each time before trying again.
                message(sprintf("  State %s, %d: attempt %d of %d failed (%s)",
                                state, year, attempt, max_tries, conditionMessage(result)))
                if (attempt < max_tries) Sys.sleep(wait_secs * attempt)
        }
        stop(sprintf("State %s, %d failed %d times. Rerun the script later; ",
                     state, year, max_tries),
             "states already saved will be skipped.")
}


####################
## Download, year by year
####################

run_start  <- Sys.time()
year_times <- c()        # minutes each downloaded year took, to estimate the time left

for (year in years) {

        year_start <- Sys.time()
        acs_file   <- file.path(raw_dir, sprintf("acs_%d.rds", year))
        repwt_file <- file.path(raw_dir, sprintf("repwt_%d.rds", year))

        # This year is already finished in an earlier run: skip it.
        if (file.exists(acs_file) && file.exists(repwt_file)) {
                cat(year, ": already downloaded, skipping.\n")
                next
        }

        ## Step 1: download each state into its own file

        year_dir <- file.path(state_dir, year)
        dir.create(year_dir, showWarnings = FALSE)

        for (state in states) {
                state_file <- file.path(year_dir, paste0(state, ".rds"))
                if (file.exists(state_file)) next          # saved in an earlier run

                cat(year, ": downloading state", state, "\n")
                d <- download_state_year(state, year)

                # tidycensus can guess different column types in different states (e.g. a column
                # read as numbers in one state and as text in another), which would stop the
                # states from stacking. All columns except the weights are therefore stored as
                # text, exactly as the API sends them. 03_clean.R converts them to numbers.
                weight_cols <- grep("WGTP", names(d), value = TRUE)
                d <- mutate(d, across(-all_of(weight_cols), as.character))

                # Store the relationship variable under one name for every year (see above).
                names(d)[names(d) == relationship_var(year)] <- "REL_RAW"

                save_safely(d, state_file)
        }

        ## Step 2: stack the 51 states into one data set for the year

        state_files <- file.path(year_dir, paste0(states, ".rds"))
        stopifnot(all(file.exists(state_files)))          # every state must be there

        acs <- bind_rows(lapply(state_files, readRDS))
        acs$YEAR <- year

        ## Step 3: checks on the stacked data before saving

        # Every requested variable came back.
        expected <- c(person_vars, "REL_RAW", "SERIALNO", "SPORDER", "PWGTP", "ST",
                      paste0("PWGTP", 1:80))
        missing_vars <- setdiff(expected, names(acs))
        if (length(missing_vars) > 0) stop(year, ": missing variables: ", paste(missing_vars, collapse = ", "))

        # All 51 states are present.
        stopifnot(n_distinct(acs$ST) == length(states))

        # Each person appears once (a person is identified by household id + person number).
        stopifnot(!anyDuplicated(acs[c("SERIALNO", "SPORDER")]))

        # The source filter worked.
        stopifnot(all(as.numeric(acs$AGEP) >= 18),
                  all(as.numeric(acs$POVPIP) >= 0 & as.numeric(acs$POVPIP) <= 400))

        ## Step 4: split off the replicate weights and save both files

        repwt_cols <- paste0("PWGTP", 1:80)
        repwt <- acs[c("SERIALNO", "SPORDER", "YEAR", repwt_cols)]  # joined back on SERIALNO + SPORDER + YEAR
        acs   <- acs[setdiff(names(acs), repwt_cols)]

        save_safely(as.data.frame(acs),   acs_file) 
        save_safely(as.data.frame(repwt), repwt_file)

        ## Step 5: record the download in the log

        log_row <- data.frame(year               = year,
                              n_rows             = nrow(acs),
                              n_states           = n_distinct(acs$ST),
                              downloaded_at      = format(Sys.time(), "%Y-%m-%d %H:%M"),
                              tidycensus_version = as.character(packageVersion("tidycensus")),
                              r_version          = R.version.string)
        write.table(log_row, log_file, sep = ",", row.names = FALSE,
                    append = file.exists(log_file), col.names = !file.exists(log_file))

        # Report how long this year took and estimate the time left from the average so far.
        mins <- as.numeric(difftime(Sys.time(), year_start, units = "mins"))
        year_times <- c(year_times, mins)
        years_left <- sum(years > year)
        cat(sprintf("%d : done, %s people saved, %.0f min. About %.0f min left for %d more year(s).\n",
                    year, format(nrow(acs), big.mark = ","), mins,
                    mean(year_times) * years_left, years_left))
        rm(acs, repwt)
}

cat(sprintf("All years downloaded in %.0f min. Files are in %s\n",
            as.numeric(difftime(Sys.time(), run_start, units = "mins")), raw_dir))
# The per-state files in data/raw/by_state/ are only a download cache. They can be deleted once
# all years are finished, but keeping them lets a rerun rebuild the yearly files without the API.
