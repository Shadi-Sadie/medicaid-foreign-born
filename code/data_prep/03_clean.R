# Title: Clean the ACS data and build the analysis variables (step 3 of the data preparation)
# Author: Shadi Seyedi
# Description: Turns the merged file from 02_merge.R into the analysis file. Every coded ACS
#              variable is converted to a number first, then recoded so that it means the same in
#              every year (see "KNOWN CODING FEATURES" in the header of 01_get_data.R). It adds the
#              derived variables the analysis uses and flags for the sensitivity checks.
#              This script drops NO person. The sample restrictions are applied later by
#              make_sample() in code/00_setup.R.
#
# Variables built here (one block each below):
#   Outcomes       HINS1-HINS7, HICOV: 1 yes, 0 no.  UNINS = 1 - HICOV.
#   Demographics   SEX (factor Male/Female) and FEMALE (0/1), AGEG (ordered), MARG, DIS, FER,
#                  ETHN (Hispanic), RACE and RACE1 (factors), SCHLG (ordered), ESRG (factor),
#                  POVPIPG (ordered)
#   Immigration    ForeginBorn, NATIVITY, NonCit, CITSTAT, FBCIT, FBMEX, FBOTHR, cit_birth_abroad,
#                  ENG (ordered), YLIU, PLIU, LTINU, recent_arrival, CORIGIN (region of birth)
#   Legal status   REL_HH (relationship to the householder, one coding for all years),
#                  UNDOC (likely undocumented, main version), UNDOC_BEN (sensitivity version that
#                  also uses benefit receipt), FBLEG
#   Treatment      ttot (years since expansion, for event studies), year_treated (for the Sun
#                  and Abraham estimator), ACA, PostACA.
#                  expansion, ExpansionY, Expan_STS, expansion_date, treat and treat_frac come
#                  from 02_merge.R unchanged. The analysis uses ExpansionY for timing;
#                  expansion_date is the exact date, kept for reference only. The state lists for sample rules (states with
#                  full coverage before 2014, early-option states, late and mid-year expanders)
#                  are written in make_sample() with their sources; they need no column here.
#   Flags          in_gq (group quarters), hins_imputed (Medicaid answer imputed), medicaid_edit
#                  (Medicaid assigned by the Census coverage edit). ipc_imputed comes from 02.
#
# Inputs:  data/clean/acs_merged.rds (from 02_merge.R)
# Outputs: data/clean/acs_clean.rds   one row per person, all input columns + the variables above
# Usage:   from the project root,  Rscript code/data_prep/03_clean.R


####################
## Setup
####################

suppressPackageStartupMessages(library(data.table))

# All paths are relative to the project root, so the script must be run from there.
if (!file.exists("run_all.R")) stop("Run this script from the project root folder.")

in_file  <- "data/clean/acs_merged.rds"
out_file <- "data/clean/acs_clean.rds"

# Saves an object without ever leaving a half-written file behind (as in 01_get_data.R).
save_safely <- function(object, path) {
        tmp <- paste0(path, ".tmp")
        saveRDS(object, tmp)
        if (!file.rename(tmp, path)) stop("Could not rename ", tmp, " to ", path)
}


####################
## Functions
####################

# Relationship to the householder, one coding for every year: "reference", "spouse" or "other".
# The Census renamed and recoded this variable in 2019 (REL_RAW holds the original codes):
#   RELP     (2011-2018): 00 reference person, 01 husband/wife
#   RELSHIPP (2019):      20 reference person, 21 opposite-sex spouse, 23 same-sex spouse
harmonize_relationship <- function(rel, year) {
        rel <- as.integer(rel)
        fifelse(rep_len(year, length(rel)) < 2019,
                fcase(rel == 0, "reference", rel == 1, "spouse", default = "other"),
                fcase(rel == 20, "reference", rel %in% c(21, 23), "spouse", default = "other"))
}

# Lived more than 25% of their life in the US (Endeshaw et al. 2018). 0 for the US-born.
code_ltinu <- function(pliu, foreign_born) {
        as.integer(foreign_born == 1 & !is.na(pliu) & pliu > 25)
}

## Likely undocumented immigrants (the "residual" method)
# The ACS does not ask about legal status. Following Borjas and Cassidy (2019, Labour Economics
# 61:101757), which applies Passel and Cohn's (Pew) method to the ACS, a non-citizen is counted as
# legal if ANY of the following holds, and as likely undocumented otherwise:
#   (a) arrived before 1980                      YOEP <= 1979
#   (b) born in Cuba (nearly all Cuban immigrants had refugee status)   POBP 327
#   (c) is a veteran or in the armed forces      active duty now or in the past (see MIL below),
#                                                or ESR 4-5 (armed forces)
#   (d) works for the government                 COW 3 local, 4 state, 5 federal
#   (e) works in a licensed occupation           OCCP lists below
#   (f) is married to a legal immigrant or a citizen (the spouse meets (a)-(e) or is a citizen)
# The sensitivity version (benefits = TRUE) adds Borjas and Cassidy's benefit rule: receives
# Social Security (SSP > 0) or SSI (SSIP > 0), or is covered by Medicare (HINS3), TRICARE
# (HINS5) or VA (HINS6). Medicaid (HINS4), which is in their list, is left out of both versions:
# it is our outcome, and using it would set Medicaid to 0 for everyone flagged undocumented.
# Public housing, also in their list, is not in the ACS.
#
# MIL changed codes in 2013 (PUMS data dictionaries):
#   2011-2012: 1 active duty now, 2 active duty in the past 12 months, 3 active duty earlier,
#              4 training only (Reserves/National Guard), 5 never served
#   2013-2019: 1 active duty now, 2 active duty in the past, 3 training only, 4 never served
# Training only does not count, as in Borjas and Cassidy ("a veteran, or currently in the Armed
# Forces") and in the IPUMS veteran status variable.
#
# Licensed occupations: Borjas and Cassidy give examples (physicians, registered nurses, air
# traffic controllers, lawyers) but did not publish a list; Pew adds police and other law
# enforcement. The list covers lawyers and judges, licensed health professionals, police,
# fire and correctional officers, transportation security screeners, pilots and air traffic
# controllers. PUMS 2011-2017 use the 2010 Census occupation codes, 2018-2019 the 2018 codes;
# the 2018 list replaces physicians and surgeons 3060 by 3090 and 3100, EMTs and paramedics 3400
# by 3401 and 3402, bailiffs and correctional officers 3800 by 3801 and 3802, and police officers
# 3850 by 3870 (Census 2018 occupation code crosswalk).
licensed_occ_2010 <- c(2100, 2105,                                     # lawyers, judges, clerks
                       3000, 3010, 3030, 3040, 3050, 3060, 3110, 3120, # health professionals
                       3140, 3150, 3160, 3200, 3220, 3230, 3250, 3255, 3256, 3258, 3310,
                       3400, 3500,
                       3700, 3710, 3720, 3740, 3750, 3800, 3820, 3850, 3945,   # protective service
                       9030, 9040)                                     # pilots, air traffic
licensed_occ_2018 <- sort(c(setdiff(licensed_occ_2010, c(3060, 3400, 3800, 3850)),
                            3090, 3100, 3401, 3402, 3801, 3802, 3870))

# TRUE for a person who meets rules (a)-(e) above or is a citizen (CIT 1-4).
legal_by_own_traits <- function(d, benefits = FALSE) {
        military <- fifelse(d$YEAR < 2013, d$MIL %in% 1:3, d$MIL %in% 1:2) | d$ESR %in% 4:5
        licensed <- fifelse(d$YEAR < 2018, d$OCCP %in% licensed_occ_2010,
                            d$OCCP %in% licensed_occ_2018)
        legal <- d$CIT %in% 1:4 | d$YOEP %in% 1900:1979 | d$POBP %in% 327 | military |
                 d$COW %in% 3:5 | licensed
        if (benefits) {
                legal <- legal | (!is.na(d$SSP) & d$SSP > 0) | (!is.na(d$SSIP) & d$SSIP > 0) |
                         d$HINS3 %in% 1 | d$HINS5 %in% 1 | d$HINS6 %in% 1
        }
        legal
}

# 1 = likely undocumented, 0 = not. Needs YEAR, SERIALNO, REL_HH, CIT, YOEP, POBP, MIL, ESR, COW,
# OCCP (and SSP, SSIP, HINS3, HINS5, HINS6 when benefits = TRUE; HINS coded 1 yes, 0 no).
# Rule (f) pairs the reference person with their spouse. PUMS has no spouse pointer, so other
# married couples in the household (e.g. a married adult child) cannot be paired; the rule is
# applied once (a spouse who is legal only through rule (f) does not pass it on).
flag_undoc <- function(d, benefits = FALSE) {
        x <- data.table(hh = paste(d$YEAR, d$SERIALNO), rel = d$REL_HH,
                        legal = legal_by_own_traits(d, benefits))
        couples <- x[rel %in% c("reference", "spouse"),
                     .(n_ref = sum(rel == "reference"), n_spouse = sum(rel == "spouse"),
                       ref_legal = any(legal & rel == "reference"),
                       spouse_legal = any(legal & rel == "spouse")),
                     by = hh]
        x <- couples[x, on = "hh"]            # keeps the order of d
        partner_legal <- fcase(x$rel == "reference" & x$n_spouse == 1, x$spouse_legal,
                               x$rel == "spouse"    & x$n_ref    == 1, x$ref_legal,
                               default = FALSE)
        as.integer(d$CIT == 5 & !x$legal & !partner_legal)
}

## Checks of the functions on made-up people (the script stops here if a rule is wrong)

# A made-up household: one undocumented-looking non-citizen per row unless changed
fake <- function(...) {
        d <- data.table(YEAR = 2015L, SERIALNO = "A", REL_HH = "reference", MAR = 5L, CIT = 5L,
                        YOEP = 2005L, POBP = 303L, ESR = 1L, MIL = 4L, COW = 1L, OCCP = 4720L,
                        SSP = 0L, SSIP = 0L, HINS3 = 0L, HINS4 = 0L, HINS5 = 0L, HINS6 = 0L)
        args <- list(...)
        n <- max(lengths(args), 1)
        d <- d[rep(1, n)]
        for (v in names(args)) d[, (v) := args[[v]]]
        d
}

# Relationship codes mean the same in every year
stopifnot(identical(harmonize_relationship(c("00", "01", "05"), 2015), c("reference", "spouse", "other")),
          identical(harmonize_relationship(c("20", "21", "23", "22", "25"), 2019),
                    c("reference", "spouse", "spouse", "other", "other")))

# Each rule on its own; the first row breaks no rule, citizens are never flagged
stopifnot(identical(flag_undoc(fake(SERIALNO = letters[1:7],
                                    CIT = c(5L, 4L, 5L, 5L, 5L, 5L, 5L),
                                    YOEP = c(2005L, 2005L, 1979L, 2005L, 2005L, 2005L, 2005L),
                                    POBP = c(303L, 303L, 303L, 327L, 303L, 303L, 303L),
                                    COW = c(1L, 1L, 1L, 1L, 4L, 1L, 1L),
                                    ESR = c(1L, 1L, 1L, 1L, 1L, 4L, 1L),
                                    OCCP = c(4720L, 4720L, 4720L, 4720L, 4720L, 4720L, 3255L))),
                    c(1L, 0L, 0L, 0L, 0L, 0L, 0L)))

# Military service means the same before and after the 2013 code change: active duty counts,
# training only does not
stopifnot(identical(flag_undoc(fake(SERIALNO = c("A", "B", "C", "D"), YEAR = c(2012L, 2012L, 2013L, 2013L),
                                    MIL = c(3L, 4L, 2L, 3L))),
                    c(0L, 1L, 0L, 1L)))

# Occupation codes are read with the code list of their year
stopifnot(identical(flag_undoc(fake(SERIALNO = c("A", "B", "C"), YEAR = c(2017L, 2018L, 2018L),
                                    OCCP = c(3060L, 3060L, 3090L))),
                    c(0L, 1L, 0L)))

# Benefits count only in the sensitivity version, and Medicaid never counts
d <- fake(SERIALNO = c("A", "B", "C"), SSP = c(5000L, 0L, 0L), HINS3 = c(0L, 1L, 0L),
          HINS4 = c(0L, 0L, 1L))
stopifnot(identical(flag_undoc(d), c(1L, 1L, 1L)),
          identical(flag_undoc(d, benefits = TRUE), c(0L, 0L, 1L)))

# A non-citizen married to a citizen (2015 codes) or to a legal non-citizen (2019 codes) is not
# flagged
stopifnot(identical(flag_undoc(fake(REL_HH = c("reference", "spouse"), MAR = 1L, CIT = c(5L, 4L))),
                    c(0L, 0L)),
          identical(flag_undoc(fake(YEAR = 2019L, REL_HH = c("reference", "spouse"), MAR = 1L,
                                    YOEP = c(2005L, 1975L))),
                    c(0L, 0L)))

# The spouse rule uses only the person's own spouse: a legal adult child does not make the
# couple legal (the old code averaged over the whole household)
stopifnot(identical(flag_undoc(fake(REL_HH = c("reference", "spouse", "other"), MAR = 1L,
                                    CIT = c(5L, 5L, 4L))),
                    c(1L, 1L, 0L)))

# LTINU is 0/1 and 0 for the US-born
stopifnot(identical(code_ltinu(c(10, 40, NA, 40), c(1, 1, 0, 0)), c(0L, 1L, 0L, 0L)))
rm(d)


####################
## Read and convert
####################

cat("Reading", in_file, "\n")
Data <- as.data.table(readRDS(in_file))
n_people <- nrow(Data)

# Every value was saved as text exactly as in the Census files, so "01" == 1 is FALSE and "086" >
# "100" is TRUE. All coded variables become integers here, before any recode. SERIALNO, ST and
# PUMA stay text (they are identifiers; ST is matched as text in 02), and so do SOCP (contains
# letters) and REL_RAW (recoded by harmonize_relationship()).
keep_text <- c("SERIALNO", "ST", "StateN", "PUMA", "SOCP", "REL_RAW", "Expan_STS")
to_int <- setdiff(names(Data)[vapply(Data, is.character, logical(1))], keep_text)
na_before <- vapply(Data[, ..to_int], function(x) sum(is.na(x)), numeric(1))
Data[, (to_int) := lapply(.SD, as.integer), .SDcols = to_int]
na_after <- vapply(Data[, ..to_int], function(x) sum(is.na(x)), numeric(1))
# The conversion turned no value into NA (every code is a whole number)
stopifnot(identical(na_before, na_after))


####################
## Outcomes: health insurance
####################

# HINS1 employer/union, HINS2 bought directly, HINS3 Medicare, HINS4 Medicaid or other
# means-tested coverage, HINS5 TRICARE, HINS6 VA, HINS7 Indian Health Service, HICOV any coverage.
# ACS codes: 1 yes, 2 no. Recoded to 1 yes, 0 no.
hins <- c(paste0("HINS", 1:7), "HICOV")
stopifnot(Data[, all(unlist(lapply(.SD, function(x) x %in% 1:2))), .SDcols = hins])
Data[, (hins) := lapply(.SD, function(x) as.integer(x == 1)), .SDcols = hins]
Data[, UNINS := 1L - HICOV]


####################
## Demographics
####################

## Sex: ACS 1 male, 2 female
Data[, SEX := factor(SEX, levels = 1:2, labels = c("Male", "Female"))]
Data[, FEMALE := as.integer(SEX == "Female")]

## Age groups (ages 18+ in the data)
Data[, AGEG := cut(AGEP, breaks = c(17, 25, 34, 44, 54, 64, Inf),
                   labels = c("18-25", "26-34", "35-44", "45-54", "55-64", "65+"),
                   ordered_result = TRUE)]

## Married: ACS MAR 1 married, 2 widowed, 3 divorced, 4 separated, 5 never married
Data[, MARG := as.integer(MAR == 1)]

## Disability: ACS 1 with a disability, 2 without
Data[, DIS := as.integer(DIS == 1)]

## Gave birth in the past 12 months: ACS 1 yes, 2 no, NA not asked (men, women outside 15-50).
## Code 8 ("suppressed") appears in 2012 only and is treated as missing.
Data[, FER := fcase(FER == 1, 1L, FER == 2, 0L)]

## Hispanic: ACS HISP 01 not Hispanic, 02-24 Hispanic origin groups
Data[, ETHN := as.integer(HISP != 1)]

## Race: ACS RAC1P 1 White, 2 Black, 3-5 American Indian / Alaska Native, 6 Asian,
## 7 Native Hawaiian / Pacific Islander, 8 some other race, 9 two or more races
Data[, RACE := factor(fcase(RAC1P == 1, "White", RAC1P == 2, "Black", RAC1P == 6, "Asian",
                            default = "Other"),
                      levels = c("White", "Black", "Asian", "Other"))]
## Race and ethnicity: Hispanic of any race, then non-Hispanic White, Black, Asian, Other
Data[, RACE1 := factor(fifelse(ETHN == 1, "Hispanic", as.character(RACE)),
                       levels = c("White", "Black", "Asian", "Hispanic", "Other"))]

## Education: ACS SCHL 01-15 less than high school, 16 high school diploma, 17 GED,
## 18-19 some college, 20 associate's, 21 bachelor's, 22 master's, 23 professional, 24 doctorate
Data[, SCHLG := cut(SCHL, breaks = c(0, 15, 17, 20, 21, 24),
                    labels = c("Less than high school", "High school",
                               "Some college or Associate degree", "College degree",
                               "Graduate and beyond"),
                    ordered_result = TRUE)]

## Employment: ACS ESR 1-2 civilian employed (at work / with a job but not at work),
## 4-5 armed forces, 3 unemployed, 6 not in the labor force
Data[, ESRG := factor(fcase(ESR %in% c(1, 2, 4, 5), "Employed", ESR == 3, "Unemployed",
                            ESR == 6, "Not in labor force"),
                      levels = c("Employed", "Unemployed", "Not in labor force"))]

## Income-to-poverty ratio groups (POVPIP is 0-400 in the data, see 01_get_data.R)
Data[, POVPIPG := cut(POVPIP, breaks = c(-1, 100, 138, 200, 400),
                      labels = c("0-100%", "101-138%", "139-200%", "201-400%"),
                      ordered_result = TRUE)]


####################
## Immigration
####################

## Citizenship: ACS CIT 1 born in the US, 2 born in Puerto Rico or the US island areas,
## 3 born abroad to American parents, 4 naturalized citizen, 5 not a citizen.
## Foreign-born means not a citizen at birth (CIT 4-5), as the Census Bureau defines it.
## People born in the territories or abroad to American parents (CIT 2-3) are flagged: the main
## sample drops them, and a sensitivity check counts them as US-born.
Data[, ForeginBorn := as.integer(CIT %in% 4:5)]
Data[, NATIVITY := factor(ForeginBorn, levels = 0:1, labels = c("US-born", "Foregin-born"))]
Data[, cit_birth_abroad := as.integer(CIT %in% 2:3)]
Data[, NonCit := as.integer(CIT == 5)]
Data[, FBCIT := as.integer(CIT == 4)]
Data[, CITSTAT := factor(fcase(CIT %in% 1:3, "US-Born", CIT == 4, "FB-Citizen",
                               CIT == 5, "FB-Non-citizen"),
                         levels = c("US-Born", "FB-Citizen", "FB-Non-citizen"))]

## English: ACS ENG 1 very well, 2 well, 3 not well, 4 not at all; NA (not applicable) means the
## person speaks only English at home (it is NA exactly when LANX == 2, in every year)
stopifnot(Data[, all(is.na(ENG) == (LANX == 2))])
Data[, ENG := factor(fifelse(is.na(ENG), 0L, ENG), levels = 0:4,
                     labels = c("Only english", "Very well", "Well", "Not well", "Not at all"),
                     ordered = TRUE)]

## Years and share of life in the US. YOEP (year of entry) is NA exactly for people born in the
## US (CIT 1). 307 people report an entry year before their birth year (age is top-coded and
## rounded), so years in the US are capped at age.
stopifnot(Data[, all(is.na(YOEP) == (CIT == 1))])
Data[, YLIU := pmin(YEAR - YOEP, AGEP)]
Data[, PLIU := floor(YLIU / AGEP * 100)]
Data[, LTINU := code_ltinu(PLIU, ForeginBorn)]
# In the US less than 5 years (the 5-year bar on Medicaid for most lawfully present immigrants)
Data[, recent_arrival := as.integer(ForeginBorn == 1 & YLIU < 5)]

## Born in Mexico or elsewhere (Figure 10)
Data[, FBMEX  := as.integer(ForeginBorn == 1 & POBP == 303)]
Data[, FBOTHR := as.integer(ForeginBorn == 1 & POBP != 303)]

## Region of birth (CORIGIN), for the foreign-born; "United States" for everyone else.
## Groups follow Pew Research Center's region-of-birth tables (Statistical Portrait of the
## Foreign-Born Population), with Mexico separate from the rest of Latin America, and the
## countries placed by the Census Bureau's region of birth (ACS table B05006). Europe, Canada and
## Oceania are combined because each is small in a low-income sample.
## POBP codes from the PUMS data dictionaries 2011-2019. Codes that exist only in some years:
## 155 Estonia and 251 Eastern Asia (2011), 208 Cyprus (2012-2016), 167 Kosovo, 219 Kyrgyzstan,
## 228 Mongolia (2017+), 464 Northern Africa n.e.c. (2017+).
pob_mena <- c(158, 159, 161,                       # Armenia, Azerbaijan, Georgia (Western Asia)
              208, 212, 213, 214, 216, 222, 224, 235, 239, 243, 245, 248,   # Cyprus, Iran ... Yemen
              400, 414, 430, 436, 451, 456, 464)  # Algeria, Egypt, Libya, Morocco, Sudan, Tunisia,
                                                  # Northern Africa n.e.c.
pob_east_asia  <- c(205, 206, 207, 209, 211, 215, 217, 223, 226, 228, 233, 236, 240, 242, 247,
                    249, 251, 254)                # Myanmar ... Vietnam; 249/254 Asia n.e.c.
pob_south_asia <- c(200, 202, 203, 210, 218, 219, 229, 231, 238, 246, 253)   # Afghanistan ...
Data[, CORIGIN := fcase(
        ForeginBorn == 0,                             "United States",
        POBP == 303,                                  "Mexico",
        POBP %between% c(310, 316),                   "Central America",
        POBP %between% c(321, 344),                   "Caribbean",
        POBP %between% c(360, 399),                   "South America",   # 399 Americas n.e.c.
        POBP %in% pob_east_asia,                      "East and Southeast Asia",
        POBP %in% pob_south_asia,                     "South and Central Asia",
        POBP %in% pob_mena,                           "Middle East and North Africa",
        POBP %between% c(400, 469),                   "Sub-Saharan Africa",
        POBP %between% c(100, 169) | POBP %in% c(60, 300, 301) | POBP %between% c(500, 554),
                                                      "Europe, Canada and Oceania")]
Data[, CORIGIN := factor(CORIGIN, levels = c("United States", "Mexico", "Central America",
                                             "Caribbean", "South America",
                                             "East and Southeast Asia", "South and Central Asia",
                                             "Middle East and North Africa", "Sub-Saharan Africa",
                                             "Europe, Canada and Oceania"))]
# Every foreign-born person has a region
stopifnot(!anyNA(Data$CORIGIN))


####################
## Legal status
####################

Data[, REL_HH := harmonize_relationship(REL_RAW, YEAR)]

Data[, UNDOC     := flag_undoc(Data)]
Data[, UNDOC_BEN := flag_undoc(Data, benefits = TRUE)]
# Foreign-born and not likely undocumented (Figure 2)
Data[, FBLEG := as.integer(ForeginBorn == 1 & UNDOC == 0)]

# Share of non-citizens flagged, by year. Pew's estimates put unauthorized immigrants at roughly
# 44% of non-citizens (2016); a residual flag is expected to run higher (Pew's own first-stage
# pool is 20-35% larger than its final estimate), and this file covers only ages 18+ with incomes
# up to 400% of poverty.
cat("\nShare of non-citizens flagged as likely undocumented (weighted):\n")
print(Data[CIT == 5, .(UNDOC = round(weighted.mean(UNDOC, PWGTP), 3),
                       UNDOC_BEN = round(weighted.mean(UNDOC_BEN, PWGTP), 3)), by = YEAR])


####################
## Treatment timing
####################

# Years since the state expanded (0 for states that never expanded; event-study models interact
# it with expansion, so its value there does not matter)
Data[, ttot := fifelse(is.na(ExpansionY), 0L, YEAR - ExpansionY)]
# Expansion year for the Sun and Abraham (2021) estimator: states that never expanded get a year
# far outside the study period, as fixest::sunab() requires for never-treated units
Data[, year_treated := fifelse(is.na(ExpansionY), 10000L, ExpansionY)]
Data[, ACA := factor(fifelse(YEAR < 2014, "Pre-ACA", "Post-ACA"), levels = c("Pre-ACA", "Post-ACA"))]
Data[, PostACA := as.integer(YEAR >= 2014)]


####################
## Flags for sensitivity checks
####################

# Group quarters: TYPE 1 housing unit, 3 noninstitutional group quarters (institutional group
# quarters, TYPE 2, have no POVPIP and were not downloaded)
Data[, in_gq := as.integer(TYPE != 1)]
# Medicaid answer imputed by the Census Bureau (FHINS4P allocation flag)
Data[, hins_imputed := as.integer(FHINS4P == 1)]
# Medicaid assigned by the Census coverage edit (e.g. for people receiving SSI); NA in the file
# for people without Medicaid
Data[, medicaid_edit := as.integer(!is.na(FHINS4C) & FHINS4C == 1)]


####################
## Checks and save
####################

# No person dropped, and the 0/1 variables are 0 or 1
stopifnot(nrow(Data) == n_people,
          Data[, all(UNINS %in% 0:1), ], Data[, all(LTINU %in% 0:1)],
          Data[, all(UNDOC %in% 0:1)], Data[, all(UNDOC_BEN <= UNDOC)])

# Treatment is 0 in every state that never expanded, and years since expansion match treat
stopifnot(Data[expansion == 0, all(treat == 0)],
          Data[expansion == 1, all((ttot >= 0) == (treat == 1))])

# No missing values in the model variables among people the main sample would keep (ages 26-64,
# income at or below 138% of poverty, not in group quarters, not born in the territories or
# abroad to American parents; DC is dropped by the main sample and has no ADA score)
model_vars <- c("HICOV", "UNINS", "HINS1", "HINS2", "HINS3", "HINS4", "SEX", "DIS", "AGEP", "POVPIPG", "SCHLG",
                "MARG", "RACE1", "ESRG", "ENG", "LTINU", "CORIGIN", "NonCit", "UNDOC",
                "IPC", "UnempR", "ADA", "treat", "ttot", "PWGTP")
core <- Data[AGEP %between% c(26, 64) & POVPIP <= 138 & in_gq == 0 & cit_birth_abroad == 0 &
             ST != "11"]
stopifnot(!anyNA(core[, ..model_vars]))
rm(core)

cat("Saving", format(nrow(Data), big.mark = ","), "people to", out_file, "\n")
save_safely(as.data.frame(Data), out_file)
