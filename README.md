# Medicaid expansion effects on immigrants insurance gain
 
This repository contains all necessary resources for the research project on the effect of Medicaid expansion on foreign-born vs US-born insurance coverage. 
 
## Table of content

- [Data](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants#data) 
   - [Raw data](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#raw-data)
   - [Cleaned data ](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#cleaned-data)
- [Codes](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#codes)  
- [Text](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#text)  
- [Figures and tables](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#figures-and-tables)  

## Data

This project uses ACS 1-year PUMS (2011–2019) at the person level, plus three state-year contextual datasets (BLS unemployment, Medicaid expansion status/timing, IPC Index). We store data in two tiers:

* data/raw/ — Immutable source files (exactly as downloaded; no edits).

* data/processed/ — Derived, cleaned, and analysis-ready datasets (subsets, recodes, merges).

> Large files are not committed to GitHub. Regenerate them via scripts or track them with Git LFS if needed.



### Raw data

#### 1. *American Community Survey(ACS) Public Use Microdata Sample (PUMS)* Core dataset

I retrieve person-level ACS 1-year PUMS files for 2011–2019 via the API using the R package [tidcycensus](https://walker-data.com/tidycensus/index.html) and append them in one single file called RAW.CSV
For the full data access use [website](https://www.census.gov/programs-surveys/acs/microdata/access.html) along with detailed [documentation](https://www.census.gov/programs-surveys/acs/microdata/documentation.html).

Each year’s person-level PUMS file includes:
* ST: State
* TYPE: Type of unit show if individual belong to group quarters or not ( This is a temporary variable for the purpose of subseting) 
* REGION: Region code based on 2010 Census definitions ( Northeast, Midwest, South, West, Puerto Rico ).
* AGEP: Age a number between 0-99 (top coded).
* CIT: Citizenship status, if borned in US, naurlized citizen or not a US citizen.
* ENG: Ability to speak English ( Well, not well, not at all, speeks only english)
* FER: Gave birth to child within the past 12 months( N/a if male, Yes, No)
* HINS1: Insurance through a current or former employer or union
* HINS2: Insurance purchased directly from an insurance company
* HINS3: Medicare, for people 65 and older, or people with certain disabilities
* HINS4: Medicaid, Medical Assistance, or any kind of government-assistance plan for those with low incomes or a disability
* HINS5: TRICARE or other military health care
* HINS6: VA (enrolled for VA health care)
* HINS7: Indian Health Service
* MAR: Marital status( Married, widowed, divorced, Separated,Never Married)
* SCHL: Educational attainment ( from no education to doctrate- 24 values)
* SEX: Male or Female
* YOEP: Year of entry to US( NA for not eligible,  value year for the rest)
* DIS: Disability recode( with disability or without disability)
* HICOV: Health insurance coverage recode ( with health insurance, no health insurance)
* HISP: Recoded detailed Hispanic origin( Not spanish 01, other hispanic origen coded 02-24)
* ESR: Employment status recode ( N/A, employed, unemployed, armed force, not in labor force)
* NATIVITY: Nativity (Native, Foreign born)
* POVPIP: Income-to-poverty ratio recode
* RAC1P: Recoded detailed race code
* POBP: (Place of birth-- state or country) 
* LANP:(Language spoken at home--N/A if english, name of other lang coded)
* LANX:Language other than English spoken at home? ( Yes/No)
* PUMA: Public use microdata area code (PUMA) based on 2010 Census definition
(areas with population of 100,000 or more, use with ST for unique code)
* POBP: Place of Birth
* RELP: 


>In 2019 ACS introduced a new variable HIMRKS which provides estimates of the number and proportion of people with subsidized Marketplace coverage. However, since this variable in not there for the rest of the years between 2015-2019 I didn't include it for our analysis, but it would be good for future analysis, Other variable that can be used for the future study are ANC1P (Recoded Detailed Ancestry - first entry) , NOP (Nativity of parent for individual less than 17-- both parent native, mother FB, father FB, both FB)

#### 2. *BLS State Unemployment*

* LS State Unemployment (LAUS)

* Source: [Bureau of Labor Statistics (LAUS)](https://www.bls.gov/lau/)

* Description: Annual state-level unemployment rates (or monthly averaged to annual).

* Key Variables: state_fips, state, year, unemp_rate

* Usage: Merged with ACS by state-year to control for economic context.


KFF, state's health fact for the dynamics status of medicaid expansion


#### 3. _Medicaid Expansion Status & Timing_

 * Source: [KFF Medicaid Expansion Tracker](https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions/)

 * Description: State decisions and effective dates for Medicaid expansion under the ACA.

 * Key Variables: state_fips, state, expansion_effective_date, year, expanded_in_year

 * Usage: Used to create the main policy variable (expanded_in_year) in DiD models.

#### 4. _The Immigration Policy Climate (IPC) Index_

 * Source: [IPC Index Dataset](https://www.goleensamari.com/data)

 * Description: A longitudinal measure of state immigration policy climate (50 states + DC, 2009–2023).

 * Key Variables: state_fips, state, year, ipc_score

 * Usage: Captures state-level xenophobia/policy restrictiveness to account for structural context.

### Cleaned data 

The raw data were merged and cleaned to create analysis-ready datasets.
#### PREACS (Intermedicate File)
Filtered according to inclusion criteria:
    1. *Income below 138%* which I already applied it when I was downloading the data.
    2. *Age being between 26 to 64*
    3. *Removing the institutionlized and unistitulionalized group quarters population* this is because there is no reported income for these groups.
    4. *Droping the non citizen with less than 5 years of residency* since this group are not eligible for mediacid benefit at all. 
    5. *Removing the data for the states that have adopted policies similar to Medicaid expansion before the Medicaid expansion went into effect in      2014* these states are Delaware, Massachusetts, New York, Vermont and  District of Columbia. 

#### CLNACS (Final File)
Further cleaned dataset including created and recoded variables ready to be used for the analysis


Codes
:

* **GetDataTideyCensus.R:** This script includes codes for getting data set with only required variable from the ACS'API, appending annual data's in one dataset, chaning the format of dataset to a data frame and class of variables from charachter to numeric ,and exporting the data to CSV file Called RAWACS. 
I filtered out the  low-income ( less than 138% income/poverty) and individuals aged between 18-65 at the source for spead. I then assigned a variable YEAR to each of the datasets, merged all these annual datasets into one single dataset by year and saved it as RAWACS, it includes 3,578,567 observation and 109 variable which 82 of them are weights. The raw data can be find here and the code [here](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/blob/master/Codes/R/00-GetDataTidcyCensus.R).


* **Binding amd Subesting.R:**
    Mergeded raw dataset and created a subset of the data using my inclusion criteria as below.
* **Cleaning Data-03.R**
   Cleaning Data-03.R: Some variables were recoded to binary variables with 0 and 1 values instead of 1 and 2. The variables that were recoded include insurance, marriage status, disability, sex, and Hispanic. Additionally, new variables were created by recoding other variables such as age, race, education, employment, and poverty rate.
   Coding for variables ENG and FER had problem I fixed so that the values be consistent for all years.  From AGEP I created a categorical variable called AGEG to set up diffrent age into diffrent categories. Because the variable for the schooling had many categories created a new variable SCHLG, to group the schooling year into the conventional categories. I have done a similar recoding for the employmentant,race and poverty rate  to create more manageable and meaningful variables for analysis. 

   Additonaly, I also dealt with observed problem of YOEP (Year of entry) in the dataset. For people borned in US the downloaded value were bottom codeded and were diffrent for each year,I fixed this error by changing the value of YOEP for people borned in US to NA since there were mistakenly bottom coded for each year. Final

   In the next section of this script, I created additional variables that would be necessary for my analysis. These included two regional variables for categorizations for the country of origin, a variable for the percentage of life spent in the US, and a variable for expansion.
   
Following this section, I exported data for the IPC index, political climate, and state's unemployment rate, and then appended these data to the original data set.

In the final section, I removed variables that I deemed unnecessary for my analysis. This included variables that were only used to create other variables that were necessary for my analysis. Finally, I exported the cleaned data in both CSV and DAT formats for use in the main analysis.

* fixed the mismatch, changed the vacant values with NA a this would be the 


