# Medicaid expansion effects on immigrants insurance gain
 
This repository contains information about my research project on the effect of Medicaid expansion on immigrant insurance coverage. I aim to reach two   goals by creating this repository:

1. To make my research reproducible in such a way that other researchers could reuse my codes and data.
2. To make my life easier by documenting every step I take ( Remembering what I have done).

For this purpos I divided this repo into 4 chunks, data, text, figures and tables, and codes. I will provide more details for each section.
 
## Table of content

- [Data](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants#data) 
   - [Raw data](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#raw-data)
   - [Cleaned data ](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#cleaned-data)
- [Codes](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#codes)  
- [Text](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#text)  
- [Figures and tables](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#figures-and-tables)  

## Data

I will discuss where I gathered the data for this analysis and what the data folder consists of.

### Raw data

The raw data for this analysis can be derived from the American Community Survey(ACS) Public Use Microdata Sample (PUMS) for the years 2009-2019, and it is available on their [website](https://www.census.gov/programs-surveys/acs/microdata/access.html) along with detailed [documentation](https://www.census.gov/programs-surveys/acs/microdata/documentation.html). This annual raw data can be downloaded in either SAS or CSV format from their File Transfer Protocol (FTP) site. These data are also available via the web API( Application Programming Interface), and the R package [tidcycensus](https://walker-data.com/tidycensus/index.html) can be used to download and handle this dataset more efficiently by using mentioned APIs. 

Using API and tidycensus R package, I downloaded each year's dataset with only variables I needed; these included variables are as follows:

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

>In 2019 ACS introduced a new variable HIMRKS which provides estimates of the number and proportion of people with subsidized Marketplace coverage. However, since this variable in not there for the rest of the years between 2015-2019 I didn't include it for our analysis, but it would be good for future analysis, Other variable that can be used for the future study are POBP (Place of birth-- state or country) ANC1P (Recoded Detailed Ancestry - first entry) LANP(Language spoken at home--N/A if english, name of other lang coded), NOP (Nativity of parent for individual less than 17-- both parent native, mother FB, father FB, both FB)

For my RAM to handle the operation, I only downloaded data for low-income ( less than 138% income/poverty) individuals aged between 18-65. I then assigned a variable YEAR to each of the datasets, merged all these annual datasets into one single dataset by year and saved it as RAWACS, it includes 3,578,567 observation and 109 variable which 82 of them are weights. The raw data can be find here and the code [here](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/blob/master/Codes/R/00-GetDataTidcyCensus.R).

### Cleaned data 

In the next step, using my inclusion criteria I created a subset of data, fixed the mismatch, changed the vacant values with NA a this would be the cleaneddata1. But, you will also find cleaneddata2 in the folder that would be the final data set whihch includes my created variable as well and exported it to Stata format for future use.

I will use both R and Stata for running analyses. For now, I only used R for cleaning and preparing the data set for the main analysis in Stata.


## Codes

### R codes

GetDataTideyCensus: This script includes codes for getting data set with only required variable from the ACS'API, appending annual data's in one dataset, chaning the format of dataset to a data frame and class of variables from charachter to numeric ,and exporting the data to CSV file Called RAWACS. Additonaly, I also dealt with observed problem of YOEP (Year of entry) in the dataset. For people borned in US the downloaded value were bottom codeded and were diffrent for each year,I fixed this error by changing the value of YOEP for people borned in US to NA since there were mistakenly bottom coded for each year. Final 

* Getting Data (Done) 
* Subsesting Based on Inclusion (Mnday)
* Cleaning the Variable (Tuesday) 
* Creating new variables and merge (Wednsday,Thursday) 
* Export final Data set(Wednsday)
* Whole data cleaning codes 
* Run the SCM to find what variable to control (Friday, Monday)

### Stata codes



## Text
## Figures and tables
 
