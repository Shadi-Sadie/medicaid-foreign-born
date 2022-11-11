# Medicaid expansion effects on immigrants insurance gain
 
This repository contains information about my research project on the effect of Medicaid expansion on immigrant insurance coverage. I aim to reach two   goals by creating this repository:

1. To make my research reproducible in such a way that other researchers could reuse my codes and data.
2. To make my life easier by documenting every step I take and being organized.

For this purpos I divided this repo into 4 chunks, data, text, presentation, and codes. I will provide more details for each section.
 
## Table of content

- [Data](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants#data) 
   - [Raw Data](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#raw-data)
- [Codes](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#codes)  
- [Text](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#text)  
- [Presentation](https://github.com/Shadi-Sadie/Paper-1-Cancer-Screening-and-Immigrants/#presentation)  

## Data

I will discuss where I gathered the data for this analysis and what the data folder consists of.

### Raw Data

The raw data for this analysis can be derived from the American Community Survey(ACS) Public Use Microdata Sample (PUMS) for the years 2009-2020, and it is available on their [website](https://www.census.gov/programs-surveys/acs/microdata/access.html) along with detailed [documentation](https://www.census.gov/programs-surveys/acs/microdata/documentation.html). This annual raw data can be downloaded in either SAS or CSV format from their File Transfer Protocol (FTP) site. These data are also available via the web API( Application Programming Interface), and the R package [tidcycensus](https://walker-data.com/tidycensus/index.html) can be used to download and handle this data set more efficiently by using these APIs. In the code section I will explain how I did use the tidycensus package to dl the subset of the data I needed for this analysis.
 
## Cleaned Data 

Using API and tidycensus package, I downloaded the data set with only variables I needed for each year to make the downloading faster I also only downloaded people aged between 18-65 who are eligible for Medicaid,assigend a variable year to each of them and then I merged all the annual datasets in a one single dataset by year saved it as rawdata. In the next step, using my inclusion criteria I created a subset of data, fixed the mismatch, changed the vacant values with NA a this would be the cleaneddata1. But, you will also find cleaneddata2 in the folder that would be the final data set whihch includes my created variable as well and exported it to Stata format for future use.. 

I will use both R and Stata for running analyses. For now, I only used R for cleaning and preparing the data set for the main analysis in Stata.

### R codes
### Stata codes

## Text
## Presentation
 
