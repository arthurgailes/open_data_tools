if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(ipumsr, data.table, scales, testthat, here, glue, magrittr)


setwd(here())
datapath <- "data/data_tools_2"
dir.create(datapath, showWarnings = FALSE)
## Download Data ---------------------------------------------------------------
# check for the existence of the data; if exists, skip
if(length(list.files(datapath, pattern = "usa_\\d{5}")) < 2) {

# sample reference: https://usa.ipums.org/usa-action/samples/sample_ids
samples <- paste0('us', 2013:2020, 'a')

# define the data extract
extract <- define_extract_usa(
  description = "Replication of Charles Murray's Data Tools part 2",
  samples =  samples,
  variables = c('YEAR', 'SERIAL', 'PERNUM', 'RELATED', 'SPLOC', 'MARST', 'SEX'),
  data_format = 'fixed_width')

# submit the data request to IPUMS and wait for collection
submitted_extract <- submit_extract(extract)
downloadable_extract <- wait_for_extract(submitted_extract)

extract_path <- download_extract(downloadable_extract, datapath, overwrite = TRUE)

# save the extract for reproduction/sharing across languages
save_extract_as_json(extract, glue('{datapath}/ipums_extract.json'))
}

## Format Data according to Murray outline -------------------------------------

#' "1. Open the dataset you want to analyze. In this example, I’ll call it 
#' workingset. Sort it by YEAR, SERIAL, and PERNUM."

workingset_files <- list.files(datapath, pattern = "usa_\\d{5}", full.names = T)
workingset_ddi <- grep("xml$", workingset_files, value = TRUE)[[1]]

ddi <- read_ipums_ddi(workingset_ddi)
workingset <- read_ipums_micro(workingset_ddi) 

# set to data.table for speed/memory
setDT(workingset, key = c('SERIAL', 'PERNUM'))

setorder(workingset, YEAR, SERIAL, PERNUM)


#' 2. Keep YEAR, SERIAL, PERNUM, RELATED, SPLOC, MARST, and SEX, and drop the 
#' rest of the variables. Save this temporarily reduced database with its 
#' own name—say, temp.
temp <- workingset[, list(YEAR, SERIAL, PERNUM, RELATED, SPLOC, MARST, SEX)]


#' 3. Drop all the cases of people who do not have a partner living in the 
#' household (SPLOC = 0).
temp <- copy(temp[SPLOC != 0])

#' 4. Drop PERNUM and then rename SPLOC as PERNUM.
temp$PERNUM <- NULL
setnames(temp, 'SPLOC', 'PERNUM')


#'5. Rename RELATED, MARST, and SEX so they reflect the values associated with 
#' the partner—for example, PARTRELATED, PARTMARST, and PARTSEX.
setnames(
  temp, c('RELATED', 'MARST', 'SEX'), 
  c('PARTRELATED', 'PARTMARST',  'PARTSEX'))

#' 6. Sort YEAR, SERIAL, and PERNUM. Save the revised temp.
setorder(temp, YEAR, SERIAL, PERNUM)


#' **Not necessary** 7. Reopen workingset.

#' Merge workingset with temp using YEAR, SERIAL, and PERNUM

workingset <- merge(
  workingset, temp, 
  by = c('YEAR', 'SERIAL', 'PERNUM'),
  all.x = T)


#' This reproduces the dataset used for analysis:
workingset

## Create new variables --------------------------------------------------------
# Addition: cast the variables to lower case
setnames(workingset, tolower)

## Coding ALTMARST
#' The codes for MARST are here: ddi$var_info$val_labels[[8]]

#' The RELATED code used to create ALTMARST is 1114 (“Unmarried partner”).
#' The codes for SEX are 1 for male and 2 for female.

# MARRIED

workingset[, altmarst := fcase(
  # married, spouse present, straight
  sploc>0 & marst==1 & partmarst==1 & sex!=partsex, 10,
  # married, spouse present, lesbian
  sploc>0 & marst==1 & partmarst==1 & sex==2 & partsex==2, 11,
  # married, spouse present, gay
  sploc>0 & marst==1 & partmarst==1 & sex==1 & partsex==1, 12,
  # married, spouse absent
  sploc==0 & marst==2, 13
)]


# COHABITING

workingset[is.na(altmarst), altmarst := fcase(
  # cohabiting, straight
  related==1114 & sex!=partsex, 20,
  partrelated==1114 & sex!=partsex, 20,
  # cohabiting,lesbian
  related==1114 & sex==2 & partsex==2, 21,
  partrelated==1114 & sex==2 & partsex==2, 21,
  # cohabiting, gay
  related==1114 & sex==1 & partsex==1, 22,
  partrelated==1114 & sex==1 & partsex==1, 22
)]




# LIVING ALONE

workingset[is.na(altmarst), altmarst := fcase(
  # single, separated
  sploc==0 & marst==3 & related!=1114, 30,
  # single, divorced
  sploc==0 & marst==4 & related!=1114, 31,
  # single, widowed
  sploc==0 & marst==5 & related!=1114, 32,
  # single, never-married
  sploc==0 & marst==6 & related!=1114, 33
)]



# create altmarst value labels

#Using R's factors as the closest in concept to STATA labels


workingset[, altmarst_factor := factor(
  altmarst,
  levels = as.character(c(10:13, 20:22, 30:33)),
  labels = c(
    "Married, straight",
    "Married, lesbian",
    "Married, gay",
    "Married, spouse absent",
    "Cohabit, straight",
    "Cohabit, lesbian",
    "Cohabit, gay",
    "Single, separated",
    "Single, divorced",
    "Single, widowed",
    "Single, never married"
  )
)]
expect_false(any(is.na(workingset$altmarst_factor))) 

workingset[, list(.N), keyby = c('altmarst', 'altmarst_factor')]



## Replicating the results

mar_lesbian <- (sum(workingset$altmarst == 11))
mar_gay <- (sum(workingset$altmarst == 12))
cohab_lesbian <- (sum(workingset$altmarst == 21))
cohab_gay <- (sum(workingset$altmarst == 22))

#test
expect_equal(mar_lesbian, 48846)
expect_equal(mar_gay, 47604)
expect_equal(cohab_lesbian, 35424)
expect_equal(cohab_gay, 35536)




#' First, ALTMARST enables researchers to study the characteristics of same-sex 
#' couples. Combining the ACS surveys from 2013–20 produces samples of 
#' **`r comma(mar_lesbian)`** married lesbian couples, **`r comma(mar_gay)`** 
#' married gay couples, **`r comma(cohab_lesbian)`** cohabiting lesbian couples,
#' and **`r comma(cohab_gay)`** cohabiting gay couples—samples large enough to
#'  permit many useful analyses.
#'  
#'  "Combining the ACS surveys from 2013–20 produces samples of 48,846 married 
#'  lesbian couples, 47,604 married gay couples, 35,424 cohabiting lesbian 
#'  couples, and 35,536 cohabiting gay couples—samples large enough to permit many useful analyses."
#'
#' "The combined ACS surveys for 2013–20 have 1,032,496 participants living as 
#' cohabiting straight couples, who can be compared with 10,047,772 persons who 
#' are part of married straight couples living together."
#'
#' "In reality, 16 percent of persons classified as “divorced” using MARST had a
#' cohabiting partner, adding up to 360,719 people in the ACS surveys from 
#' 2013–20."
#'
#'"Among the people who MARST classifies as “never married” are 665,691 persons 
#' who are cohabiting with a partner."