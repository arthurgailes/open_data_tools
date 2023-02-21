if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(ipumsr, dplyr, scales, testthat, here, glue)


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