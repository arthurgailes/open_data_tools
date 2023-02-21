Data Tools 2: A New Variable for Classifying Marital Status in the
American Community Survey
================

An R reproduction of STATA code in [Data Tools 2: A New Variable for
Classifying Marital Status in the American Community
Survey](https://www.aei.org/research-products/working-paper/data-tools-2-a-new-variable-for-classifying-marital-status-in-the-american-community-survey/)
by Charles Murray.

Running this file in R will convert the IPUMS source data in this
directory to the form discussed in the article above.

### Load and install (if necessary) packages

    ## Loading required package: pacman

    ## Warning: package 'pacman' was built under R version 4.1.1

### Download data to folder

\*\*Skip this section if you don’t have a beta API key

``` r
# sample reference: https://usa.ipums.org/usa-action/samples/sample_ids
samples <- paste0('us', 2013:2020, 'a')

# define the data extract
extract <- define_extract_usa(description = "Replication of Charles Murray's Data Tools part 2", samples =  samples,
  variables = c('YEAR', 'SERIAL', 'PERNUM', 'RELATED', 'SPLOC', 'MARST',
    'SEX'),
  data_format = 'fixed_width')

# submit the data request to IPUMS
submitted_extract <- submit_extract(extract)

# save the extract for reproduction
save_extract_as_json(extract, 'ipums_extract.json')
```

##### Have R check periodically until the extract is ready

``` r
extract <- define_extract_from_json('ipums_extract.json')

submitted_extract <- submit_extract(extract)
# will print TRUE when ready, may take a few minutes
wait_for_extract(submitted_extract)

workingset_path <- download_extract(submitted_extract)
```

*Alternatively, if you download the extract directly from IPUMS, you can
use the below:*

``` r
workingset_path <- 'usa_00021.xml' # your number will vary.
```

### Follow the instructions in the article

1.  Open the dataset you want to analyze. In this example, I’ll call it
    workingset. Sort it by YEAR, SERIAL, and PERNUM.

``` r
ddi <- read_ipums_ddi(workingset_path)
workingset <- read_ipums_micro(ddi) 
```

    ## Use of data from IPUMS USA is subject to conditions including that users should
    ## cite the data appropriately. Use command `ipums_conditions()` for more details.

``` r
# set to data.table for speed
setDT(workingset, key = c('SERIAL', 'PERNUM'))

setorder(workingset, YEAR, SERIAL, PERNUM)
```

2.  Keep YEAR, SERIAL, PERNUM, RELATED, SPLOC, MARST, and SEX, and drop
    the rest of the variables. Save this temporarily reduced database
    with its own name—say, temp.

``` r
temp <- copy(workingset[, list(YEAR, SERIAL, PERNUM, RELATED, SPLOC, MARST, SEX)])
```

3.  Drop all the cases of people who do not have a partner living in the
    household (SPLOC = 0).

``` r
temp <- temp[SPLOC != 0]
```

4.  Drop PERNUM and then rename SPLOC as PERNUM.

``` r
temp$PERNUM <- NULL
setnames(temp, 'SPLOC', 'PERNUM')
```

5.  Rename RELATED, MARST, and SEX so they reflect the values associated
    with the partner—for example, PARTRELATED, PARTMARST, and PARTSEX.

``` r
setnames(temp, c('RELATED', 'MARST', 'SEX'), 
  c('PARTRELATED', 'PARTMARST',  'PARTSEX'))
```

6.  Sort YEAR, SERIAL, and PERNUM. Save the revised temp.

``` r
setorder(temp, YEAR, SERIAL, PERNUM)
```

**Not necessary** 7. Reopen workingset.

8.  Merge workingset with temp using YEAR, SERIAL, and PERNUM

``` r
workingset <- merge(workingset, temp, by = c('YEAR', 'SERIAL', 'PERNUM'),
  all.x = T)
```

This reproduces the dataset used for analysis:

``` r
workingset
```

    ##           YEAR  SERIAL PERNUM SAMPLE     CBSERIAL HHWT      CLUSTER STRATA GQ
    ##        1: 2013       1      1 201301 8.400000e+01   65 2.013000e+12 260001  4
    ##        2: 2013       2      1 201301 1.540000e+02   51 2.013000e+12 250001  1
    ##        3: 2013       2      2 201301 1.540000e+02   51 2.013000e+12 250001  1
    ##        4: 2013       2      3 201301 1.540000e+02   51 2.013000e+12 250001  1
    ##        5: 2013       2      4 201301 1.540000e+02   51 2.013000e+12 250001  1
    ##       ---                                                                    
    ## 24854079: 2020 1193466      5 202001 2.020001e+12  112 2.020012e+12  50056  1
    ## 24854080: 2020 1193466      6 202001 2.020001e+12  112 2.020012e+12  50056  1
    ## 24854081: 2020 1193467      1 202001 2.020001e+12   50 2.020012e+12  20056  1
    ## 24854082: 2020 1193467      2 202001 2.020001e+12   50 2.020012e+12  20056  1
    ## 24854083: 2020 1193468      1 202001 2.020001e+12  172 2.020012e+12  30056  1
    ##           PERWT SPLOC RELATE RELATED SEX MARST PARTRELATED PARTMARST PARTSEX
    ##        1:    65     0     12    1270   2     6          NA        NA      NA
    ##        2:    51     2      1     101   2     1         201         1       1
    ##        3:    62     1      2     201   1     1         101         1       2
    ##        4:   232     0      3     301   1     6          NA        NA      NA
    ##        5:    97     0      3     301   2     6          NA        NA      NA
    ##       ---                                                                   
    ## 24854079:   103     0      3     301   2     6          NA        NA      NA
    ## 24854080:   107     0      3     301   1     6          NA        NA      NA
    ## 24854081:    50     2      1     101   2     1         201         1       1
    ## 24854082:    53     1      2     201   1     1         101         1       2
    ## 24854083:   172     0      1     101   1     6          NA        NA      NA

## Coding ALTMARST

The codes for MARST are:

``` r
ddi$var_info$val_labels[[8]]
```

    ## # A tibble: 7 x 2
    ##     val lbl                                        
    ##   <dbl> <chr>                                      
    ## 1     0 Vacant unit                                
    ## 2     1 Households under 1970 definition           
    ## 3     2 Additional households under 1990 definition
    ## 4     3 Group quarters--Institutions               
    ## 5     4 Other group quarters                       
    ## 6     5 Additional households under 2000 definition
    ## 7     6 Fragment

The RELATED code used to create ALTMARST is 1114 (“Unmarried partner”).
The codes for SEX are 1 for male and 2 for female.

Here is the code for creating ALTMARST

``` r
# cast the variables to lower case
setnames(workingset, tolower)
```

#### MARRIED

``` r
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
```

#### COHABITING

``` r
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
```

#### LIVING ALONE

``` r
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
```

#### CREATE ALTMARST VALUE LABELS

**Using R’s factors are the closest in concept to STATA labels**

``` r
workingset[, altmarst_factor := factor(altmarst, 
  levels = as.character(c(10:13, 20:22, 30:33)),
  labels = c("Married, straight", "Married, lesbian",
 "Married, gay", "Married, spouse absent", "Cohabit, straight",
 "Cohabit, lesbian", "Cohabit, gay", "Single, separated",
 "Single, divorced", "Single, widowed", "Single, never married")
)]

workingset[, list(.N), keyby = c('altmarst', 'altmarst_factor')]
```

    ##     altmarst        altmarst_factor        N
    ##  1:       10      Married, straight 10047772
    ##  2:       11       Married, lesbian    48846
    ##  3:       12           Married, gay    47604
    ##  4:       13 Married, spouse absent   456770
    ##  5:       20      Cohabit, straight  1032496
    ##  6:       21       Cohabit, lesbian    35424
    ##  7:       22           Cohabit, gay    35536
    ##  8:       30      Single, separated   317725
    ##  9:       31       Single, divorced  1858847
    ## 10:       32        Single, widowed  1308061
    ## 11:       33  Single, never married  9665002

## Replicating the results

``` r
mar_lesbian <- (sum(workingset$altmarst == 11))
mar_gay <- (sum(workingset$altmarst == 12))
cohab_lesbian <- (sum(workingset$altmarst == 21))
cohab_gay <- (sum(workingset$altmarst == 22))

#test
expect_equal(mar_lesbian, 48846)
expect_equal(mar_gay, 47604)
expect_equal(cohab_lesbian, 35424)
expect_equal(cohab_gay, 35536)
```

First, ALTMARST enables researchers to study the characteristics of
same-sex couples. Combining the ACS surveys from 2013–20 produces
samples of **48,846** married lesbian couples, **47,604** married gay
couples, **35,424** cohabiting lesbian couples, and **35,536**
cohabiting gay couples—samples large enough to permit many useful
analyses.
