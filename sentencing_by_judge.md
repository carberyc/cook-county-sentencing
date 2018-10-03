Overview
--------

This analysis examined sentencing harshness by Sentencing Judge in Cook
County by analyzing 189,287 criminal charges in Cook County from the
past eight years. We determine how each sentence a judge gives compares
to the median (expected) sentence for the given charge to evaluate
judicial harshness.

### Loading Data

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    sentencing_dat <- read.csv(url("https://datacatalog.cookcountyil.gov/api/views/tg8v-tm6u/rows.csv?accessType=DOWNLOAD"))
    judge_status <- read.csv('/Users/charliecarbery/Desktop/judge_by_status.csv')

### Cleaning Data

We take three steps to clean our data:

-   Convert day and month sentences to years. This allows us to compare
    all sentences using the same units.
-   Combine the ACT and Section fields into the new 'ASID' field. This
    allows us to have a unique key for each specific charge.
-   Merge the judge status (Retired, Active, or Retention)
    from judge\_status.csv.

<!-- -->

    clean_sentencing_dat <- 
      sentencing_dat %>%
      mutate(COMMITMENT_UNIT = case_when(COMMITMENT_UNIT == 'Year(s)' ~ 'YEARS',
                                         TRUE ~ as.character(COMMITMENT_UNIT)),
             COMMITMENT_TERM = case_when(COMMITMENT_UNIT == 'Months' ~ COMMITMENT_TERM/12.0,
                                         COMMITMENT_UNIT == 'Days' ~ COMMITMENT_TERM/365.0,
                                         TRUE ~ COMMITMENT_TERM)) %>%
      mutate(COMMITMENT_UNIT = case_when(COMMITMENT_UNIT %in% c('Months','Days') ~ 'YEARS',
                                         TRUE ~ COMMITMENT_UNIT),
             ASID = paste(ACT,'-',SECTION)) %>%
      mutate(judge_lower = tolower(gsub(" ","",SENTENCE_JUDGE))) %>%
      left_join(judge_status, by = 'judge_lower')

    ## Warning: Column `judge_lower` joining character vector and factor, coercing
    ## into character vector

### Calculate Median Sentences by Charge

In order to understand whether a specific sentence is relatively
harsh/lenient, we need to have a measure for a "typical" sentence for
each charge. We do this by taking the median of all sentences for each
charge, at the courthouse level. By looking at medians within a specific
courthouse, we ensure that each group of charges is coming, we ensure
that we are controlling for variation in defendants (particularly
criminal history) when using the median sentences to evaluate judges.

    asid_median <- 
      clean_sentencing_dat %>%
      filter(COMMITMENT_UNIT == 'YEARS',
             PRIMARY_CHARGE == 'true',
             SENTENCE_PHASE == 'Original Sentencing',
             SENTENCE_TYPE == 'Prison') %>%
      group_by(ASID, COURT_FACILITY) %>%
      dplyr::mutate(median_sentence = median(COMMITMENT_TERM, na.rm = T)) %>%
      ungroup() %>%
      dplyr::select(ASID, COURT_FACILITY, median_sentence) %>%
      unique()

### Overall Sentencing

For each judge, we look at every sentence and determine whether it is
above, below, or at the courthouse level median for the specific charge.
For this analysis, we are looking at all possible charges. We only
included judges with more than 50 sentences to ensure meaningful sample
size.

    overall_sentence_dist <- 
      clean_sentencing_dat %>%
      filter(COMMITMENT_UNIT == 'YEARS',
             PRIMARY_CHARGE == 'true',
             SENTENCE_PHASE == 'Original Sentencing',
             SENTENCE_TYPE == 'Prison') %>%
      group_by(SENTENCE_JUDGE) %>%
      mutate(filter_count = n()) %>%
      ungroup() %>%
      filter(filter_count > 50) %>%
      select(SENTENCE_JUDGE, COURT_FACILITY, ASID, COMMITMENT_TERM, STATUS) %>%
      left_join(asid_median, by = c('ASID', 'COURT_FACILITY')) %>%
      mutate(over_median = ifelse(COMMITMENT_TERM > median_sentence, 1, 0),
             at_median = ifelse(COMMITMENT_TERM == median_sentence, 1, 0),
             below_median = ifelse(COMMITMENT_TERM < median_sentence, 1, 0)) %>%
      group_by(SENTENCE_JUDGE) %>%
      mutate(prop_over = mean(over_median),
             prop_at = mean(at_median),
             prop_below = mean(below_median),
             count = n()) %>%
      ungroup() %>%
      select(SENTENCE_JUDGE, STATUS, prop_over, prop_at, prop_below, count) %>%
      unique()

### Overall Sentencing

For each judge, we look at every sentence and determine whether it is
above, below, or at the courthouse level median for the specific charge.
For this analysis, we are looking only at drug charges (Act 570). We
only included judges with more than 50 sentences to ensure meaningful
sample size.

    drug_sentence_dist <- 
      clean_sentencing_dat %>%
      filter(COMMITMENT_UNIT == 'YEARS',
             PRIMARY_CHARGE == 'true',
             SENTENCE_PHASE == 'Original Sentencing',
             SENTENCE_TYPE == 'Prison',
             ACT == '570') %>%
      group_by(SENTENCE_JUDGE) %>%
      mutate(filter_count = n()) %>%
      ungroup() %>%
      filter(filter_count > 50) %>%
      select(SENTENCE_JUDGE, STATUS, COURT_FACILITY, ASID, COMMITMENT_TERM) %>%
      left_join(asid_median, by = c('ASID', 'COURT_FACILITY')) %>%
      mutate(over_median = ifelse(COMMITMENT_TERM > median_sentence, 1, 0),
             at_median = ifelse(COMMITMENT_TERM == median_sentence, 1, 0),
             below_median = ifelse(COMMITMENT_TERM < median_sentence, 1, 0)) %>%
      group_by(SENTENCE_JUDGE) %>%
      mutate(prop_over = mean(over_median),
             prop_at = mean(at_median),
             prop_below = mean(below_median),
             count = n()) %>%
      ungroup() %>%
      select(SENTENCE_JUDGE, STATUS, prop_over, prop_at, prop_below, count) %>%
      unique()

### Results

The outputs for the overall sentencing, overall\_judges.csv, and drug
sentencing, drug\_sentencing.csv, can be accessed in this repository.
For any questions please contact Charlie Carbery: <carberyc@gmail.com>.
