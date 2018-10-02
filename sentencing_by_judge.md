Overview
--------

The following

### Loading Data

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

### Cleaning Data

We take three steps to clean our data:

-   Convert day and month sentences to years. This allows us to compare
    all sentences using the same units.
-   Combine the ACT and Section fields into the new 'ASID' field. This
    allows us to have a unique key for each specific charge.
-   Merge the judge status (Retired, Active, or Retention) from
    judge\_status.csv.

<!-- -->

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
included judges with more than 20 sentences to ensure meaningful sample
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
      filter(filter_count > 20) %>%
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
only included judges with more than 20 sentences to ensure meaningful
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
      filter(filter_count > 20) %>%
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
