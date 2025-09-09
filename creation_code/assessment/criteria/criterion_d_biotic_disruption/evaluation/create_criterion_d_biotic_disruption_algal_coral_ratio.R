##
##  Name:       create_criterion_d_biotic_disruption_algal_coral_ratio.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                using algal-coral ratio method
##
##  Approach:   Import data tables from original assessment, including:
##                 - raw algal coral ratio data table
##                 - baseline acr data
##              Compare with summary from gcrmn raw data
##
##              Loop through calculations from baseline estimations,
##                categorise threat status and summarise.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. Need to review final summary of threat categories
##                 by ecoregion                      [ fs: 2024-04-26 ]

##
## 1. Set up
##
 ## -- call to original acr data table -- ##
  # point to data locale
    data_locale <- "data_raw/assessment/criteria/dev/"

  # point to data file
    # data_file <- "criterion_d_acr_data_table.xlsx"
    data_file <- "Crit_D_ACR_data_table.xlsx"

  # call to data table
    criterion_d_acr_data_table <-
      paste0(data_locale, data_file) %>%
      read_xlsx()


 ## -- call to baseline acr data -- ##
  # point to data locale
    data_locale <- "data_raw/assessment/criteria/dev/"

  # point to data file
    # data_file <- "criterion_d_acr_baseline.xlsx"
    data_file <- "Criterion_D_acr_baseline.xlsx"

  # call to data table
    criterion_d_acr_baseline <-
      paste0(data_locale, data_file) %>%
      read_xlsx()


 ## -- call to algal coral ratio data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/algal_coral_ratio/"

  # set data file name
    data_file <- "regional_algal_coral_ratio.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    regional_algal_coral_ratio
# # A tibble: 489 × 6
# # Groups:   Ecoregion, site_id, Year [489]
   # Ecoregion site_id            Year    FA    HC algal_coral_ratio
   # <chr>     <chr>             <dbl> <dbl> <dbl>             <dbl>
 # 1 Comoros   Comoros_Dyn_plat…  2018  36.7  32.2             0.533
 # 2 Comoros   Comoros_Dyn_plat…  2017  57.9  35.7             0.618
 # 3 Comoros   Comoros_Dyn_plat…  2018  61.2  21.3             0.742
 # 4 Comoros   Comoros_Dzindri_…  2017  31.7  43.6             0.421
 # 5 Comoros   Comoros_Dzindri_…  2018  53.5  39.7             0.574
 # 6 Comoros   Comoros_Ferenga    2018  42.4  90.5             0.319
 # 7 Comoros   Comoros_Hamare_c…  2017  27.2  55.9             0.327
 # 8 Comoros   Comoros_Hamare_c…  2018  33.4  40.8             0.450
 # 9 Comoros   Comoros_Hamare_p…  2018  48.2  30.0             0.616
# 10 Comoros   Comoros_Itsamia_…  2018 168.   60.6             0.734
# # ℹ 479 more rows
# # ℹ Use `print(n = ...)` to see more rows


 ## -- filter regional
  # set year for time-series cut-off
    y_thresh <- 2013

  # set cut off and average for all valuees in period
    regional_algal_coral_cover_summary <-
      regional_algal_coral_cover %>%
      dplyr::filter(Year >= y_thresh) %>%
      group_by(# site_id,
               # Country,
               Ecoregion) %>%
      summarise(recent_year = Year %>% max(na.rm = TRUE),
                first_year  = Year %>% min(na.rm = TRUE),
                no_years    = Year %>% unique() %>% length(),
                acr_recent  = algal_coral_ratio %>% head(1),
                acr_mean    = algal_coral_ratio %>% mean(na.rm = TRUE))

 ## -- compare data objects with original data table -- ##
  # review data summary
    regional_algal_coral_cover_summary %>% quickview()
#                     Ecoregion recent_year first_year no_years
# 1 Bight of Sofala/Swamp Coast        2014       2014        1
# 2                     Delagoa        2019       2013        7
# 3    East African Coral Coast        2018       2013        6
#   acr_recent  acr_mean
# 1  0.3577426 0.3577426
# 2  0.5842133 0.5056617
# 3  0.4280908 0.4251824

   # review original table
     criterion_d_acr_data_table %>% quickview()
#   Ecoregion Longitude  Latitude no_years recent_acr current_acr
# 1   Delagoa  32.72661 -27.41492        1  0.6542154   0.6542154
# 2   Delagoa  35.50539 -21.80775        1  0.4523651   0.4523651
# 3   Delagoa  35.50181 -21.72648        1  0.5722076   0.5722076
#   threshold_acr
# 1         0.833
# 2         0.833
# 3         0.833

 ## -- align to original wio data tables -- ##
  # for acr data table
    criterion_d_acr_data_table %<>%
      rename(Ecoregion = eco_rgn)

  # for baseline acr
    criterion_d_acr_baseline %<>%
      rename(Ecoregion = eco_rgn)


##
## 3. Evaluate criterion
##
 ## -- set collapse threshold -- ##
  # set acr threshold
    threshold_acr <- 0.833  ## -- use 0.8 as starting point -- ##

  # set iteration levels
    i_min <- 10
    i_max <- 1e3

  # set iteration interval
    i_interval <- 10

  # create empty object to hold results
    criterion_d_biotic_disruption_algal_coral_ratio <- tibble()

  # loop through iterations # i=5  ## -- for testing -- ##
    for(i in seq(from = i_min,
                 to   = i_max,
                 by   = i_interval)){

     ## -- calculate severity -- ##
      # set seed for reproducibility
        set.seed(i + 81)

      # randomly assign baseline values
        dat <-
          criterion_d_acr_data_table %>%
            left_join(criterion_d_acr_baseline %>%
                        dplyr::select(Ecoregion,
                                      baseline_mean,
                                      baseline_sd)) %>%
            mutate(baseline_acr = rnorm(1, mean = baseline_mean,
                                             sd = baseline_sd))

      # calculate relative severity
        dat %<>%
          mutate(relative_severity = 100 * (baseline_acr - current_acr) /
                                           (baseline_acr - threshold_acr))

      # bound by 0 and 100
        dat %<>%
          mutate(relative_severity = relative_severity %>% scales::rescale(to = c(0, 100)))

     ## -- determine extent -- ##
      # get proportion of stations for relative severity classes
        dat %<>%
          group_by(Ecoregion) %>%
            summarise(rel_sev_30 = 100 * sum(relative_severity >= 30 &
                                             relative_severity < 50) /
                                               length(relative_severity),
                      rel_sev_50 = 100 * sum(relative_severity >= 50 &
                                             relative_severity < 80) /
                                               length(relative_severity),
                      rel_sev_80 = 100 * sum(relative_severity >= 80 &
                                             relative_severity <= 100) /
                                               length(relative_severity))


 ## -- correction from mishal 2024-04-04 -- ##
  # need to re-evaluate from updated script
# # correct rel severity levels
# t_coral2$rel_30 <- rowSums(t_coral2[, c("rel_sev_30", "rel_sev_50", "rel_sev_80")])
# t_coral2$rel_50 <- rowSums(t_coral2[, c("rel_sev_50", "rel_sev_80")])

      # correct rel severity levels
        dat %>%
          mutate(rel_30 = (rel_sev_30 + rel_sev_50 + rel_sev_80),
                 rel_50 = (rel_sev_50 + rel_sev_80))


     ## -- assign threat status -- ##
      # set status
        dat %<>%
          mutate(status_30 = ifelse(rel_sev_30 >= 80 & rel_sev_30 <= 100, 2,        NA),
                 status_50 = ifelse(rel_sev_50 >= 80 & rel_sev_50 <= 100, 3,        NA),
                 status_50 = ifelse(rel_sev_50 >= 50 & rel_sev_50 < 80,   2, status_50),
                 status_80 = ifelse(rel_sev_80 >= 50 & rel_sev_80 < 80,   3,        NA),
                 status_80 = ifelse(rel_sev_80 >= 80 & rel_sev_80 <= 100, 4, status_80),
                 status_80 = ifelse(rel_sev_80 >= 30 & rel_sev_80 < 50,   2, status_80))

      # set nas to 1
        dat %<>%
          mutate(status_30 = ifelse(is.na(status_30), 1, status_30),
                 status_50 = ifelse(is.na(status_50), 1, status_50),
                 status_80 = ifelse(is.na(status_80), 1, status_80))

     ## -- pick most severe categories -- ##
      # set max from status categories
        dat %<>%
         mutate(max_threat = pmax(status_30,
                                  status_50,
                                  status_80))

      # create conversion object for threat values
        threat_conversions <-
          tribble(~threat_value, ~status,
                              # 0,    "LC",
                              # 1,    "NT",
                              1, "NT/LC",
                              2,    "VU",
                              3,    "EN",
                              4,    "CR",
                              5,    "CO")

      # convert threat values
        dat %<>%
          left_join(threat_conversions %>%
                      rename(max_threat = threat_value))


      # set iteration
        dat %<>%
          mutate(Iteration = i)

      # harvest results
        criterion_d_biotic_disruption_algal_coral_ratio %<>%
          bind_rows(dat)


      }


##
## 4. Review results
##
  # summarise
    algal_coral_ratio_summary <-
    criterion_d_biotic_disruption_algal_coral_ratio %>%
      group_by(Ecoregion,
               status,
               max_threat) %>%
      summarise(n_categories = n()) %>%
      mutate(percent = 100 * n_categories / sum(n_categories))
# # A tibble: 8 × 5
# # Groups:   Ecoregion, status [8]
#   Ecoregion               status max_threat n_categories percent
#   <chr>                   <chr>       <dbl>        <int>   <dbl>
# 1 Bight of Sofala/Swamp … NT/LC           1          100     100
# 2 Delagoa                 VU              2          100     100
# 3 East African Coral Coa… NT/LC           1          100     100
# 4 Mascarene Islands       NT/LC           1          100     100
# 5 Northern Monsoon Curre… EN              3          100     100
# 6 Seychelles              NT/LC           1          100     100
# 7 Southeast Madagascar    VU              2          100     100
# 8 Western and Northern M… NT/LC           1          100     100

      ## -- for each country/eco-region, need to take percentage    ##
      ##    of classifications which were either VU, EN, CR      -- ##

       # create a new column called threatened which
       # threat_prop$threatened<-threat_prop$max_threat
       # threat_prop$threatened[threat_prop$max_threat=='VU' |threat_prop$max_threat=='EN'|threat_prop$max_threat=='CR' ]<-'TH'
       #
       # #AND NOW SUM BASED ON COLUMN 'threatened'

       # set proportion
         algal_coral_ratio_summary %>%
           group_by(Ecoregion) %>%
           summarise(percent_threat = (percent[status == 'VU'|
                                               status == 'EN'|
                                               status == 'CR']) %>% sum(na.rm = TRUE))


##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_d_biotic_disruption_algal_coral_ratio,
      file = paste0(save_locale, "criterion_d_biotic_disruption_algal_coral_ratio.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(gcrmn_percent_cover,
       gcrmn_algal_coral_cover,
       gcrmn_algal_coral_cover_summary,
       criterion_d_acr_data_table)

  # remove core objects
    rm(criterion_d_biotic_disruption_algal_coral_ratio)

