##
##  Name:       create_criterion_a_reduction_geographic_distribution.R
##
##  Objective:  Standardise & format data for analysing criterion A:
##                Reduction in geographic distribution
##
##  Approach:   Although Criterion A refers to the "reduction in
##                geographic distribution", the data collection for
##                coral reef monitoring usually occurs as "point"
##                sampling at individual sites (i.e. site for
##                replicate quadrates).
##
##              This means that the use of sample "points"
##                (or monitoring sites) instead of a continuous
##                surface to evaluate Criterion A is not
##                entirely adequate.
##
##              The approach therefore is to use the sample
##                "points" (at monitoring sites) to estimate the
##                reduction of geographic distribution by
##                simulating the collapse of coral reef
##                ecosystems by evaluating the proportion of
##                sites collapsed for a given % of coral cover
##                loss.
##
##              Procedure involves:
##                1. Import of summary data for Criterion A
##                2. Filter relevant time period by Ecoregion
##                     (or Area of Assessment)
##                3. Estimate proportion loss & classify
##                4. Save object
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Analysis assumes *current* state represents a
##                   50-year period and that all sites were
##                   above threshold 50 years ago.
##                 This is sub-criterion A1 (i.e. past 50 years)
##              2. A cut-off year for most recent year (e.g. 2010), sets
##                   all sites at same time period
##              3. *Current* hard coral cover is the average of
##                   all time-points from cut-off year (e.g. 2013)
##              4. Improvement of the methodology to include
##                   spatial information to more accurately
##                   estimate the "Reduction in geographic
##                   distribution" and provide a more comparable
##                   standard across regions with different
##                   spatial extent of monitoring sites.

##
## 1. Set up
##
 ## -- call to collapse function -- ##
  # call to function
    source("R/calculate_collapse.R")

 ## -- call to relative reef areas -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # set data file name
    data_file <- "reef_area_ecoregions.rda"

  # call to data
    load(paste0(data_locale, data_file))


 ## -- call to criterion a data data -- ##
  # point to data locale
    data_locale <- "data_intermediate/assessment/criteria/"

  # set data file name
    data_file <- "criterion_a_data_table_coral_cover.rda"

  # call to data
    load(paste0(data_locale, data_file))

 # ## -- call to original data table -- ##
  # # point to data locale
    # data_locale <- "data_raw/criteria/"

  # # point to data file
    # data_file <- "criterion_a_data_table.xlsx"

  # # import data table
    # criterion_a_data_table <-
      # paste0(data_locale, data_file) %>%
      # read_xlsx()


##
## 2. Groom data
##
  # have a look
    criterion_a_data_table_coral_cover
# # A tibble: 412 × 12
# # Groups:   Ecoregion [10]
#    Ecoregion site_name first_year recent_year   lat  long no_years
#    <chr>     <chr>          <dbl>       <dbl> <dbl> <dbl>    <int>
#  1 Comoros   Anjouan:…       2003        2017 -12.2  44.2        6
#  2 Comoros   Anjouan:…       2015        2015 -12.2  44.2        1
#  3 Comoros   Anjouan:…       2009        2017 -12.2  44.5        3
#  4 Comoros   Anjouan:…       2016        2016 -12.2  44.2        1
#  5 Comoros   Anjouan:…       2016        2016 -12.2  44.3        1
#  6 Comoros   Anjouan:…       2003        2017 -12.1  44.3        4
#  7 Comoros   Grande_C…       2003        2017 -11.4  43.2        6
#  8 Comoros   Moheli:F…       2018        2018 -12.4  43.7        1
#  9 Comoros   Moheli:F…       2018        2018 -12.4  43.7        1
# 10 Comoros   Moheli:I…       2018        2018 -12.4  43.8        1
# # ℹ 402 more rows
# # ℹ 5 more variables: year_gap <dbl>, recent_coral_cover <dbl>,
# #   original_coral_cover <dbl>, mean_coral_cover_thresh <dbl>,
# #   current_coral_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

 ## -- match ecoregion names -- ## ----
  # from criterion table
    criterion_a_data_table_coral_cover %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                   "Delagoa"
 # [3] "East Madagascar"           "Mascarene Isl"
 # [5] "N Mozambique - S Tanzania" "N Tanzania - Kenya"
 # [7] "North Madagascar"          "Seychelles Outer"
 # [9] "Seychelles North"          "West Madagascar"

  # check from reef area table
    reef_area_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                   "Delagoa"
 # [3] "East Madagascar"           "Mascarene Isl"
 # [5] "N Mozambique - S Tanzania" "N Tanzania - Kenya"
 # [7] "North Madagascar"          "Seychelles Outer"
 # [9] "Seychelles North"          "South Madagascar"
# [11] "West Madagascar"


##
## 3. Evaluate criterion
##
 ## -- calculate eco-regional and regional results -- ##
  # apply function to whole region
    collapse_regional <-
      criterion_a_data_table_coral_cover %>%
        calculate_collapse(by_ecoregion = FALSE)

  # apply function to at eco-regional scale
    collapse_by_ecoregion <-
      criterion_a_data_table_coral_cover %>%
      calculate_collapse(by_ecoregion = TRUE)


 ## -- calculate regional results - weighted and unweighted means -- ##
  # add proportion of regional coral reef area within each eco-region
    dat_collapse_weighted <-
      collapse_by_ecoregion %>%
         left_join(reef_area_ecoregions %>%
                     dplyr::select(Ecoregion,
                                   prop_area),
                     by = "Ecoregion"
           )

  # calculate weighting
    dat_collapse_weighted %<>%
      mutate(weighted_prop = (prop_collapse * prop_area) %>% round(5))

 ## -- calculate regional statistics -- ##
  # calculate regional collapsed for each threshold
    dat_collapse_regional <-
      dat_collapse_weighted %>%
        group_by(threshold) %>%
        summarise(weighted_prop              = weighted_prop %>% mean(na.rm = TRUE),
                  equal_weight_prop_collapse = prop_collapse %>% mean(na.rm = TRUE)) %>%
        mutate(weighted_prop              = weighted_prop %>% round(2),
               equal_weight_prop_collapse = equal_weight_prop_collapse %>% round(2))

   ## -- convert to match format of eco-regional results -- ##
       weighted_regional <-
         dat_collapse_regional %>%
         pivot_longer(
           cols      = c(weighted_prop, equal_weight_prop_collapse),
           names_to  = "Ecoregion",
           values_to = "prop_collapse") %>%
         mutate(
           Ecoregion = case_when(
                         Ecoregion == "weighted_prop" ~ "Regional_weighted",
                         Ecoregion == "equal_weight_prop_collapse" ~ "Regional_unweighted")
           )

   ## -- assign threat categories -- ##
    # classify
       weighted_regional %<>%
         mutate(status = ifelse(prop_collapse >= 0.80,                        "CR",     NA),
                status = ifelse(prop_collapse >= 0.50 & prop_collapse < 0.80, "EN", status),
                status = ifelse(prop_collapse >= 0.30 & prop_collapse < 0.50, "VU", status),
                status = ifelse(prop_collapse >= 0.27 & prop_collapse < 0.30, "NT", status),
                status = ifelse(prop_collapse < 0.27,                         "LC", status))


 ## -- combine objects -- ##
  # join weighted & unweighted regional with ecoregions results
     criterion_a_reduction_geographic_distribution <-
       collapse_by_ecoregion %>%
         bind_rows(collapse_regional,
                   weighted_regional)%>%
       arrange(threshold)


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_a_reduction_geographic_distribution,
      file = paste0(save_locale, "criterion_a_reduction_geographic_distribution.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # # remove parameter objects
  #   rm(s_cover,
  #      f_cover,
  #      t_interval)

  # remove core functions
    rm(calculate_collapse)

  # remove intermediate objects
    rm(criterion_a_data_table_coral_cover,
       dat_collapse_regional,
       dat_collapse_weighted,
       weighted_regional)

  # remove core data objects
    rm(criterion_a_reduction_geographic_distribution)

