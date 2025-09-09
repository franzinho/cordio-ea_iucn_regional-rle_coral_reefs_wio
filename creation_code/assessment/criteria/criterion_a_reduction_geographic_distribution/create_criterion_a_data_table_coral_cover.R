##
##  Name:       create_criterion_a_data_table_coral_cover.R
##
##  Objective:  Standardise & format data for evaluating
##                criterion A: Reduction in geographic
##                distribution
##
##  Approach:   Call sessile data compilation, filter by:
##                - Hard coral cover
##                - time period
##                - geographic region,
##                summarise and save.
##
##              Output saved as *.rda
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Analysis assumes *current* state represents
##                   a 50-year period and that all sites were
##                   above threshold 50 years ago.
##                 This is sub-criterion A1 (i.e. past 50 years)
##              2. A cut-off year for most recent year (e.g. 2010),
##                   sets all sites at same time period
##              3. *Current* hard coral cover is the average of
##                   all time-points from cut-off year (e.g. 2013)
##              4. Need to parse out geographic coordinates
##                   for spatial visualisation
##

##
## 1. Set up
##
 ## -- call to regional percent cover data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/sessiles/"

  # point to data file
    data_file <- "regional_percent_cover.rda"

  # load data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look at percent cover data
    regional_percent_cover
# # A tibble: 17,266 × 8
#    Ecoregion site_name        Longitude Latitude  Year level1_code
#    <chr>     <chr>                <dbl>    <dbl> <dbl> <chr>
#  1 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 HC
#  2 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 SC
#  3 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 INV
#  4 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 AMAC
#  5 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 AHAL
#  6 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 ACOR
#  7 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 ATRF
#  8 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 BS
#  9 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 RUB
# 10 Comoros   Grand_Comore:NA…      43.3    -11.4  2018 SND
# # ℹ 17,256 more rows
# # ℹ 2 more variables: percent_cover_mean <dbl>,
# #   percent_cover_sd <chr>
# # ℹ Use `print(n = ...)` to see more rows

 ## -- generate criteria data table object -- ## ----
  # set level 1 code of interest
    level1_of_interest <-
      # c("Coral")
      c("HC")

  # set year threshold
    y_thresh <-
      c("2013")

 ## -- summarise for criterion a                                         ##
 ##      no_years                = no. of time points                    ##
 ##      year_gap                = years between 1st and final record    ##
 ##      recent_coral_cover      = returns latest coral cover            ##
 ##      original_coral_cover    = returns earliest coral cover          ##
 ##      mean_coral_cover_thresh = 2013 onwards                       -- ##

  # filter & summarise
    criterion_a_data_table_coral_cover <-
      regional_percent_cover %>%
        dplyr::filter(level1_code %in% level1_of_interest) %>%
        group_by(Ecoregion,
                 site_name) %>%
      summarise(first_year  = Year %>% min(na.rm = TRUE),
                recent_year = Year %>% max(na.rm = TRUE),
                lat         = Latitude  %>% mean(na.rm = TRUE),
                long        = Longitude %>% mean(na.rm = TRUE),
                no_years    = Year %>% unique() %>% length(),
                year_gap    = (recent_year - first_year) + 1,
                recent_coral_cover      = percent_cover_mean %>% tail(n = 1),
                original_coral_cover    = percent_cover_mean %>% head(n = 1),
                mean_coral_cover_thresh = percent_cover_mean[Year >= y_thresh] %>%
                                                             mean(na.rm = TRUE))
                # n           = n(),
                # n_diff      = n - no_years)

  # check n diff
    criterion_a_data_table_coral_cover %>% pull(no_years) %>% unique() %>% sort()
 # [1]  1  2  3  4  5  6  7  8  9 10 11 13 14 15 16 18 19 22 23

 ## -- get station frequencies per year -- ##
  # calculate frequencies
    station_frequencies <-
      criterion_a_data_table_coral_cover %>%
        group_by(recent_year) %>%
        summarise(n_stations = site_name %>% unique() %>% length())

  # set cumulative percentages
    station_frequencies %>%
      ungroup() %>%
      mutate(cumulative_percentage = 100 * cumsum(n_stations) / sum(n_stations),
             cumulative_percentage = cumulative_percentage %>% round(2)) %>%
      data.frame()
#    recent_year n_stations cumulative_percentage
# 1         1999          2                  0.25
# 2         2000         12                  1.75
# 3         2001         24                  4.76
# 4         2002          7                  5.63
# 5         2003         14                  7.38
# 6         2004          4                  7.88
# 7         2005         20                 10.39
# 8         2006         20                 12.89
# 9         2007         10                 14.14
# 10        2008         19                 16.52
# 11        2009         24                 19.52
# 12        2010         19                 21.90
# 13        2011         47                 27.78
# 14        2012         51                 34.17
# 15        2013         17                 36.30
# 16        2014        116                 50.81
# 17        2015         79                 60.70
# 18        2016         73                 69.84
# 19        2017        134                 86.61
# 20        2018         95                 98.50
# 21        2019         12                100.00


 ## -- filte stations by time & cover thresholds -- ## ----
  # set coral cover threshold
    c_thresh <- 10

  # set first year threshold
    f_thresh <- 1998

 ## -- Approach removes sites with had coral cover          ##
 ##       < 10% before 198?. Would be to create             ##
 ##       a list of sites & then determine whether          ##
 ##       to keep them, based on:                           ##
 ##         * naturally low coral cover - exclude           ##
 ##         * known to have degraded at some point          ##
 ##             in the last 50 years (from ~1970) - keep -- ##

  # filter naturally low cover sites
    criterion_a_data_table_coral_cover %<>%
      dplyr::filter(!original_coral_cover <= c_thresh,
                    !first_year           <  f_thresh)
# # A tibble: 612 × 11
# # Groups:   Ecoregion [11]
#    Ecoregion site_name first_year recent_year   lat  long no_years
#    <chr>     <chr>          <dbl>       <dbl> <dbl> <dbl>    <int>
#  1 Comoros   Anjouan:…       2003        2017 -12.2  44.2        6
#  2 Comoros   Anjouan:…       2015        2015 -12.2  44.2        1
#  3 Comoros   Anjouan:…       2009        2017 -12.2  44.5        3
#  4 Comoros   Anjouan:…       2005        2005 -12.2  44.5        1
#  5 Comoros   Anjouan:…       2016        2016 -12.2  44.2        1
#  6 Comoros   Anjouan:…       2003        2005 -12.1  44.3        3
#  7 Comoros   Anjouan:…       2009        2011 -12.3  44.4        2
#  8 Comoros   Anjouan:…       2009        2011 -12.4  44.5        2
#  9 Comoros   Anjouan:…       2016        2016 -12.2  44.3        1
# 10 Comoros   Anjouan:…       2003        2011 -12.1  44.3        4
# # ℹ 602 more rows
# # ℹ 4 more variables: year_gap <dbl>, recent_coral_cover <dbl>,
# #   original_coral_cover <dbl>, mean_coral_cover_thresh <dbl>
# # ℹ Use `print(n = ...)` to see more rows


 ## -- review data distribution per ecoregion -- ## ----
  # get frequencies
    criterion_a_data_table_coral_cover %>%
      group_by(Ecoregion) %>%
      summarise(n_stations = site_name %>% unique() %>% length())
# # A tibble: 11 × 2
#    Ecoregion               n_stations
#    <chr>                        <int>
#  1 Comoros                         63
#  2 Delagoa                         40
#  3 East Madagascar                 13
#  4 Mascarene Isl.                  22
#  5 N Mozambique-S Tanzania        151
#  6 N Tanzania-Kenya               135
#  7 North Madagascar                16
#  8 Seychelles Outer                30
#  9 Seychelles north                88
# 10 South Madagascar                 2
# 11 West Madagascar                 52

 ## -- *current* coral cover can be set as          ##
 ##       average across years or single value      ##
 ##       (e.g. ave_coral).  Otherwise, average     ##
 ##       can be calculated of all time-points      ##
 ##       from cut-off year (e.g. 2013)          -- ##

 ## -- remove stations with no data after cut-off -- ##
  # set recent year threshold
    r_thresh <- 2013

  # filter stations with no data
    criterion_a_data_table_coral_cover %<>%
      dplyr::filter(recent_year >= r_thresh)
# # A tibble: 412 × 11
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
# # ℹ 4 more variables: year_gap <dbl>, recent_coral_cover <dbl>,
# #   original_coral_cover <dbl>, mean_coral_cover_thresh <dbl>
# # ℹ Use `print(n = ...)` to see more rows


 ## -- clean data table object -- ## ----
  # set current coral cover values
    criterion_a_data_table_coral_cover %<>%
      mutate(current_coral_cover = recent_coral_cover)

  # # match ecoregion names
    # criterion_a_data_table_coral_cover %<>%
      # mutate(Ecoregion = ifelse(Ecoregion == "Mascarene Isl.",
                                             # "Mascarene Isl", Ecoregion),
             # Ecoregion = ifelse(Ecoregion == "N Mozambique-S Tanzania",
                                             # "N Mozambique - S Tanzania", Ecoregion),
             # Ecoregion = ifelse(Ecoregion == "N Tanzania-Kenya",
                                             # "N Tanzania - Kenya", Ecoregion),
             # Ecoregion = ifelse(Ecoregion == "Seychelles north",
                                             # "Seychelles North", Ecoregion))


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_a_data_table_coral_cover,
      file = paste0(save_locale, "criterion_a_data_table_coral_cover.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove parameter objects
    rm(y_thresh,
       c_thresh,
       f_thresh,
       r_thresh,
       level1_of_interest)

  # remove intermediate objects
    rm(station_frequencies,
       regional_percent_cover)

  # remove core data objects
    rm(criterion_a_data_table_coral_cover)

