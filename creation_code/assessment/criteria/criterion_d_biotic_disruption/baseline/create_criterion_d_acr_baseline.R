##
##  Name:       create_criterion_d_acr_baseline.R
##
##  Objective:  Create reference (or "baseline") table for
##                algal-coral ratio
##
##  Approach:   Import percent cover data table and
##                summarise for estimating "baseline" or
##                reference values for analysis.
##
##              Output saved as *.rda
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2025-08-20
##

##
## 1. Set up
##
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

 ## -- summarise algal:coral ratio -- ## ----
  # set year for time-series cut-off
    y_thresh <- 2013

  # set cut off & summarise
    regional_algal_coral_ratio_summary <-
      regional_algal_coral_ratio %>%
        dplyr::filter(Year >= y_thresh) %>%
      group_by(Ecoregion) %>%
      reframe(first_year       = Year %>% min(na.rm = TRUE),
              recent_year      = Year %>% max(na.rm = TRUE),
              no_years         = Year %>% unique() %>% length(),
              acr_first_mean   = algal_coral_ratio[Year == first_year]  %>% mean(na.rm = TRUE),
              acr_first_sd     = algal_coral_ratio[Year == first_year]  %>%   sd(na.rm = TRUE),
              acr_recent_mean  = algal_coral_ratio[Year == recent_year] %>% mean(na.rm = TRUE),
              acr_recent_sd    = algal_coral_ratio[Year == recent_year] %>%   sd(na.rm = TRUE),
              acr_overall_mean = algal_coral_ratio %>% mean(na.rm = TRUE),
              acr_overall_sd   = algal_coral_ratio %>%   sd(na.rm = TRUE))

 ## -- review stats -- ## ----
  # review data summary
    regional_algal_coral_ratio_summary %>% quickview()
        # Ecoregion first_year recent_year no_years acr_first_mean
# 1         Comoros       2016        2018        3      0.4955752
# 2         Delagoa       2013        2019        7      0.7148692
# 3 East Madagascar       2014        2018        3      0.4013664
  # acr_first_sd acr_recent_mean acr_recent_sd acr_overall_mean
# 1           NA       0.5594325      0.207993        0.4898943
# 2           NA       0.6422172            NA        0.6650561
# 3    0.3104011       0.4051315            NA        0.4184359
  # acr_overall_sd
# 1      0.2165976
# 2      0.2171200
# 3      0.1819885

 ## -- visualise -- ## ----
  # # open window
    # quartz("algal coral ratio", 7, 7)

  # # quick view
    # regional_algal_coral_ratio_summary %>%
      # dplyr::select(Ecoregion,
                    # acr_first_mean,
                    # acr_recent_mean,
                    # acr_overall_mean) %>%              
      # GGally::ggpairs()

 ## -- create baseline object -- ##
  # select reference values
    criterion_d_acr_baseline <-
      regional_algal_coral_ratio_summary %>%
        dplyr::select(Ecoregion,
                      acr_first_mean,
                      acr_first_sd)
# # A tibble: 10 × 3
   # Ecoregion               acr_first_mean acr_first_sd
   # <chr>                            <dbl>        <dbl>
 # 1 Comoros                          0.496      NA     
 # 2 Delagoa                          0.715      NA     
 # 3 East Madagascar                  0.401       0.310 
 # 4 Mascarene Isl.                   0.562       0.0797
 # 5 N Mozambique-S Tanzania          0.175       0.182 
 # 6 N Tanzania-Kenya                 0.394       0.269 
 # 7 North Madagascar                 0.113      NA     
 # 8 Seychelles Outer                 0.255       0.356 
 # 9 Seychelles north                 0.262       0.292 
# 10 West Madagascar                  0.711      NA 

##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_d_acr_baseline,
      file = paste0(save_locale, "criterion_d_acr_baseline.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove filter objects
    rm(y_thresh)

  # remove intermediate objects
    rm(regional_algal_coral_ratio,
       regional_algal_coral_ratio_summary)

  # remove core objects
    rm(criterion_d_acr_baseline)

