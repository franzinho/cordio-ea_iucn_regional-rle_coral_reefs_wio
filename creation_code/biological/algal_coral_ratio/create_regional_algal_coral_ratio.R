##
##  Name:       create_regional_algal_coral_ratio.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                using algal-coral ratio method
##
##  Approach:   Import percent cover data and calculate ratio
##                of macroalgae to live coral cover (ACR)
##
##              For each eco-region, ACR is calculated by:
##                algal_coral_ratio = FA / (FA + HC)
##
##              Summary object saved as *.rda
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##
## 1. Set up
##
 ## -- call to percent cover data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/sessiles/"

  # set data file name
    data_file <- "regional_percent_cover.rda"

  # call to data
    load(paste0(data_locale, data_file))

##
## 2. Groom data
##
  # have a look
    regional_percent_cover
# # A tibble: 16,253 × 8
#    Ecoregion site_name Longitude Latitude  Year level1_code
#    <chr>     <chr>         <dbl>    <dbl> <dbl> <chr>
#  1 Comoros   Alamage        44.2    -12.2  2003 HC
#  2 Comoros   Alamage        44.2    -12.2  2004 HC
#  3 Comoros   Alamage        44.2    -12.2  2005 HC
#  4 Comoros   Alamage        44.2    -12.2  2007 HC
#  5 Comoros   Alamage        44.2    -12.2  2011 HC
#  6 Comoros   Alamage        44.2    -12.2  2015 HC
#  7 Comoros   Alamage        44.2    -12.2  2017 BS
#  8 Comoros   Alamage        44.2    -12.2  2017 DC
#  9 Comoros   Alamage        44.2    -12.2  2017 HC
# 10 Comoros   Alamage        44.2    -12.2  2017 INV
# # ℹ 16,243 more rows
# # ℹ 2 more variables: percent_cover_mean <dbl>,
# #   percent_cover_sd <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # set site id
    regional_percent_cover %<>%
      mutate(site_id = paste(Ecoregion, site_name, sep = "_"))


 ## -- filter sites without macroalgae data -- ## ----
  # set level 1 coral codes
    level1_corals <-
      c("HC")

  # set level 1 algal codes
    level1_algae <-
      c("AHAL",
        "AMAC",
        "ALG",
        "ATRF")

  # filter & summarise
    algal_coral_cover <-
      regional_percent_cover %>%
        dplyr::filter(level1_code %in% c(level1_corals, level1_algae)) %>%
        mutate(level1_code = ifelse(level1_code %in% level1_algae,
                                    "FA", level1_code)) %>%
        group_by(Ecoregion,
                 site_id,
                 Year,
                 level1_code) %>%
        summarise(percent_cover_sum = percent_cover_mean %>% sum(na.rm = TRUE))
# `summarise()` has grouped output by 'Ecoregion', 'site_id',
# 'Year'. You can override using the `.groups` argument.
# # A tibble: 1,121 × 5
# # Groups:   Ecoregion, site_id, Year [632]
#    Ecoregion site_id            Year level1_code percent_cover_sum
#    <chr>     <chr>             <dbl> <chr>                   <dbl>
#  1 Comoros   Comoros_Alamage    2003 HC                       25
#  2 Comoros   Comoros_Alamage    2004 HC                       18
#  3 Comoros   Comoros_Alamage    2005 HC                       19
#  4 Comoros   Comoros_Alamage    2007 HC                       24
#  5 Comoros   Comoros_Alamage    2011 HC                       24
#  6 Comoros   Comoros_Alamage    2015 HC                       65
#  7 Comoros   Comoros_Alamage    2017 HC                       54.2
#  8 Comoros   Comoros_Bambao_m…  2005 HC                       26
#  9 Comoros   Comoros_Bambao_m…  2009 HC                       14
# 10 Comoros   Comoros_Bambao_m…  2011 HC                       25
# # ℹ 1,111 more rows
# # ℹ Use `print(n = ...)` to see more rows


 ## -- select time period -- ##

 ## -- Since we cannot extrapolate into future we will           ##
 ##      use values from 2013-2019.                              ##
 ##    Time period options for 50 year periods:                  ##
 ##      ~1969-2019: assumption here is that only minor          ##
 ##      declines between 1969-1997, so pre-1998 baseline        ##
 ##      values from literature can be assumed to be             ##
 ##      equivalent to 1969 (or slightly lower) -                ##
 ##      D1 past 50 years                                        ##
 ##    The advantage of this is that we don’t need to            ##
 ##      extrapolate into the future, which comes with a         ##
 ##      number of assumptions, particularly picking the         ##
 ##      pattern (non-linear change due to bleaching events). -- ##


 ## -- calculate current algal-coral ratio -- ##
  # calculate acr
    regional_algal_coral_ratio <-
      algal_coral_cover %>%
        dplyr::filter(!percent_cover_sum %>% is.na()) %>%
        spread(level1_code, percent_cover_sum) %>%
        dplyr::filter(!FA %>% is.na(),
                      !HC %>% is.na()) %>%
        mutate(algal_coral_ratio = FA / (FA + HC))
# # A tibble: 489 × 6
# # Groups:   Ecoregion, site_id, Year [489]
#    Ecoregion site_id            Year    FA    HC algal_coral_ratio
#    <chr>     <chr>             <dbl> <dbl> <dbl>             <dbl>
#  1 Comoros   Comoros_Dyn_plat…  2018  36.7  32.2             0.533
#  2 Comoros   Comoros_Dyn_plat…  2017  57.9  35.7             0.618
#  3 Comoros   Comoros_Dyn_plat…  2018  61.2  21.3             0.742
#  4 Comoros   Comoros_Dzindri_…  2017  31.7  43.6             0.421
#  5 Comoros   Comoros_Dzindri_…  2018  53.5  39.7             0.574
#  6 Comoros   Comoros_Ferenga    2018  42.4  90.5             0.319
#  7 Comoros   Comoros_Hamare_c…  2017  27.2  55.9             0.327
#  8 Comoros   Comoros_Hamare_c…  2018  33.4  40.8             0.450
#  9 Comoros   Comoros_Hamare_p…  2018  48.2  30.0             0.616
# 10 Comoros   Comoros_Itsamia_…  2018 168.   60.6             0.734
# # ℹ 479 more rows
# # ℹ Use `print(n = ...)` to see more rows


##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/algal_coral_ratio/"

  # save to file
    save(regional_algal_coral_ratio,
      file = paste0(save_locale, "regional_algal_coral_ratio.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove variables
    rm(level1_corals,
       level1_algae)

  # remove intermediate objects
    rm(regional_percent_cover,
       algal_coral_cover)

  # remove core objects
    rm(regional_algal_coral_ratio)

