##
##  Name:       create_coral_percent_cover.R
##
##  Objective:  Create coral and other benthic cover
##                data object from gcrmn sources
##
##  Approach:   Call to clean data compilation from gcrmn
##              sources, format and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Benthic cover data are primary source for
##                   Criterion A. Data should include a
##                   column assigning each record to the
##                   respective geographic unit of assessment.
##              2. Final part of script creates a summary of benthic
##                  to be used in final reporting template
##                  (i.e. either in Intro or method section) [ fs: 2025-07-23 ]


##
## 1. Set up
##
 ## -- call to ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # load data
    load(paste0(data_locale, data_file))


 ## -- call to percent cover data -- ##
  # point to data locale
    data_locale <- "data_raw/biological/sessiles/"

  # set data file name
    data_file <- "WIO_GCRMN_Benthic_RLE_2019.xlsx"

  # import data
    regional_percent_cover <-
      paste0(data_locale, data_file) %>%
      read_excel()


##
## 2. Groom data
##
  # have a look
    regional_percent_cover
# # A tibble: 18,276 × 17
#    Country  Year Sector  Site  Station Reef.zone Depth level1_code
#    <chr>   <dbl> <chr>   <chr> <chr>   <chr>     <chr> <chr>
#  1 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    HC
#  2 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    SC
#  3 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    INV
#  4 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    AMAC
#  5 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    AHAL
#  6 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    ACOR
#  7 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    ATRF
#  8 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    BS
#  9 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    RUB
# 10 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    SND
# # ℹ 18,266 more rows
# # ℹ 9 more variables: mean_cover <dbl>, sd <chr>,
# #   number_replicates <dbl>, Latitude <dbl>, Longitude <dbl>,
# #   Organization <chr>, Period <chr>, Source <chr>, eco_rgn <chr>
# # ℹ Use `print(n = ...)` to see more rows

  # compressed view
    regional_percent_cover %>% quickview()
#   Country Year       Sector Site     Station Reef.zone Depth
# 1 Comoros 2018 Grand_Comore   NA GC_A_P_SC06 Fore_reef    NA
# 2 Comoros 2018 Grand_Comore   NA GC_A_P_SC06 Fore_reef    NA
# 3 Comoros 2018 Grand_Comore   NA GC_A_P_SC06 Fore_reef    NA
#   level1_code mean_cover  sd number_replicates Latitude Longitude
# 1          HC        3.3 6.5                24 -11.3791  43.28735
# 2          SC        0.0   0                24 -11.3791  43.28735
# 3         INV        0.0   0                24 -11.3791  43.28735
#   Organization Period   Source eco_rgn
# 1       CORDIO     NA RLE 2018 Comoros
# 2       CORDIO     NA RLE 2018 Comoros
# 3       CORDIO     NA RLE 2018 Comoros


 ## -- link ecoregions -- ##
  # set to spatial object
    regional_percent_cover_sf <-
      regional_percent_cover %>%
        mutate(site_name = paste(Sector, Site, Station, Reef.zone, sep = ":")) %>%
      mutate(long = Longitude,
             lat  = Latitude) %>%
      dplyr::filter(!Longitude %>% is.na(),
                    !Latitude  %>% is.na()) %>%
      dplyr::select(# Ecoregion,
                    site_name,
                    # reef_zone,
                    Longitude,
                    Latitude,
                    long,
                    lat,
                    Year,
                    level1_code,
                    percent_cover_mean = mean_cover,
                    percent_cover_sd   = sd) %>%
      st_as_sf(coords = c("long", "lat"),
               crs    = 4326)

  # link with ecoregions
    regional_percent_cover_sf %<>%
      st_transform(32737) %>%
    st_intersection(regional_ecoregions %>%
                      dplyr::select(Ecoregion,
                                    geometry))

  # return to data frame
    regional_percent_cover <-
      regional_percent_cover_sf %>%
        st_drop_geometry() %>%
        dplyr::select(Ecoregion,
                      site_name,
                      # reef_zone,
                      Longitude,
                      Latitude,
                      # long,
                      # lat,
                      Year,
                      level1_code,
                      percent_cover_mean,
                      percent_cover_sd)
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


 ## -- create summary table of available data -- ##

  ## -- review available percent cover data       ##
  ##    for each eco-region including:            ##
  ##    - temporal range of data                  ##
  ##    - number of survey sites               -- ##

   # set level1 code of interest
     level1_of_interest <-
       c("HC")

   # create data summary
     regional_percent_cover_summary <-
      regional_percent_cover %>%
        dplyr::filter(level1_code %in% level1_of_interest) %>%
      group_by(Ecoregion) %>%
      summarise(first_year  = Year %>% min(na.rm = TRUE),
                recent_year = Year %>% max(na.rm = TRUE),
                no_years    = Year %>% unique() %>% length(),
                no_sites    = site_name %>% unique() %>% length())  # update column name to site_id
# # A tibble: 11 × 5
#    Ecoregion              first_year recent_year no_years no_sites
#    <chr>                       <dbl>       <dbl>    <int>    <int>
#  1 Comoros                      1999        2018       12       64
#  2 Delagoa                      1993        2019       27       58
#  3 East Madagascar              1999        2018       10       13
#  4 Mascarene Isl.               1998        2019       22       23
#  5 N Mozambique-S Tanzan…       1999        2018       15      156
#  6 N Tanzania-Kenya             1992        2018       27      206
#  7 North Madagascar             1998        2017       16       17
#  8 Seychelles Outer             2010        2017        6       41
#  9 Seychelles north             1994        2019       16      160
# 10 South Madagascar             2007        2008        2        3
# 11 West Madagascar              1998        2018       19       58

  # # match ecoregion names
    # regional_percent_cover_summary %<>%
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
    save_locale <- "data_intermediate/biological/sessiles/"

  # save to file
    save(regional_percent_cover,
      file = paste0(save_locale, "regional_percent_cover.rda"))


##
## 4. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(level1_of_interest,
       regional_percent_cover_sf)

  # remove core objects
    rm(regional_percent_cover_summary,
       regional_percent_cover)
