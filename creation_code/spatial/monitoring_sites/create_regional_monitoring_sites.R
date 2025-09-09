##
##  Name:       create__sampling_sites.R
##
##  Objective:  Create data object for monitoring site
##              coordinates and other characteristics
##
##  Approach:   Import data table with site coordinates,
##              summarise and generate spatial object.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-13
##

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


 ## -- call to data sourced from original -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/sessiles/"

  # set data file name
    data_file <- "regional_percent_cover.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look at percent cover
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
# # ℹ 2 more variables: percent_cover_mean <dbl>, percent_cover_sd <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # have a loook at ecoregions
    regional_ecoregions
# Simple feature collection with 11 features and 1 field
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: -151726.1 ymin: 6709593 xmax: 3485904 ymax: 9900473
# Projected CRS: WGS 84 / UTM zone 37S
# # A tibble: 11 × 2
#    Ecoregion                                                     geometry
#  * <chr>                                                   <GEOMETRY [m]>
#  1 Comoros                 POLYGON ((1386573 8784736, 1386643 8784691, 1…
#  2 Delagoa                 POLYGON ((352929.6 7019372, 352761.4 7019172,…
#  3 East Madagascar         POLYGON ((1812767 8290355, 1859169 8294619, 1…
#  4 Mascarene Isl.          MULTIPOLYGON (((2762101 7918440, 2843855 7949…
#  5 N Mozambique-S Tanzania POLYGON ((874419.7 8763185, 874370.3 8762590,…
#  6 N Tanzania-Kenya        MULTIPOLYGON (((940503.7 9294775, 949666.6 92…
#  7 North Madagascar        MULTIPOLYGON (((1634850 8820959, 1634864 8820…
#  8 Seychelles Outer        POLYGON ((1964547 9232621, 2008642 9177089, 2…
#  9 Seychelles north        POLYGON ((2853145 9145640, 2853321 9144484, 2…
# 10 South Madagascar        POLYGON ((1772612 7414567, 1773997 7413740, 1…
# 11 West Madagascar         POLYGON ((1147690 8433605, 1159340 8434628, 1…

 ## -- link ecoregions -- ##
  # set to spatial object
    regional_percent_cover_sf <-
      regional_percent_cover %>%
      mutate(long = Longitude,
             lat  = Latitude) %>%
      dplyr::filter(!Longitude %>% is.na(),
                    !Latitude  %>% is.na()) %>%
      dplyr::select(# Country = Region,
                    Longitude,
                    Latitude,
                    long,
                    lat,
                    Year,
                    # Reef.zone,
                    level1_code,
                    percent_cover_mean,
                    percent_cover_sd) %>%
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
        dplyr::select(# Country,
                      Ecoregion,
                      Year,
                      # Reef.zone,
                      Latitude,
                      Longitude,
                      level1_code,
                      percent_cover_mean,
                      percent_cover_sd)


 ## -- generate criteria object -- ##
  # set level 1 code of interest
    level1_of_interest <-
      c("HC")

  # filter & summarise
    regional_percent_cover_summary <-
      regional_percent_cover %>%
      dplyr::filter(level1_code %in% level1_of_interest) %>%
      group_by(# site_id,
               # Country,
               Ecoregion,
               Latitude,
               Longitude) %>%
      summarise(first_year  = Year %>% min(na.rm = TRUE),
                recent_year = Year %>% max(na.rm = TRUE),
                lat         = Latitude  %>% unique(),
                long        = Longitude %>% unique(),
                no_years    = Year %>% unique() %>% length(),                ## -- no. of sampling points           -- ##
                year_gap    = (recent_year - first_year) + 1,                ## -- years between 1st & final record -- ##
                recent_coral_cover   = percent_cover_mean %>% tail(n = 1),        ## -- returns latest coral cover       -- ##
                original_coral_cover = percent_cover_mean %>% head(n = 1),        ## -- returns earliest coral cover     -- ##
                mean_coral_cover     = percent_cover_mean %>% mean(na.rm = TRUE)) ## -- ave across series                -- ##

  # have a look
    regional_percent_cover_summary %>% quickview()
# Ecoregion  Latitude Longitude first_year recent_year       lat
# 1   Comoros -12.97964  45.21137       2017        2017 -12.97964
# 2   Comoros -12.83212  45.22168       2017        2017 -12.83212
# 3   Comoros -12.78721  45.30792       2017        2017 -12.78721
#     long no_years year_gap recent_coral_cover original_coral_cover
# 1 45.21137        1        1           73.74802             73.74802
# 2 45.22168        1        1           48.30247             48.30247
# 3 45.30792        1        1           31.87533             31.87533
# mean_coral_cover
# 1         73.74802
# 2         48.30247
# 3         31.87533

 # ## -- create spatial object -- ##
 #  # create Ecoregion
 #    regional_percent_cover_summary %<>%
 #      mutate(Ecoregion = ecoregion_country %>% str_split_i("_", 1))

  # order columns
    regional_percent_cover_summary %<>%
      ungroup() %>%
      dplyr::select(Ecoregion,
                    # Country,
                    # ecoregion_country,   ## -- evaluating if this is redundant -- ##
                    # site_id,
                    lat,
                    long,
                    first_year,
                    recent_year,
                    no_years,
                    year_gap,
                    recent_coral_cover,
                    original_coral_cover,
                    mean_coral_cover)

  # generate spatial object
    regional_monitoring_sites <-
      regional_percent_cover_summary %>%
        st_as_sf(coords = c("long", "lat"),
                 crs    = 4326)


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/monitoring_sites/"

  # save ecoregions to file
    save(regional_monitoring_sites,
      file = paste0(save_locale, "regional_monitoring_sites.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_percent_cover,
       level1_of_interest,
       regional_percent_cover_summary)

  # remove core objects
    rm(regional_monitoring_sites)
