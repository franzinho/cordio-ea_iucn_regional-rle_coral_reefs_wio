##
##  Name:       calculate_collapse.R
##
##  Objective:  Create helper function to calculate
##                the proportion of collapsed sites
##                for Criterion A - Reduction of
##                Geographic Distribution
##
##  Approach:   Set collapse function with defaults
##                for start & final thresholds,
##                interval and whether to
##                analyse based on ecoregional
##                spatial units.
##
##  Authors:    Mishal Gudka
##              Conservation Science Research Group,
##              University of Melbourne
##
##  Date:       2025-07-23
##

 ## -- create function for proportion of collapsed sites -- ##
  # set function
    calculate_collapse <-
      function(data,
               start_threshold    = 1,       ## start collapse threshold percent cover
               final_threshold    = 10,      ## final collapse threshold cover
               threshold_interval = 1,       ## test interval
               by_ecoregion       = TRUE) {  ## used at eco-regional & regional scale

  # create empty object to hold results
    result <- tibble()

  # loop to calculate   # p=10  ## -- for testing -- ##
    for (p in seq(from = start_threshold,
                  to   = final_threshold,
                  by   = threshold_interval)){

      # set threshold
        dat <-
          data %>%
            mutate(threshold = p)

      # create ratio
        dat %<>%
          mutate(ratio = current_coral_cover / threshold)

     ## -- set to assign a 1 if it has collapsed -- ##
      # set colapse
        dat %<>%
          mutate(collapse = ifelse(ratio <= 1, 1, 0))

      ## -- for each (if loop) eco-region or (area of assesment),   ##
      ##    calculate proportion of sites which collapsed        -- ##

      # loop
        if (by_ecoregion == TRUE) {

        # create data collapse object
          dat_collapse_raw <-
            dat %>%
              group_by(Ecoregion) %>%
            summarise(prop_collapse = (sum(collapse) / length(collapse)) %>% round(2),
            .groups = "drop"
            )
        } else {

          dat_collapse_raw <-
            dat %>%
              ungroup() %>%
            summarise(prop_collapse = (sum(collapse) / length(collapse)) %>% round(2)) %>%
            mutate(Ecoregion = 'Regional_overall')
        }

      # add threshold cover to data frame
        dat_collapse_raw %<>%
          mutate(threshold = p)

    ## -- set threat categories -- ##
     # classify
        dat_collapse_raw %<>%
          mutate(status = ifelse(prop_collapse >= 0.80,                        "CR",     NA),
                 status = ifelse(prop_collapse >= 0.50 & prop_collapse < 0.80, "EN", status),
                 status = ifelse(prop_collapse >= 0.30 & prop_collapse < 0.50, "VU", status),
                 status = ifelse(prop_collapse >= 0.27 & prop_collapse < 0.30, "NT", status),
                 status = ifelse(prop_collapse < 0.27,                         "LC", status))


      # Combine results for all thresholds
        result %<>%
          bind_rows(dat_collapse_raw)


    }

    # harvest results

    return(result)

    }

