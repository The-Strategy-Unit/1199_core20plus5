#' -----------------------------------------------------------------------------
#' GET CVDPREVENT DATA
#'
#' 2025-03-04
#'
#' This script obtains outcome data from the CVDPREVENT audit and stores it for
#' later analysis.
#'
#' Metrics:
#' CVDP001HYP - prevalence of GP recorded hypertension
#' CVDP004HYP - of those with HT, BP reading in preceding 12 months
#' CVDP007HYP - of those with HT, where reading below age-appropriate threshold
#' CVDP002HYP - of those with HT, aged < 80, where BP reading is within threshold
#' CVDP003HYP - of those with HT, aged >= 80, where BP reading is within threshold
#' -----------------------------------------------------------------------------

# functions --------------------------------------------------------------------

#' Get Metric Data for a given System Level
#'
#' This function gathers  metric data for all time periods at which data is
#' available for the specified system level.
#'
#' The default is to use ICB-level data (ID = 7) for indicator 11 (hypertension
#' prevalence).
#'
#' @param .system_level_id Integer specifying the system level (aka geography) the metric data should be returned for. Default = 7, ICB data. Other useful values include 1 = England.
#' @param .indicator_ids Numeric vector - The ID numbers of the indicators to return data for.
#'
#' @returns Tibble of metric data for all time periods available at the specified system level.
get_metric_data_for_systemlevel <- function(
    .system_level_id = 7, # default to ICB-level data (ID = 7)
    .indicator_ids = c(11) # default to indicator 11 just to return some info
  ) {

  # list time periods with standard outcomes (id = 1)
  timeperiods_standard <-
    cvdprevent::cvd_time_period_list(indicator_type_id = 1)

  # list time periods where the system level data is reported
  timeperiods <-
    cvdprevent::cvd_area_system_level_time_periods() |>
    dplyr::filter(
      SystemLevelID == .system_level_id,
      TimePeriodID %in% timeperiods_standard$TimePeriodID
    )

  # iterate over each time period and gather data
  df <-
    purrr::map_dfr(
      .x = timeperiods$TimePeriodID,
      .f = \(.x) get_metric_data_for_timeperiod(
        .time_period_id = .x,
        .system_level_id = .system_level_id,
        .indicator_ids = .indicator_ids
      )
    )

  # return the result
  return(df)
}

#' Get Metric Data for a given Time Period ID
#'
#' Gathers metric data for a given time period and system level.
#'
#' @param .time_period_id Numeric - The ID number for the reporting period to gather metric data for.
#' @param .system_level_id Numeric - The ID number for the system level at which the metric data is required for.
#' @param .indicator_ids Numeric vector - The ID numbers of the indicators to return data for.
#' @returns Tibble of indicator metric data for the specified time period, system level.
get_metric_data_for_timeperiod <- function(
    .time_period_id,
    .system_level_id,
    .indicator_ids
  ) {

    # iterate over each indicator and gather metric data for the specified time period and system level
  df_return <-
    purrr::map_dfr(
      .x = .indicator_ids,
      .f = \(.x) cvdprevent::cvd_indicator_raw_data(
        indicator_id = .x,
        time_period_id = .time_period_id,
        system_level_id = .system_level_id
      )
    )

  # return the result
  return(df_return)
}

# main -------------------------------------------------------------------------
# define our indicator ids (as defined in the CVDPREVENT audit)
hyp_ind_id <- c(
  'CVDP001HYP' = 11,
  'CVDP002HYP' = 2,
  'CVDP003HYP' = 3,
  'CVDP004HYP' = 4,
  'CVDP005HYP' = 20,
  'CVDP006HYP' = 21,
  'CVDP007HYP' = 32,
  'CVDP009HYP' = 52
)

## england-level ----
# gather metric data over time
df_hyp_eng <- get_metric_data_for_systemlevel(
  .system_level_id = 1, # England
  .indicator_ids = hyp_ind_id
)

# save this for future use
saveRDS(
  object = df_hyp_eng,
  file = here::here('data', 'cvdprevent', 'cvd_prevent_eng.Rds')
)

## icb-level ----
# gather metric data over time
df_hyp_icb <- get_metric_data_for_systemlevel(
  .system_level_id = 7, # ICB
  .indicator_ids = hyp_ind_id
)

# save this for future use
saveRDS(
  object = df_hyp_icb,
  file = here::here('data', 'cvdprevent', 'cvd_prevent_icb.Rds')
)


# meta-data --------------------------------------------------------------------
# gather meta-data for these indicators
metric_meta <-
  purrr::map_dfr(
    .x = hyp_ind_id,
    .f = \(.x) cvdprevent::cvd_indicator_details(indicator_id = .x)
  )

saveRDS(
  object = metric_meta,
  file = here::here('data', 'cvdprevent', 'cvd_prevent_meta.Rds')
)

# experimentation --------------------------------------------------------------
#
# # time periods
# cvd_tp <- cvdprevent::cvd_time_period_list()
#
# # system levels
# cvd_sl <- cvdprevent::cvd_time_period_system_levels()
# # ICB is level 7
#
# # indicators for to time period 20
# cvd_indicators <- cvdprevent::cvd_indicator_list(time_period_id = 20, system_level_id = 7)
#
# hyp_ind_id <- c(
#   'CVDP001HYP' = 11,
#   'CVDP002HYP' = 2,
#   'CVDP003HYP' = 3,
#   'CVDP004HYP' = 4,
#   'CVDP005HYP' = 20,
#   'CVDP006HYP' = 21,
#   'CVDP007HYP' = 32,
#   'CVDP009HYP' = 52
# )
#
# # metrics
# cvd_metrics <- cvdprevent::cvd_indicator_metric_list(time_period_id = 20, system_level_id = 7)
#
# # there are two deprivation metrics, 'deprivation quintile' and 'deprivation quintile (age standardised)'
#
# # which reporting periods are available for ICBs
# hyp_icb_tp <- cvdprevent::cvd_area_system_level_time_periods()
#
# # which areas are available for a given time period
# icb_areas_tp20 <- cvdprevent::cvd_area_list(time_period_id = 20, system_level_id = 7)
#
# # all metric data for tp 20 at ICB level
# hyp_icb_tp20 <- cvdprevent::cvd_indicator_raw_data(indicator_id = 11, time_period_id = 20, system_level_id = 7)
