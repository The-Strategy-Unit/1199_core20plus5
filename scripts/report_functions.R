#' -----------------------------------------------------------------------------
#' REPORT FUNCTIONS
#'
#' Functions that support the writing of the hypertension metrics report
#' -----------------------------------------------------------------------------


# plotly -----------------------------------------------------------------------
plotly_font_family <- list(family = 'Arial, Helvetica, Droid Sans, sans')

#' Configure Plotly
#'
#' Carries out `plotly` configuration to hide unwanted modeBar buttons
#' define the toImage button output type and name prefix.
#'
#' @param p `plotly` object
#'
#' @returns a `plotly` object with config options set
configure_plotly <- function(p) {
  p <-
    p |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtons = list(list('toImage')),
      toImageButtonOptions = list(
        'format' = 'svg',
        'filename' = glue::glue(
          "core20plus5_", # name for this plot
          "{strftime(Sys.time(), '%Y%m%d_%H%M%S')}") # datetime
      )
    ) |>
    plotly::partial_bundle()

  return(p)
}

## colours ---------------------------------------------------------------------
crude_rate_range <- colorRampPalette(c("#abc0e0", "#5881c1"))
age_adj_range <- colorRampPalette(c("#fcdf83", "#f9bf07"))
su_pal <- StrategyUnitTheme::su_theme_pal("main")

## age-standardised split time-series ------------------------------------------

#' Plot time-series for both crude and age-standardised data
#'
#' Returns a `plotly::subplot()` containing two time-series charts over two
#' rows. The first row shows the crude rate and the second row shows the age-
#' standardised rate.
#'
#' @param df Tibble of time-series metric data for plotting
#' @param plot_title String the title to use as the plot title
#'
#' @returns {plotly} subplot object
plot_timeseries_agestandardised <- function(df, plot_title) {

  # prepare the annotations
  df_annotate <-
    df |>
    dplyr::slice_max(time_period_month)

  df_annotate_crude <-
    df_annotate |>
    dplyr::filter(
      stringr::str_detect(
        string = metric_category_type_name,
        pattern = "Age Standardised",
        negate = TRUE
      )
    )

  df_annotate_agestd <-
    df_annotate |>
    dplyr::filter(
      stringr::str_detect(
        string = metric_category_type_name,
        pattern = "Age Standardised",
        negate = FALSE
      )
    )

  # prepare the colours
  plot_colours <- su_pal(n = df$series |> levels() |> length())

  # crude rate
  p1 <-
    df |>
    dplyr::filter(
      stringr::str_detect(
        string = metric_category_type_name,
        pattern = "Age Standardised",
        negate = TRUE
      )
    ) |>
    plot_timeseries_single(
      df_annotate = df_annotate_crude,
      plot_colours = plot_colours,
      y_axis_title = "Crude hypertension prevalence\n(percent)"
    )

  # age-standardised rate
  p2 <-
    df |>
    dplyr::filter(
      stringr::str_detect(
        string = metric_category_type_name,
        pattern = "Age Standardised",
        negate = FALSE
      )
    ) |>
    plot_timeseries_single(
      df_annotate = df_annotate_agestd,
      plot_colours = plot_colours,
      y_axis_title = "Age-standardised hypertension prevalence\n(percent)"
    )

  # combine the two
  plot <-
    plotly::subplot(
      p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE
    ) |>
    plotly::layout(
      title = plot_title,
      font = plotly_font_family,
      showlegend = FALSE
    ) |>
    configure_plotly()

  return(plot)

}

#' Plot time-series for crude data
#'
#' Returns a `plotly::plot_ly()` containing a single time-series chart showing
#' the crude rate.
#'
#' @param df Tibble of time-series metric data for plotting
#' @param plot_title String - the title to use as the plot title
#' @param y_axis_title String - the title to use for the y-axis
#'
#' @returns {plotly} object
plot_timeseries <- function(df, plot_title, y_axis_title) {

  # prepare the annotations
  df_annotate <-
    df |>
    dplyr::slice_max(time_period_month)

  # prepare the colours
  plot_colours <- su_pal(n = df$series |> levels() |> length())

  # plot
  plot <-
    df |>
    plot_timeseries_single(
      df_annotate = df_annotate,
      plot_colours = plot_colours,
      y_axis_title = y_axis_title
    ) |>
    plotly::layout(
      title = plot_title,
      font = plotly_font_family,
      showlegend = FALSE
    ) |>
    configure_plotly()

  return(plot)
}


#' Plot a single time-series chart
#'
#' Returns a `plotly::plot_ly()` object containing a single time-series chart
#' base on the supplied details.
#'
#' @param df Tibble of time-series metric data for plotting
#' @param df_annotate Tibble of data for the last reporting period in `df`
#' @param plot_colours Vector of colours to use when generating the plot
#' @param y_axis_title String title to use for y-axis
#'
#' @returns {plotly} object
plot_timeseries_single <- function(
    df,
    df_annotate,
    plot_colours,
    y_axis_title = "(percent)"
  ) {

  plot <-
    df |>
    plotly::plot_ly(
      x = ~ time_period_month,
      y = ~ value,
      hovertemplate = ~ text,
      colors = plot_colours,
      color = ~ series
    ) |>
    plotly::add_trace(
      name = ~ series,
      type = "scatter",
      mode = "lines",
      line = list(width = 5),
      opacity = 0.3
    ) |>
    plotly::add_trace(
      name = ~ series,
      type = "scatter",
      mode = "markers"
    ) |>
    plotly::add_annotations(
      data = df_annotate,
      text = ~ series,
      showarrow = FALSE,
      xanchor = "left",
      borderpad = 10
    ) |>
    plotly::layout(
      font = plotly_font_family,
      xaxis = list(title = ""),
      yaxis = list(
        title = y_axis_title,
        rangemode = "tozero"
      ),
      showlegend = FALSE
    )

  return(plot)
}


# leaflet ----------------------------------------------------------------------

# set highlight options
leaflet_highlight <- leaflet::highlightOptions(
  weight = 2,
  color = "#151412",
  fillOpacity = 1,
  bringToFront = TRUE
)

#' Add Leaflet Polygon
#'
#' This function adds a polygon layer to a leaflet map.
#'
#' @param map a `leaflet` map object
#' @param report_period string name of the report period to use
#'
#' @returns `leaflet` map
add_leaflet_polygon <- function(map, report_period) {

  # add this layer to the poly_layers
  poly_layers <<- c(report_period, poly_layers) |> unique()

  # get the report period data
  df <-
    df_sii |>
    dplyr::filter(time_period_name_f == report_period) |>
    # add in ICB name
    dplyr::left_join(
      y = icb_sf |>
        dplyr::select(
          ICB23CD,
          area_name = ICB23NM
        ),
      by = dplyr::join_by(
        area_code == ICB23CD
      )
    )

  # prepare the labels
  labs <- df$hover_label |>
    lapply(htmltools::HTML)

  # add the data as polygons to the map
  map <-
    map |>
    leaflet::addPolygons(
      group = report_period,
      fillColor = ~ pal(df$estimate),
      opacity = 0.7,
      color = "white",
      fillOpacity = 0.7,
      weight = 1,
      highlightOptions = leaflet_highlight,
      label = labs,
      labelOptions = leaflet::labelOptions(
        style = list(
          "font-family" = "Arial, Helvetica, Droid Sans, sans",
          "font-size" = "14px"
        )
      )
    )

  return(map)

}
