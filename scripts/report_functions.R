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

#' Plot the SII for focal schemes
#' 
#' This function produces a faceted {plotly} time-series chart for the specified 
#' focal ICBs as a line and marker scatter plot with confidence intervals 
#' overlaid on a background of grey traces showing all other focal ICBs.
#'
#' @param .df_sii Tibble - time-series SII data to plot
#' @param .df_icb_focal Tibble - a df identifying the focal ICBs
#'
#' @returns {plotly}
plot_focal_scheme_sii_timeseries <- function(
    .df_sii,
    .df_icb_focal
) {
  # iterate over the focal icbs and generate a plotly::subplot
  
  # setup ---
  # define colours for the traces
  su_background_colour <- StrategyUnitTheme::su_theme_cols("light_charcoal")
  su_focal_colour <- StrategyUnitTheme::su_theme_cols("orange")
  su_focal_colour_line <- StrategyUnitTheme::su_theme_cols("light_orange")
  
  # data prep ---
  # limit df_sii to just the focal schemes
  df_focal <- 
    .df_sii |> 
    dplyr::filter(area_code %in% .df_icb_focal$area_code)
  
  # get a base time-series plot with all icbs as feint grey traces
  plot_base <-
    df_focal |> 
    plotly::plot_ly(
      x = ~ time_period_month
    ) |> 
    plotly::add_trace(
      name = "All ICBs",
      y = ~ estimate,
      type = "scatter",
      mode = "lines",
      hoverinfo = "skip",
      line = list(color = su_background_colour, width = 0.5),
      opacity = 0.5
    )
  
  # order icbs by their 'average' sii
  df_focal_ordered <-
    df_focal |> 
    dplyr::summarise(
      average_sii = estimate |> 
        mean(na.rm = TRUE) |> 
        abs(),
      .by = area_code
    ) |> 
    dplyr::arrange(average_sii)
  
  # iterate over these icbs and generate charts
  plot_list <-
    purrr::map(
      .x = df_focal_ordered$area_code,
      .f = \(.x) plot_focal_scheme_sii_timeseries_single(
        .icb = .x,
        .plot_base = plot_base,
        .df_focal = df_focal,
        .col_marker = su_focal_colour,
        .col_line = su_focal_colour_line
      )
    )
  
  plot <- 
    plotly::subplot(
      plot_list,
      nrows = length(plot_list),
      # add spacing between plots to allow for facet titles
      # needs scaling in proportion to number of facets
      margin = c(
        0, 0,
        0.1 * (1 / length(plot_list)),
        0.1 * (1 / length(plot_list))
      ), # l, r, t, b
      shareY = TRUE, 
      shareX = FALSE
    ) |> 
    configure_plotly()
  
  # combine these charts into a subplot
  #`return(plot_list)
  return(plot)
  
}

#' Plot the SII for a single focal scheme
#' 
#' This function produces a single {plotly} time-series chart for the specified 
#' focal ICB as a line and marker scatter plot with confidence intervals 
#' overlaid on a background of grey traces showing all other focal ICBs.
#'
#' @param .icb String - the area code of the ICB to produce this plot for
#' @param .plot_base {plotly} - a plot showing all focal ICB time-series as grey traces
#' @param .df_focal Tibble - time-series metric data for the focal ICBs
#' @param .col_marker Colour - the colour definition for the markers
#' @param .col_line Colour - the colour definition for the lines
#'
#' @returns {plotly}
plot_focal_scheme_sii_timeseries_single <- function(
    .icb,
    .plot_base,
    .df_focal,
    .col_marker,
    .col_line
) {
  # create a single chart to be included in the subplot
  
  # setup ---
  
  # filter data for the specified icb
  df_icb <-
    .df_focal |> 
    dplyr::filter(area_code == .icb)
  
  # create a plot for the specified icb
  plot <-
    .plot_base |> 
    plotly::add_trace(
      data = df_icb,
      name = "SII",
      y = ~ estimate,
      type = "scatter",
      mode = "lines"
    ) |> 
    # add the name of the ICB
    plotly::add_annotations(
      data = df_icb |> 
        dplyr::ungroup() |> 
        dplyr::select(area_name) |>
        dplyr::distinct(),
      text = ~ area_name,
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    ) |> 
    # add confidence interval
    plotly::add_trace(
      data = df_icb,
      name = "Upper CI",
      y = ~ upperci,
      type = "scatter",
      mode = "lines",
      line = list(color = "transparent"),
      hoverinfo = "skip"
    ) |> 
    plotly::add_trace(
      data = df_icb,
      name = "Lower CI",
      y = ~ lowerci,
      type = "scatter",
      mode = "lines",
      fill = "tonexty",
      fillcolor = adjustcolor(col = .col_marker, alpha.f = 0.2),
      line = list(color = "transparent"),
      hoverinfo = "skip"
    ) |> 
    # add the lines
    plotly::add_trace(
      data = df_icb,
      name = "SII",
      y = ~ estimate,
      type = "scatter",
      mode = "lines",
      line = list(color = .col_line, width = 5),
      hoverinfo = "skip"
    ) |>
    # add the markers
    plotly::add_trace(
      data = df_icb,
      name = "SII",
      y = ~ estimate,
      type = "scatter",
      mode = "markers",
      marker = list(color = .col_marker),
      hovertemplate = ~ hover_label_plotly
    ) |> 
    plotly::layout(
      title = "Slope Index of Inequality (SII) timeseries - focal ICBs",
      font = plotly_font_family,
      showlegend = FALSE,
      yaxis = list(
        title = "Slope Index of Inequality (SII)\n(Closer to zero is better)", 
        showgrid = FALSE, showline = FALSE, 
        rangemode = "tozero"
      ),
      xaxis = list(title = "", showgrid = FALSE, showline = FALSE)
    ) 
}


#' Plot a faceted chart of demographic intersections
#' 
#' This function produces a {plotly} subplot showing time-series charts 
#' of intersections between two demographic characteristics.
#' 
#' These are limited to Age and Gender as these are the only data available
#' from CVDPREVENT which are split by both characteristics.
#'
#' @param .df Tibble of time-series metric data for plotting
#' @param .y_axis_title String - the label to use on the y-axis
#'
#' @returns {plotly} plot
plot_intersection_timeseries <- function(
    .df,
    .y_axis_title = "(percent)"
) {
  
  # iterate over .df$facet and generate charts
  plot_list <-
    purrr::map(
      .x = .df$facet |> unique(),
      .f = \(.x) plot_intersection_timeseries_single(
        .df = .df,
        .facet = .x,
        .y_axis_title = .y_axis_title
      )
    )
  
  # create the faceted plot
  plot <- 
    plotly::subplot(
      plot_list,
      nrows = length(plot_list),
      # add spacing between plots to allow for facet titles
      # needs scaling in proportion to number of facets
      margin = c(
        0, 0,
        0.1 * (1 / length(plot_list)),
        0.1 * (1 / length(plot_list))
      ), # l, r, t, b
      shareY = TRUE, 
      shareX = FALSE
    ) |> 
    configure_plotly()
  
  # return the facet chart
  return(plot)
  
}

#' Plot a single chart of demographic intersections
#' 
#' This function produces a single {plotly} plot showing a time-series chart 
#' of the intersections between two demographic characteristics.
#' 
#' These are limited to Age and Gender as these are the only data available
#' from CVDPREVENT which are split by both characteristics.
#' 
#'
#' @param .df Tibble of time-series metric data for plotting
#' @param .facet String - the value of a single age-group on which to plot
#' @param .y_axis_title String - the label to use on the y-axis
#'
#' @returns {plotly} plot
plot_intersection_timeseries_single <- function(
    .df,
    .facet,
    .y_axis_title
) {
  
  # limit the data to the specified facet
  df <-
    .df |> 
    dplyr::filter(facet == .facet)
  
  
  # plot colours
  plot_colours <- su_pal(n = df$series |> levels() |> length())
  
  # annotations
  df_annotate <-
    df |>
    dplyr::slice_max(time_period_month)
  
  # plot
  plot <-
    df |>
    plotly::plot_ly(
      x = ~ time_period_month,
      y = ~ value,
      hovertemplate = ~ text,
      colors = plot_colours,
      color = ~ series
    ) |>
    plotly::add_annotations(
      data = df |>
        dplyr::ungroup() |> 
        dplyr::select(metric_category_name) |>
        dplyr::distinct(),
      text = ~ metric_category_name,
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    ) |>
    plotly::add_trace(
      data = df,
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
        title = .y_axis_title#,
        #rangemode = "tozero"
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