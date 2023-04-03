library(tidyverse)
library(ggplot2)
library(plotly)
library(glue)
library(patchwork)
library(leaflet)
library(config)

config <- config::get()

# Topo template
t_prefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
key <- config$linzkey
tMid <- "/tiles/v4/layer="
tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"
topo_50_template <- paste0(t_prefix, key, tMid, 50767, tSuffix)

generate_map <- function(catchments, sites, flow_sites, rainfall_sites) {

  flow_sites_plot  <- subset(sites, site_name %in% flow_sites)
  rainfall_sites_plot <- subset(sites, site_name %in% rainfall_sites)
  
  leaflet() %>%
    addTiles(urlTemplate = topo_50_template, group = "NZ Topo50") %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    # add catchments
    addPolygons(
      group = "Catchments",
      data = catchments,
      fillColor = "blue",
      weight = 2,
      opacity = 1,
      color = "blue",
      fillOpacity = 0.2,
      labelOptions = labelOptions(noHide = FALSE, direction = "auto")
    ) %>%
    addLabelOnlyMarkers(
      group = "Catchments",
      data = catchment_centroids,
      label = ~ as.character(catchment),
      labelOptions = labelOptions(
        noHide = TRUE, direction = "top", textOnly = TRUE,
        style = list(
          "color" = "blue",
          "font-family" = "serif",
          "font-style" = "bold",
          # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
          "font-size" = "15px"
          # "border-color" = "rgba(0,0,0,0.5)"
        )
      )
    ) %>%
    # add rivers
    addPolylines(
      group = "Rivers",
      data = rivers,
      weight = 1
    ) %>%
    # add flow sites
    addCircleMarkers(
      group = "Flow Sites",
      data = flow_sites_plot,
      radius = 5,
      color = "red",
      stroke = FALSE,
      fillOpacity = 1,
      label = ~ as.character(site),
      # clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
      labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list(
        "color" = "red",
        "font-family" = "serif",
        "font-size" = "15px"
      ))
    ) %>%
    # add rainfall sites
    addCircleMarkers(
      group = "Rainfall Sites",
      data = rainfall_sites_plot,
      radius = 5,
      color = "blue",
      stroke = FALSE,
      fillOpacity = 1,
      label = ~ as.character(site),
      # clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
      labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list(
        "color" = "red",
        "font-family" = "serif",
        "font-size" = "15px"
      ))
    ) %>%
    # Layers control
    addLayersControl(
      baseGroups = c("NZ Topo50", "OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Catchments", "Rivers", "Flow Sites", "Rainfall Sites"),
      options = layersControlOptions(collapsed = TRUE)
    )

}


# Testing
# f <- subset(flows, site_name == "Aorere at Devils Boots")
# r <- subset(rainfall, site_name == "Aorere at Devils Boots")
# start <- default_date - days(3)
# end <- default_date
# p_test <- generate_rainfall_flow_for_site_plot(f, r, start, end)

generate_rainfall_flow_for_site_plot <- function(f, r, start, end, flow_thresholds) {
  
  f_hysite <- unique(f$site)
  f_site <- unique(f$site_name)
  r_site <- unique(r$site_name)
  
  start_dt <- as_datetime(start, tz = "NZ")
  
  # Determine datetime ticks
  dt_axis_by <- ceiling(difftime(max(r$datetime, na.rm = TRUE), start_dt, units='hours') / 16) %>% 
    as.numeric() %>% 
    paste0(" hours")
  
  # Flow thresholds
  thresholds_site <- flow_thresholds %>%
    filter(site == !!f_hysite) %>%
    head(1) %>% 
    select_if(~ !any(is.na(.)))
  
  threshold_limits <- thresholds_site %>%
    select(ends_with("limit")) %>%
    slice(1) %>%
    unlist(use.names = FALSE)
  threshold_labels <- thresholds_site %>%
    select(ends_with("label")) %>%
    slice(1) %>%
    unlist(use.names = FALSE)
  
  thresholds_all <- tibble(x = start, limit = threshold_limits, label = threshold_labels)
  highest_record <- thresholds_all %>%
    filter(grepl("Highest", label)) %>%
    mutate(x = x + days(1))
  if (nrow(highest_record) == 0) {
    highest_record <- tibble(x = start, limit = -999, label = "")
  }
  thresholds_plot <- thresholds_all %>% filter(!grepl("Highest", label)) %>%
    filter(limit > max(f$flow_m3ps)) %>%  # only plot closest threshold
    head(1)
  if (nrow(thresholds_plot) == 0) {
    thresholds_plot <- thresholds_all %>% filter(!grepl("Highest", label)) %>% 
      arrange(desc(limit)) %>% 
      head(1)
  }
  
  p1 <- f %>%
    ggplot(aes(x = datetime, y = flow_m3ps)) +
    geom_line(size = 1.2, color = "red") +
    geom_area(fill = "red", alpha = 0.4) +
    geom_hline(yintercept = thresholds_plot$limit, linetype = "dashed", color = "black") +
    geom_text(data = thresholds_plot, aes(as_datetime(x, tz = "NZ"), limit, label = label, hjust = 0, vjust = 1.5), size = 4, color = "black") +
    theme_bw() +
    labs(x = "Datetime (NZDT)", y = "Flow (m3/s)", title = glue("{f_site} Flow")) +
    scale_x_datetime(breaks = seq(start_dt, max(r$datetime, na.rm = TRUE), by = dt_axis_by), date_labels = "%Y%m%d-%H", 
                     limits = c(start_dt, max(r$datetime, na.rm =TRUE))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.ticks.y.left = element_line(colour = "red"),
          axis.title.y.left = element_text(colour = "red"),
          axis.line.y.left = element_line(color = "red"),
          axis.text.y.left = element_text(color = "red"))
  
  total_rainfall <- round(sum(r$rainfall_total_mm), 0)
  
  rainfall_annotations <- data.frame(
    xpos = c(min(r$datetime, na.rm = TRUE) + (max(r$datetime, na.rm = TRUE) - min(r$datetime, na.rm = TRUE))/2),
    ypos = c(Inf),
    annotateText = c(glue("Total rainfall: {total_rainfall} mm")),
    hjustvar = c(-0.3),
    vjustvar = c(1.6)
  )
  
  coeff <- 10
  
  p2 <- r %>%
    ggplot(aes(x = datetime  - minutes(30), y = rainfall_total_mm)) +
    geom_col(color = "blue", fill = "blue", alpha = 0.4) +
    geom_line(aes(x = datetime, y = cumsum((rainfall_total_mm))/coeff), size = 0.8, color = "magenta", linetype = "twodash", inherit.aes = FALSE) +
    scale_y_continuous(
      name = "Hourly Rainfall (mm)",
      limits = c(0, max(r$rainfall_total_mm) * 1.05),
      sec.axis = sec_axis(~.*coeff, name="Cumulative Rainfall (mm)"),
      expand = c(0, NA)
    ) + 
    geom_text(data = rainfall_annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText), size = 4.5, color = "blue", alpha = 0.9) +
    theme_bw() +
    labs(x = "Datetime (NZDT)", title = glue("{r_site} Rainfall")) +
    scale_x_datetime(breaks = seq(start_dt, max(r$datetime, na.rm = TRUE), by = dt_axis_by), date_labels = "%Y%m%d-%H", 
                     limits = c(start_dt, max(r$datetime, na.rm =TRUE)))+
    theme(axis.title.x = element_blank(),
          #axis.ticks.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.y.left = element_line(colour = "blue"),
          axis.title.y.left = element_text(colour = "blue"),
          axis.line.y.left = element_line(color = "blue"),
          axis.text.y.left = element_text(color = "blue"),
          axis.ticks.y.right = element_line(colour = "magenta"),
          axis.title.y.right = element_text(colour = "magenta", angle = 90),
          axis.line.y.right = element_line(color = "magenta"),
          axis.text.y.right = element_text(color = "magenta"))
  
  p <- (p2 / p1)
}

generate_rainfall_summary_plot <- function(r, sites) {
  
  r_summary <- r %>%
    group_by(site) %>%
    summarise(
      event_rainfall_total = round(sum(rainfall_total_mm, na.rm = TRUE), 0),
      event_max_hrly_rainfall = round(max(rainfall_total_mm, na.rm = TRUE), 0)
    ) %>% 
    left_join(select(sites, c(site, catchment, site_name)), by = "site")
  
  ggplot(r_summary, aes(x = reorder(site_name, -event_rainfall_total), y = event_rainfall_total, fill = catchment)) +
    geom_bar(color = "black", alpha = 0.6, stat = "identity") +
    geom_text(mapping = aes(label = event_rainfall_total), size = 2, vjust = -1) + 
    theme_bw() +
    labs(x = "", y = "Rainfall Total (mm)", fill = "Catchment", title = "Rainfall Total by Site") + #caption = glue("at {now_plot})"
    scale_y_continuous(limits = c(0, max(r_summary$event_rainfall_total * 1.1)), expand = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}