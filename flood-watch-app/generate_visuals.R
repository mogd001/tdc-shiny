library(tidyverse)
library(ggplot2)
library(plotly)
library(glue)
library(patchwork)

generate_catchment_map <- function(catchments, sites, flow_sites, rainfall_sites) {

  flow_sites_plot  <- subset(sites, site_name %in% flow_sites)
  rainfall_sites_plot <- subset(sites, site_name %in% rainfall_sites)
  
  catchments <- st_transform(catchments, crs = 4326)
  
  catchment_map <- ggplot() +
    coord_cartesian() +
    geom_sf(catchments, mapping = aes(fill = catchment), alpha = 0.6) +
    geom_point(flow_sites_plot, mapping = aes(x = longitude, y = latitude), size = 1.5, alpha = 1, color = "red", shape = 3) +
    geom_point(rainfall_sites_plot, mapping = aes(x = longitude, y = latitude), size = 1.5, alpha = 1, color = "blue", shape = 1) +
    
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.background = element_blank(),
      legend.position = c(0.15, 0.9)
    ) +
    guides(color = "none", size = none)
  
  catchment_map %>% ggplotly(dynamicTicks = TRUE)
  
}

#f <- subset(flows, site_name == "Anatoki at Happy Sams")
#r <- subset(rainfall, site_name == "Nelson at Founders Park")
#start <- default_date - days(3)
#end <- default_date

generate_rainfall_flow_for_site_plot <- function(f, r, start, end) {
  
  f_site <- unique(f$site_name)
  r_site <- unique(r$site_name)
    
  p1 <- f %>%
    ggplot(aes(x = datetime, y = flow_m3ps)) +
    geom_line(size = 1.2, color = "red") +
    geom_area(fill = "red", alpha = 0.4) +
    #geom_hline(yintercept = thresholds_plot$limit, linetype = "dashed", color = "black") +
    #geom_text(data = thresholds_plot, aes(x, limit, label = label, hjust = 0, vjust = 1.5), size = 4, color = "black") +
    #geom_hline(yintercept = highest_record$limit, linetype = "dashed", color = "magenta") +
    #geom_text(data = highest_record, aes(x, limit, label = label, hjust = 0, vjust = -0.4), size = 2.2) +
    theme_bw() +
    labs(x = "Datetime (NZST)", y = "Flow (m3/s)", title = glue("{f_site} Flow")) +
    scale_x_datetime(breaks = seq(as_datetime(start, tz = "Etc/GMT+12"), max(f$datetime, na.rm = TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
    #scale_y_continuous(limits = c(0, max(c(f$flow_m3ps), na.rm = TRUE) * 1.05), expand = c(0, NA)) +
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
    hjustvar = c(1.0),
    vjustvar = c(1.6)
  )
  
  coeff <- 10
  
  p2 <- r %>%
    ggplot(aes(x = datetime, y = rainfall_total_mm)) +
    geom_col(color = "blue", fill = "blue", alpha = 0.4) +
    geom_line(aes(x = datetime, y = cumsum((rainfall_total_mm))/coeff), size = 0.8, color = "magenta", linetype = "twodash", inherit.aes = FALSE) +
    scale_y_continuous(
      name = "Hourly Rainfall (mm)",
      limits = c(0, NA),
      sec.axis = sec_axis(~.*coeff, name="Cumulative Rainfall (mm)"),
      #expand = c(0, 1)
    ) + 
    geom_text(data = rainfall_annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText), size = 4.5, color = "blue", alpha = 0.9) +
    theme_bw() +
    labs(x = "Datetime (NZST)", title = glue("{r_site} Rainfall")) +
    scale_x_datetime(breaks = seq(as_datetime(start, tz = "Etc/GMT+12"), max(f$datetime, na.rm =TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
    #scale_y_continuous(limits = c(0, max(rainfall_nearest$rainfall_total) * 1.05), expand = c(0, NA)) +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
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
    theme_bw() +
    labs(x = "", y = "Rainfall Total (mm)", fill = "Catchment", title = "Rainfall Total by Site") + #caption = glue("at {now_plot})"
    scale_y_continuous(limits = c(0, max(r_summary$event_rainfall_total * 1.05)), expand = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}