# https://stackoverflow.com/questions/30179442/plotting-minor-breaks-on-a-log-scale-with-ggplot
log_breaks <- function(maj, radix = 10) {
  function(x) {
    minx <- floor(min(logb(x, radix), na.rm = T)) - 1
    maxx <- ceiling(max(logb(x, radix), na.rm = T)) + 1
    n_major <- maxx - minx + 1
    major_breaks <- seq(minx, maxx, by = 1)
    if (maj) {
      breaks <- major_breaks
    } else {
      steps <- logb(1:(radix - 1), radix)
      breaks <- rep(steps, times = n_major) +
        rep(major_breaks, each = radix - 1)
    }
    radix^breaks
  }
}
scale_x_log_eng <- function(..., radix = 10) {
  scale_x_continuous(...,
                     trans = log_trans(radix),
                     breaks = log_breaks(TRUE, radix),
                     minor_breaks = log_breaks(FALSE, radix)
  )
}
scale_y_log_eng <- function(..., radix = 10) {
  scale_y_continuous(...,
                     trans = log_trans(radix),
                     breaks = log_breaks(TRUE, radix),
                     minor_breaks = log_breaks(FALSE, radix)
  )
}


# Determine estimate of ari duration
determine_ari_duration <- function(dl, v, hirds) {
  temp <-
    filter(hirds, duration_label == dl) %>%
    mutate(ari_numeric = as.numeric(as.character(ari)))
  # approximate ari for the duration
  if (v <= min(temp$val)) {
    return(-999)
  } else if (v >= max(temp$val)) {
    return(999)
  } else {
    return(approx(temp$val, temp$ari_numeric, v, method = "linear")$y)
  }
}


get_max_rainfall <- function(site, interval, from, to) {
  interval_offset <- convert_interval_to_offset(interval)

  get_data_site_measurement(site = site, measurement = "Rainfall", method = "Moving Average", interval = interval, from = from, to = to) %>%
    dplyr::select(datetime, value) %>%
    mutate(
      datetime = datetime + 0.5 * as.duration(interval_offset),
      duration_interval = interval
    ) %>%
    slice_max(value) %>%
    head(1)
}
