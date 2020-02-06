### Helper fns
plotSiteColours <- function(...) {
  amb_scale <- RColorBrewer::brewer.pal(12, "Paired")[1:10]
  names(amb_scale) <- c("East Midlands",
                        "East of England",
                        "London",
                        "North East",
                        "North West",
                        "South Central",
                        "South East Coast",
                        "South Western",
                        "West Midlands",
                        "Yorkshire")
  return(scale_discrete_manual(..., values = amb_scale, aesthetics = c("fill", "colour"),
                               guide = guide_legend(title = "Ambulance service",
                                                    title.position = "top")))
}

plotARPPeriods <- function(...) {
  return(geom_rect(data = ref_time_intervals,
                   aes(x = NULL, y = NULL, xmin = start, xmax = end, fill = factor(period, c("pre-ARP",
                                                                                             "ARP",
                                                                                             "post-ARP"))),
                   ymin = -Inf,
                   ymax = Inf,
                   alpha = .4))
}

plotMissingPeriods <- function(periods, ...) {
  return(geom_rect(data = periods, aes(x = NULL, y = NULL, xmin = min_date, xmax = max_date), fill = "black", ymin = -Inf, ymax = Inf, alpha = 1))
}

plotARPPeriodColours <- function(...) {
  return(scale_fill_manual(values = c("pre-ARP" = "#e9a3c9",
                                      "ARP" = "#b15928",
                                      "post-ARP" = "#5ab4ac"),
                           guide = guide_legend(title = "ARP Phase",
                                                title.position = "top")))
}


profileMissing <- function(data, variables_of_interest = NULL) {
  # identify cols of interest
  any_nas <- sapply(data, function(x) { return(any(is.na(x))) })
  if(is.null(variables_of_interest)) {
    na_cols <- names(any_nas[any_nas])
  } else {
    na_cols <- variables_of_interest[variables_of_interest %in% colnames(data)]
  }
  cols_of_interest <- c(na_cols, "ARP_phase", "ems_month", "site")

  # for those cols replace values with TRUE/FALSE for missing or not
  data_nas <- data.table::copy(data[, ..cols_of_interest])[, (na_cols) := lapply(.SD, is.na), .SDcols = na_cols]

  # melt data
  ohcao_data_long <- data.table::melt(data_nas, measure.vars = na_cols, variable.factor = FALSE)

  # count the numbers of missing values for each melted variable
  missing_value <- ohcao_data_long[, .(missing = sum(value), .N), by = .(ARP_phase, ems_month, site, variable)]

  return(missing_value)
}

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
