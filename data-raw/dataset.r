library(data.table)
library(openxlsx)
library(readxl)
library(lubridate)
library(sf)
library(sp)
library(rgdal)
library(maptools)


# Global var --------------------------------------------------------------

ref_age_cut_points <- c(seq(0, 90, by = 5), Inf)

# Prepare field code tables -----------------------------------------------

sheets <- getSheetNames("data-raw/OHCAO_coded_tables.xlsx")

ref_coding_tables <- lapply(sheets, function(sheet) {
  return(data.table(read.xlsx("data-raw/OHCAO_coded_tables.xlsx", sheet, check.names = TRUE)))
})
names(ref_coding_tables) <- sheets


# Prepare implementation date info ----------------------------------------

ref_implementation_dates <- fread("data-raw/ARP implementation dates - 2019-07-04.csv", header = TRUE, nrows = 11, select = c(1, 4:5))

setnames(ref_implementation_dates, c("site", "additional_triage_time", "revised_call_categories"))
ref_implementation_dates <- ref_implementation_dates[site != ""]

ref_implementation_dates[, ':=' (additional_triage_time = as.Date(additional_triage_time),
                             revised_call_categories = as.Date(revised_call_categories))]

stopifnot(all(ref_implementation_dates[, additional_triage_time < revised_call_categories]))


# Prepare ref_time_intervals for plotting -------------------------------------

time_intervals_wide <- copy(ref_implementation_dates)
ref_time_intervals <- melt(time_intervals_wide,
                       id.vars = "site",
                       variable.name = "period",
                       value.name = "ref_date",
                       variable.factor = FALSE)

ref_time_intervals[period == "additional_triage_time", ':=' (period = "pre-ARP",
                                                         start = floor_date(ref_date, unit = "month") - months(13),
                                                         end = floor_date(ref_date, unit = "month"))]

ref_time_intervals[period == "revised_call_categories", ':=' (period = "post-ARP",
                                                          start = floor_date(ref_date, unit = "month") + months(1),
                                                          end = floor_date(ref_date, unit = "month") + months(13))]

ref_time_intervals[, ref_date := NULL]

ref_time_intervals <- rbind(ref_time_intervals,
                            time_intervals_wide[, .(site,
                                                    period = "ARP",
                                                    start = floor_date(additional_triage_time, unit = "month"),
                                                    end = floor_date(revised_call_categories, unit = "month") + months(1))])


# Prepare CCG to Amb Trust lookup -----------------------------------------

ccg_to_amb_lookup <- data.table(read.xlsx("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/20190509-AmbSYS-2019-March.xlsx",
                                         sheet = "CCG to Ambulance Trust",
                                         startRow = 3, cols = c(2, 5)))

setnames(ccg_to_amb_lookup,
         c("Office.for.National.Statistics.April.2018.CCG.codes", "Ambulance.Service"),
         c("ccg18_code", "amb_code"))

ccg_to_amb_lookup <- ccg_to_amb_lookup[!is.na(amb_code) & amb_code != "IOW"]
stopifnot(ccg_to_amb_lookup[, .N, by = ccg18_code][N > 1, .N] == 0)

amb_code_to_site_lookup <- data.table(amb_code = c("EEAST",
                                                               "EMAS",
                                                               "LAS",
                                                               "NEAS",
                                                               "NWAS",
                                                               "SCAS",
                                                               "SECAmb",
                                                               "SWAS",
                                                               "WMAS",
                                                               "YAS"),
                                                  site = c("East of England",
                                                           "East Midlands",
                                                           "London",
                                                           "North East",
                                                           "North West",
                                                           "South Central",
                                                           "South East Coast",
                                                           "South Western",
                                                           "West Midlands",
                                                           "Yorkshire"))

ccg_to_amb_lookup <- merge(ccg_to_amb_lookup, amb_code_to_site_lookup, by = "amb_code", all = TRUE)
stopifnot(ccg_to_amb_lookup[is.na(amb_code) | is.na(site), .N] == 0)
ccg_to_amb_lookup[, amb_code := NULL]


# Prepare population data -------------------------------------------------

pop_data_archives <- data.table(year = 2018:2013,
                                            src = paste0("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fclinicalcommissioninggroupmidyearpopulationestimates%2f",
                                                         c("mid2018sape21dt5/sape21dt6bmid2018ccgsyoaestimatesunformatted.zip",
                                                           "mid2017/sape20dt6mid2017ccgsyoaestimatesunformatted.zip",
                                                           "mid2016/sape20dt6mid2016ccgsyoaestimatesunformatted.zip",
                                                           "mid2015/sape20dt6mid2015ccgsyoaestimatesunformatted.zip",
                                                           "mid2014/correctedsape20dt6mid2014ccgsyoaestimatesunformatted.zip",
                                                           "mid2013/sape20dt6mid2013ccgsyoaestimatesunformatted.zip")))

# Download and extract data
pop_data_list <- mapply(function(yr, link) {
  temp <- tempfile()
  download.file(link, temp)
  files <- unzip(temp, exdir = "./data-raw/temp")

  males <- data.table(read_excel(files[1], sheet = paste0("Mid-", yr, " Males"), skip = 6))[, ':=' (year = yr,
                                                                                                                        sex = "male")]
  females <- data.table(read_excel(files[1], sheet = paste0("Mid-", yr, " Females"), skip = 6))[, ':=' (year = yr,
                                                                                                                            sex = "female")]

  unlink(c(temp, files))

  return(rbind(males, females))
}, pop_data_archives$year, pop_data_archives$src, SIMPLIFY = FALSE)

pop_data_wide <- rbindlist(pop_data_list)

# Format pop data
setnames(pop_data_wide, make.names(gsub(".", "_", make.names(tolower(colnames(pop_data_wide))), fixed = TRUE), unique = TRUE))
setnames(pop_data_wide, c("area_codes", "X90_"), c("ccg18_code", "X90"))

# Check
pop_data_wide[, ages_summed := Reduce("+", .SD), .SDcols = paste0("X", 0:90)]
stopifnot(pop_data_wide[all_ages != ages_summed, .N] == 0)

# Remove unneccesary fields
pop_data_wide[, c("all_ages", "ages_summed", "area_names") := NULL]

pop_data_wide <- pop_data_wide[substr(ccg18_code, 1, 3) == "E38"]

pop_data_wide <- merge(pop_data_wide, ccg_to_amb_lookup, by = "ccg18_code")

pop_data <- melt(pop_data_wide, id.vars = c("ccg18_code", "site", "year", "sex"),
                             measure.vars = patterns("^X"),
                             variable.name = "age",
                             variable.factor = FALSE,
                             value.name = "pop")

pop_data[, age := as.integer(substring(age, 2))]
pop_data[, age_group := cut(age, ref_age_cut_points, right = FALSE)]
pop_data[, age_group := levels(age_group)[age_group]]

ref_annual_site_pop <- pop_data[, .(pop = sum(pop)), by = .(year, site, sex, age_group)]

# annual_site_pop_diff <- copy(ref_annual_site_pop)
# setorder(annual_site_pop_diff, year)
# annual_site_pop_diff[, annual_pop_diff := shift(pop, 1, type = "lead") - pop, by = .(site, sex, age_group)]
# annual_site_pop_diff <- merge(annual_site_pop_diff,
#                          annual_site_pop_diff[year == 2017, .(year = 2018L, site, sex, age_group, y2017_annual_pop_diff = annual_pop_diff)],
#                          by = c("year", "site", "sex", "age_group"),
#                          all.x = TRUE)
# annual_site_pop_diff[year == 2018, annual_pop_diff := y2017_annual_pop_diff]
# annual_site_pop_diff[, y2017_annual_pop_diff := NULL]
#
#
# ref_monthly_site_pop <- annual_site_pop_diff[rep(1:nrow(annual_site_pop_diff), 12), .(year, site, sex, age_group)]
# ref_monthly_site_pop[, month := rep(1:12, each = nrow(ref_monthly_site_pop) / 12)]
# ref_monthly_site_pop[month < 6, base_year := year - 1L]
# ref_monthly_site_pop[month >= 6, base_year := year]
#
# ref_monthly_site_pop <- merge(ref_monthly_site_pop,
#                           annual_site_pop_diff[, .(base_year = year, site, sex, age_group, base_pop = pop, annual_pop_diff)],
#                           by = c("base_year", "site", "sex", "age_group"),
#                           all.x = TRUE)
#
# # Linear interpolation
# ref_monthly_site_pop[, pop := base_pop + round(((month - 6) %% 12) * annual_pop_diff / 12)]
#
# ref_monthly_site_pop[, month_year := as.Date(paste(year, sprintf("%02d", month), "01", sep = "-"))]
#
# ref_monthly_site_pop[, c("base_year",
#                      "base_pop",
#                      "annual_pop_diff",
#                      "year",
#                      "month") := NULL]
#
# ref_monthly_site_pop <- copy(ref_monthly_site_pop[month_year >= min(ref_time_intervals$start) & month_year <= max(ref_time_intervals$start)])
#

# Geographical data -------------------------------------------------------

# Download CCG boundary
ccg18_shp_data <- "https://opendata.arcgis.com/datasets/5252644ec26e4bffadf9d3661eef4826_2.zip?outSR=4326"
temp <- tempfile(fileext = ".zip")
deo_data_path <- "data-raw/geographical_data"
download.file(ccg18_shp_data, temp, mode = "wb")
files <- unzip(temp, exdir = "./data-raw/temp")

# Read in CCG boundary data
ccg_sp_data <- readOGR(dsn = "data-raw/temp",
                       layer = "Clinical_Commissioning_Groups_April_2018_Generalised_Clipped_Boundaries_in_England")

# Merge CCG to Amb service lookp into CCG boundary data
ccg_sp_data <- merge(ccg_sp_data, ccg_to_amb_lookup, by.x = "ccg18cd", by.y = "ccg18_code", all.x = TRUE)

# Collapse CCG boundaries to Ambulance service boundaries
amb_sp_data <- unionSpatialPolygons(ccg_sp_data, IDs = ccg_sp_data@data$site)

# Promote to SpatialPolygonsDataFrame
amb_sp_data <- SpatialPolygonsDataFrame(amb_sp_data,
                                             data.frame(site = names(amb_sp_data),
                                                        stringsAsFactors = FALSE),
                                             match.ID = "site")


# Reproject from BNG (British National Grid) to EPSG:3857 (Worldwide GPS system)
ref_amb_sf_data_EPSG3857 <- st_as_sf(spTransform(amb_sp_data, CRS("+init=epsg:3857")))

# delete temp/geo files
unlink(c(temp, files))


# ESP 2013 ----------------------------------------------------------------

ref_esp2013_data <- fread("https://www.opendata.nhs.scot/dataset/4dd86111-7326-48c4-8763-8cc4aa190c3e/resource/29ce4cda-a831-40f4-af24-636196e05c1a/download/european_standard_population_by_sex.csv")
setnames(ref_esp2013_data, c("age_group", "sex", "std_pop"))
ref_esp2013_data[, ':=' (age_group = cut(as.integer(substr(age_group,
                                              1,
                                              regexpr("(-|p)", age_group) - 1L)),
                                ref_age_cut_points,
                                right = FALSE),
                     sex = tolower(sex))]
ref_esp2013_data[, age_group := levels(age_group)[age_group]]


# Save internal data ------------------------------------------------------

usethis::use_data(ref_coding_tables,
                  ref_implementation_dates,
                  ref_time_intervals,
                  ref_annual_site_pop,
                  ref_amb_sf_data_EPSG3857,
                  ref_esp2013_data,
                  ref_age_cut_points,
                  internal = TRUE,
                  overwrite = TRUE)
