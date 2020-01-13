
# Read setup options ------------------------------------------------------
setup_opts = yaml::read_yaml("private.yaml")


# Read in data ------------------------------------------------------------

ohcao_data_raw <- data.table::fread(setup_opts[["data-source-filepath"]])

# Prepare field code tables -----------------------------------------------

sheets <- openxlsx::getSheetNames("data-raw/OHCAO_coded_tables.xlsx")

coding_tables <- lapply(sheets, function(sheet) {
  return(data.table::data.table(openxlsx::read.xlsx("data-raw/OHCAO_coded_tables.xlsx", sheet, check.names = TRUE)))
})
names(coding_tables) <- sheets

recodes <- coding_tables[["data_coding"]][substr(data_coding_type, 1, 2) == "tb" & field %in% colnames(ohcao_data_raw)]
recode_field_names <- recodes$field


# Attach coding labels to data --------------------------------------------

for(var in recode_field_names) {
  data.table::setnames(coding_tables[[recodes[field == var, data_coding_type]]], paste0(var, c("", "_original")))
  ohcao_data_raw <- merge(ohcao_data_raw,
                          coding_tables[[recodes[field == var, data_coding_type]]],
                          by = var,
                          all.x = TRUE)
}

# Replace original codes with labels
recode_field_name_labels <- paste0(recode_field_names, "_original")
ohcao_data_raw[, (recode_field_names) := .SD, .SDcols = recode_field_name_labels]


# Remove values for (effectively) missing coded data ----------------------

ohcao_data_raw[, (recode_field_names) := lapply(.SD, function(vec) {
  replace(vec, vec %in% c("Not recorded", "Not applicable", "Unobtainable", "Unknown"), NA)
}), .SDcols = recode_field_names]



# standardise field names (& remove prefixes) -----------------------------

data.table::setnames(ohcao_data_raw, make.names(tolower(sub("^clg_", "", colnames(ohcao_data_raw))), unique = TRUE))


# Remove missing / non-hospital codes -------------------------------------
# [all ambiguous values have frequency at least double of any recognisably valid hospital]

ohcao_data_raw[, hospcode_original := hospcode]

ohcao_data_raw[, hospcode := trimws(gsub("[\\s]{1,}", " ", gsub("[^A-Z0-9 ]{1,}", "", toupper(hospcode_original)), perl = TRUE))]

non_hospital_codes <- toupper(c("", "NOT CONVEYED", "9009", "XXX", "NA", "0",
                                "NOT TRANSPORTED", "9008", "ROLE",
                                "ROLE ON SCENE", "PATIENT NOT CONVEYED",
                                "UNKNOWN", "NOT APPLICABLE", "PLEASE SELECT",
                                "OTHER", "UNKNOWN X X X", "OTHER HOSPITAL",
                                "MISCELLANEOUS HOSPITAL ENTER DETAILS IN NOTES",
                                "NOT RECORDED", "OUT OF AREA"))
ohcao_data_raw[hospcode %in% non_hospital_codes, hospcode := NA]

# Remove missing data from time fields and convert to decimal hour --------
# (midnight = 0)

time_fields <- c("emstime", "stoptime")
ohcao_data_raw[, (time_fields) := lapply(.SD, function(time) {
  time <- replace(time, time == "", NA)
  (as.numeric(lubridate::fast_strptime(paste("2014-01-01", time), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London", lt = FALSE)) -
      as.numeric(lubridate::fast_strptime("2014-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London", lt = FALSE))) / 3600
}), .SDcols = time_fields]



# standardise data types --------------------------------------------------

ohcao_data_raw[, ems_month := as.Date(lubridate::fast_strptime(paste(emsdate, "12:00:00"), format = "%Y%B %H:%M:%S", tz = "Europe/London", lt = FALSE))]


# Ensure OHCAO IDs are unique ---------------------------------------------

stopifnot(ohcao_data_raw[, .N, by = ohcaoid][N != 1, .N] == 0)


# Define analysis dataset -------------------------------------------------

ohcao_data <- data.table::copy(ohcao_data_raw[, .(ohcaoid, site, ems_month, emstime, responsetime,
                                                  sex, age, imd2015decile, wit, cprlay, padused,
                                                  initrhythm, roscpreems, role, hospcode, discharged,
                                                  roschosp)])


# Save data ---------------------------------------------------------------

usethis::use_data(coding_tables, internal = TRUE, overwrite = TRUE)
