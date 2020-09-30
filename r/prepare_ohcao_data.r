#' Prepare OHCAO data for analysis
#' @param ohcao_data_raw A data.table of raw OHCAO data.
#' @return A data.table of prepared OHCAO data.
#' @examples
#' \dontrun{
#' prepareOHCAOData(ohcao_data_raw)
#' }
#' @import data.table
#' @export
prepareOHCAOData <- function(ohcao_data_raw) {

  . = N = age = cprlay = data_coding_type = discharged = ems_month = NULL
  emsdate = emstime = field = hospcode = hospcode_original = NULL
  imd2015decile = initrhythm = ohcaoid = padused = responsetime = NULL
  role = roschosp = roscpreems = sex = site = wit = NULL

  # Attach coding labels to data --------------------------------------------

  recodes <- ref_coding_tables[["data_coding"]][substr(data_coding_type, 1, 2) == "tb" & field %in% colnames(ohcao_data_raw)]
  recode_field_names <- recodes$field
  recode_boolean_field_names <- recodes$field[recodes$data_coding_type == "tbCodeBool"]


  for(var in recode_field_names) {
    data.table::setnames(ref_coding_tables[[recodes[field == var, data_coding_type]]], paste0(var, c("", "_original")))
    ohcao_data_raw <- merge(ohcao_data_raw,
                            ref_coding_tables[[recodes[field == var, data_coding_type]]],
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


  # Convert boolean values to R native boolean type -------------------------

  ohcao_data_raw[, (recode_boolean_field_names) := lapply(.SD, function(vec) {
    return(vec == "Yes")
  }), .SDcols = recode_boolean_field_names]


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


  # Create age_group fields -------------------------------------------------

  ohcao_data_raw[, age_group := cut(age,
                                    ref_age_cut_points,
                                    right = FALSE)]

  ohcao_data_raw[, age_group := levels(age_group)[age_group]]



# recode sex as lower case ------------------------------------------------

  ohcao_data_raw[, sex := tolower(sex)]


# Identify ARP phase ------------------------------------------------------


  ohcao_data_raw <- merge(ohcao_data_raw, ref_implementation_dates, by = "site", all.x = TRUE)

  ohcao_data_raw[, ':=' (additional_triage_time = lubridate::floor_date(additional_triage_time, unit = "month"),
                         revised_call_categories = lubridate::floor_date(revised_call_categories, unit = "month") + months(1))]

  ohcao_data_raw[ems_month < additional_triage_time, ARP_phase := "pre-ARP"]
  ohcao_data_raw[ems_month >= revised_call_categories, ARP_phase := "post-ARP"]
  ohcao_data_raw[, ARP_phase := factor(ARP_phase, c("pre-ARP", "post-ARP"))]


# Recode ROLE as per OHCAO guidance ---------------------------------------

  ohcao_data_raw[(roschosp == TRUE | discharged == TRUE) & role == TRUE, role := FALSE]

# Recode respontime/crplay as per OHCAO guidance --------------------------

  ohcao_data_raw[wit == "EMS witnessed", ':=' (responsetime = 0,
                                                cprlay = "No bystander CPR")]

# Create new roschosp (It is unfeasable that a pt would be discharged alive without ROSC at hospital)

  ohcao_data_raw[, roschosp_original := roschosp]
  ohcao_data_raw[discharged == TRUE, roschosp := TRUE]


# Create three-value (+NA) version of cpr ---------------------------------

  ohcao_data_raw[wit %in% c("Bystander witnessed", "Yes") &
                   cprlay %in% c("Bystander CPR",
                                 "Subset: compression only",
                                 "Subset: compression and ventilations"),
                 witness_cpr := "Bystander witness CPR"]

  ohcao_data_raw[(!(wit %in% "EMS witnessed") &
                   cprlay == "No bystander CPR") |
                   wit == "Unwitnessed",
                 witness_cpr := "No witness CPR"]

  ohcao_data_raw[wit == "EMS witnessed",
                 witness_cpr := "EMS witness CPR"]


  # Create binary version of initrhythm -------------------------------------

  ohcao_data_raw[!is.na(initrhythm), initrhythm_shockable_binary := (initrhythm %in% c("VF/VT",
                                                                                       "AED shockable",
                                                                                       "VF",
                                                                                       "Pulseless VT"))]


# Create binary version of padused ----------------------------------------

  ohcao_data_raw[!is.na(padused), padused_binary := (padused %in% c("Yes",
                                                                    "AED used, no shock delivered",
                                                                    "AED used, shock delivered"))]


# Create binary version of witnessed --------------------------------------

  ohcao_data_raw[!is.na(wit), wit_binary :=  (wit %in% c("Bystander witnessed",
                                                         "EMS witnessed",
                                                         "Yes"))]


# Create bi-monthly "season" variable -------------------------------------

  ohcao_data_raw[!is.na(ems_month), bimonth := paste(month.abb[as.integer(ceiling(as.numeric(format(ems_month, format = "%m")) / 2)) * 2 - 1],
                                                   month.abb[as.integer(ceiling(as.numeric(format(ems_month, format = "%m")) / 2)) * 2], sep = "-")]
  ohcao_data_raw[, bimonth := factor(bimonth, paste(month.abb[1:6 * 2 - 1], month.abb[1:6 * 2], sep = "-"))]

  # Identify complete cases -------------------------------------------------

  ohcao_data_raw[, complete_case_discharged := complete.cases(site, bimonth,
                                                   initrhythm_shockable_binary,
                                                   age, sex, witness_cpr,
                                                   discharged)]

  ohcao_data_raw[, complete_case_roschosp := complete.cases(site, bimonth,
                                                            initrhythm_shockable_binary,
                                                            age, sex, witness_cpr,
                                                            roschosp)]

  # Ensure OHCAO IDs are unique ---------------------------------------------

  stopifnot(ohcao_data_raw[, .N, by = ohcaoid][N != 1, .N] == 0)


  # Define analysis dataset -------------------------------------------------

  ohcao_data <- data.table::copy(ohcao_data_raw[, .(ohcaoid,
                                                    site, ARP_phase, bimonth,
                                                    responsetime, age,
                                                    wit_binary, witness_cpr,
                                                    initrhythm_shockable_binary,
                                                    discharged, roschosp,
                                                    complete_case_discharged,
                                                    complete_case_roschosp,
                                                    age_group, roschosp_original,
                                                    ems_month, emstime,
                                                    sex, imd2015decile,
                                                    wit, cprlay, initrhythm,
                                                    padused, padused_binary,
                                                    roscpreems, role,
                                                    hospcode)])


  # Return data -------------------------------------------------------------

  return(ohcao_data)
}
