library(data.table)
library(openxlsx)

setup_opts <- yaml::read_yaml("private.yaml")


# Prepare field code tables -----------------------------------------------

sheets <- openxlsx::getSheetNames("data-raw/OHCAO_coded_tables.xlsx")

coding_tables <- lapply(sheets, function(sheet) {
  return(data.table::data.table(openxlsx::read.xlsx("data-raw/OHCAO_coded_tables.xlsx", sheet, check.names = TRUE)))
  })
names(coding_tables) <- sheets

recodes <- coding_tables[["data_coding"]][substr(data_coding_type, 1, 2) == "tb" & field %in% colnames(ohcao_data)]

# Read in data and pre-process --------------------------------------------

# Read in data
ohcao_data <- fread(setup_opts[["data-source-filepath"]])

# Attach coding labels
for(var in recodes$field) {
  setnames(coding_tables[[recodes[field == var, data_coding_type]]], paste0(var, c("", "_label")))
  ohcao_data <- merge(ohcao_data, coding_tables[[recodes[field == var, data_coding_type]]], by = var, all.x = TRUE)
}

# stadardise field names (& remove prefixes)
setnames(ohcao_data, make.names(tolower(sub("^clg_", "", colnames(ohcao_data))), unique = TRUE))

# standardise data types
