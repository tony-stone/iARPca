# Prepare field code tables -----------------------------------------------

sheets <- openxlsx::getSheetNames("data-raw/OHCAO_coded_tables.xlsx")

coding_tables <- lapply(sheets, function(sheet) {
  return(data.table::data.table(openxlsx::read.xlsx("data-raw/OHCAO_coded_tables.xlsx", sheet, check.names = TRUE)))
})
names(coding_tables) <- sheets


# Save internal data ------------------------------------------------------

usethis::use_data(coding_tables, internal = TRUE, overwrite = TRUE)
