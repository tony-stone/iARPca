source("r/prepare_ohcao_data.r")
source("r/descriptive_stats.r")
load("r/sysdata.rda")


# Read setup options ------------------------------------------------------

setup_opts = yaml::read_yaml("private.yaml")


# Read in data ------------------------------------------------------------

ohcao_data_raw <- data.table::fread(setup_opts[["data-source-filepath"]])

# Prepare data and save ---------------------------------------------------

ohcao_data <- prepareOHCAOData(ohcao_data_raw)

todays_date <- format(Sys.Date(), format = "%Y-%m-%d")
dir_name <- paste0("data/ohcao_data_", todays_date)

if(!dir.exists(dir_name)) {
  dir.create(dir_name)
}

saveRDS(ohcao_data, file = paste0(dir_name,
                                  "/ohcao_data_",
                                  todays_date,
                                  ".rds"))

data.table::fwrite(ohcao_data, file = paste0(dir_name,
                                            "/ohcao_data_",
                                            todays_date,
                                            ".csv"))


# Read in prepared OHCAO data ---------------------------------------------

data_version <- "2020-02-06"
ohcao_data <- readRDS(paste0("data/ohcao_data_", data_version, "/ohcao_data_", data_version, ".rds"))


# Run Descriptive Stats ---------------------------------------------------

generateDescriptiveStats(ohcao_data, paste0("data/ohcao_data_", data_version))

ohcao_data[, .(outcome_discharged = sum(discharged %in% "Yes") / sum(discharged %in% c("Yes", "No")),
               outcome_rosc = sum(roschosp %in% "Yes") / sum(roschosp %in% c("Yes", "No"))),
           by = ARP_phase]

outcomes_wide <- ohcao_data[, .(outcome_discharged = sum(discharged %in% "Yes") / sum(discharged %in% c("Yes", "No")),
                           outcome_rosc = sum(roschosp %in% "Yes") / sum(roschosp %in% c("Yes", "No")),
                           N = max(sum(roschosp %in% c("Yes", "No")), sum(discharged %in% c("Yes", "No")))),
                       by = .(ems_month, site)]

outcomes <- melt(outcomes_wide, id.vars = c("site", "ems_month", "N"), variable.name = "measure", value.name = "rate", variable.factor = FALSE)

ggplot(outcomes[N > 100], aes(x = ems_month, y = rate, linetype = measure, colour = site)) +
         geom_line() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_colour_brewer( palette = "Paired") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~site)

