#' Generate descriptive statistics
#'
#' @param ohcao_data_file Path to RDS files of prepared OHCAO
#' data, a character vector of length one.
#' @param output_directory Path to desired output directory,
#' a character vector of length one.
#'
#' @return Nothing. On success and html file named "Report.html"
#' will be created in the the specified output directory.
#'
#' @examples
#' generateDescriptiveStats("path/to/data.rds", "path/to/output/dir/")
generateDescriptiveStats <- function(ohcao_data_file, output_directory) {
  # read in data
  ohcao_data <- readRDS("data/ohcao_data_2020-01-08.rds")
  #ohcao_data <- readRDS(ohcao_data_file)

  ggplot2::ggplot(ohcao_data, ggplot2::aes(x = ems_month, colour = site)) +
    ggplot2::geom_line(stat = "count") +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggplot2::scale_x_date(date_labels = "%Y-%b", date_breaks = "6 months")

  DataExplorer::create_report(ohcao_data)
}

