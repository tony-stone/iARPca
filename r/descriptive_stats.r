#' Generate descriptive statistics
#' @param data A data.table of prepared OHCAO data
#' @param output_directory Path to desired output directory,
#' a character vector of length one.
#' @return Nothing. On success and html file named
#' "OHCAO_data_report.html" will be created in the
#' specified output directory.
#' @examples
#' \dontrun{
#' generateDescriptiveStats(ohcao_data, "path/to/output/dir/")
#' }
#' @export
generateDescriptiveStats <- function(data, output_directory) {

  ems_month = site = NULL

  print(ggplot2::ggplot(data, ggplot2::aes(x = ems_month, colour = site)) +
          ggplot2::geom_line(stat = "count") +
          ggplot2::scale_color_brewer(palette = "Paired") +
          ggplot2::scale_x_date(date_labels = "%Y-%b", date_breaks = "6 months"))

  return(DataExplorer::plot_missing(data))
}

