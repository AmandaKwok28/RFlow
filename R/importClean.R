#' Title
#'
#' @param file_path path to file
#' @param file_type csv or excel
#' @param remove_na bool to remove NA value
#' @param trim_ws bool to trim white spaces
#' @param convert_types bool to convert data from csv to excel file
#'
#' @return imported and cleaned data
#' @export
#'
#' @examples
#' # Import and clean a CSV file
#' cleaned_data <- importClean("path/to/data.csv", file_type = "csv")
#'
#' # Import and clean an Excel file
#' cleaned_data <- importClean("path/to/data.xlsx", file_type = "excel", remove_na = FALSE)
#'
#' # Import and clean a CSV file without trimming white space
#' cleaned_data <- importClean("path/to/data.csv", file_type = "csv", trim_ws = FALSE)
importClean <- function(file_path, file_type = c("csv", "excel"),
                        remove_na = TRUE, trim_ws = TRUE, convert_types = TRUE) {
  # Match the file type
  file_type <- match.arg(file_type)

  # Import data based on file type
  if (file_type == "csv") {
    data <- readr::read_csv(file_path)
  } else if (file_type == "excel") {
    data <- readxl::read_excel(file_path)
  } else {
    stop("Unsupported file type. Please use 'csv' or 'excel'.")
  }

  # Remove NA values
  if (remove_na) {
    data <- dplyr::drop_na(data)
  }

  # Trim white space
  if (trim_ws) {
    data <- dplyr::mutate(data, dplyr::across(dplyr::where(is.character), ~stringr::str_trim(.)))
  }

  # Convert data types
  if (convert_types) {
    data <- readr::type_convert(data)
  }

  return(data)
}
