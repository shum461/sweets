#' Flexible file reader for SAS, XPT, and CSV formats, with optional input recording
#'
#' Reads a dataset from a path, supporting \code{.sas7bdat}, \code{.xpt}, and \code{.csv} files.
#' The original file path is retained as an attribute \code{"path"} on the returned data.
#' The file modification time (\code{"mtime"}) is also set as an attribute.
#' If \code{record_input} is \code{TRUE}, records the input file path via \code{utilscognigen::record_input()}.
#'
#' @param path Character. Path to the file to read.
#' @param ... Additional arguments passed to \code{data.table::fread} (for .CSVs).
#' @param record_input Logical. If \code{TRUE}, records the input file path via \code{utilscognigen::record_input()}. Default is \code{TRUE}.
#'
#' @return A data.frame or data.table with attributes \code{"path"} and \code{"mtime"} and class \code{"sweet_read"}.
#' @examples
#' \dontrun{
#' df <- read_flexible("mydata.csv")
#' df2 <- read_flexible("mydata.csv", record_input = FALSE)
#' }
#'
#' @export

sweet_read <- function(path, ..., record_input = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }
  if (isTRUE(record_input)) {
    utilscognigen::record_input(path)
  }
  ext <- tolower(tools::file_ext(path))
  out <- switch(
    ext,
    "csv" = data.table::fread(path, ...),
    "sas7bdat" = haven::read_sas(path),
    "xpt" = haven::read_xpt(path),
    {
      cli::cli_abort("Unsupported file type: {.val {ext}}. Supported types are: csv, sas7bdat, xpt.")
    }
  )
  attr(out, "path") <- path
  attr(out, "mtime") <- file.info(path)$mtime
  attr(out, "var_labels") <- sapply(out, function(x) attr(x, "label"), USE.NAMES = TRUE)
  class(out) <- unique(c("sweet_read", class(out)))
  out
}
