
#' Track QC notes and findings
#'
#'@description
#' Retrieve findings and notes from QC program and generate QC email
#' FINDING: this is a finding or NOTE: this is a note
#' @param qc_data_path path to qc dataset. Auto loads qc_data_path if already in environment.
#' @param qc_program_path path to qc code file with comments. Default opens active tab
#' @export
#' @examples
#' \dontrun{
#' airheads()
#' }
#'
#'


airheads <- function(qc_data_path=NULL,qc_program_path=NULL){

  if(interactive()){

  options(todor_patterns = c("FINDINGS","FINDING","NOTE", "NOTES","QUESTIONS","QUESTION","TODO"))

  if (missing(qc_program_path) || is.null(qc_program_path)) {

    qc_program_path <- utilscognigen::get_source_file()
  }


  if (!exists("qc_data_path") && is.null(qc_data_path)) {
    cli::cli_abort("provide a qc_data_path
                   or have one already loaded in Global Env")
  }



  # If the object exists, use it, otherwise use the argument provided
  if (exists("qc_data_path")) {
    qc_data_path <- get(envir = .GlobalEnv,"qc_data_path")
  }

  #label <- attr(qc_data, "label")
  new_path <- glue::glue("{normalizePath(qc_data_path)}")
  mod_time <- format(file.info(qc_data_path)$mtime, "%Y-%m-%d %H:%M:%S %Z") %>%
  as.character()

  findings <- todor::todor(file = qc_program_path,output = "list") %>%
    purrr::map_dfr( ~ tibble::tibble(message = .x[["message"]])) %>%
    dplyr::filter(stringr::str_detect(message,"FIND")) %>%
    dplyr::mutate(message=gsub("\\[.*?\\]", "", message))

  findings_messages <-   findings$message %>%
    paste("<li>", ., "</li>", collapse = "\n")

  notes <- todor::todor(file= qc_program_path,output = "list") %>%
    purrr::map_dfr( ~ tibble::tibble(message = .x[["message"]])) %>%
    dplyr::filter(stringr::str_detect(message,"NOTE")) %>%
    dplyr::mutate(message=gsub("\\[.*?\\]", "", message))

  notes_messages <-   notes$message %>%
    paste("<li>", ., "</li>", collapse = "\n")

  blastula::compose_email(
    body = blastula::md(
      glue::glue(
        "
    <div style='font-family: Arial, sans-serif; font-size: 14.5px;'>
        DM Data QC has been completed as requested, details follow. Please let me know if you have any questions.
    <p><strong>Full path and file name:</strong><br>{new_path}</p>

    <p><strong>Type of analysis:</strong><br> (Insert analysis type here)</p>

    <p><strong>Studies and/or Type of data:</strong><br> (Insert studies/data type here)</p>

    <p><strong>File date/time stamp:</strong><br>{mod_time}</p>

    <p><strong>Finding(s):</strong><br> {findings_messages}</p>

	<p><strong>Note(s):</strong><br> {notes_messages}</p>
       CPP-DM-1.6_FormC DM Data QC Findings
      </div>
      "
      )
    )
  )
}
}
