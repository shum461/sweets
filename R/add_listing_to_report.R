
#' make a listing table
#'
#' @description
#' `listing_table()`
#' is used to build listing tables for each deletion data subset
#' listing tables can then be modified further using [reporter::define()] or sent to [add_listing_to_report()] where
#' listing tables are the second argument
#' @param listing Data to be used in the listing. Subset to variables that best portray why the data were deleted.
#' Selected variables should be source variables if possible
#' @param ... additional arguments passed to [reporter::create_table()]
#' @param title Title to describe each unique listing in the report. Usually closely resembles DELFNC
#' @return A deletion report object that includes all added listings and their titles of class "table_spec" & "list"
#' @export
#' @examples
#' \dontrun{
#'
#'  # Add tables to the report
#'
#'del_report <- del_report %>%
#'  add_listing_to_report(
#'    listing_table(
#'      pk_delimp_mod  %>%
#'        filter(DELFN == 5, DVID == 2) %>%
#'        arrange(STUDYID, USUBJID, DTTM) %>%
#'        select(ROW,USUBJID, VISIT, PCTPT, PCTESTCD, PCSTAT),
#'      "Compound A (ng/mL), Sample Not Done / Not Recorded"
#'    ) %>%
#'      define(ROW, label = " ", width = 0.15, id_var = TRUE, align = "left")
#'  )  %>%
#'  add_listing_to_report(
#'    listing_table(
#'      pk_delimp_mod %>%
#'        filter(DELFN == 5, DVID == 3) %>%
#'        arrange(STUDYID, USUBJID, DTTM) %>%
#'        select(ROW,USUBJID, VISIT, PCTPT, PCTESTCD, PCSTAT),
#'      "Compound B (ng/mL), Sample Not Done / Not Recorded"
#'    ))
#'
#'
#'}

listing_table <- function(listing,title,...){

  if(missing(title)){
    cli::cli_abort(text = "please provide a title for your listing table")
  }


  reporter::create_table(listing,
                         borders = "none",
                         use_attributes=c("format","label"),
                         headerless = FALSE,
                         continuous = TRUE,
                         first_row_blank = FALSE,...) %>%
    reporter::titles(width="page",align = "left",title)

}



#' Add each listing to the initialized report
#'
#' @description
#' `add_listing_to_report()`
#' is used to build listings one deletion flag at a time
#'
#' @param deletion_report Name of initialized deletion report made with [build_report()]
#' @param listing Data to be used in the listing. Subset to variables that best portray why the data were deleted.
#' Selected variables should be source variables if possible
#' @param ... additional arguments passed to [reporter::create_table()]
#' @return A deletion report object that includes all added listings and their titles
#' @export
#' @examples
#' \dontrun{
#' # A deletion report with DELFN 1 and 9 added
#'
#'
#'  add_listing_to_report(
#'    listing = pd_delimp %>%
#'      filter(DELFN == 9) %>%
#'      select(STUDY, USUBJID, VISIT, DV, SCF),
#'    title = "Missing Concentration Value"
#'  )
#'}
#'
#'
#'


add_listing_to_report <- function(deletion_report,listing,...){

  assertthat::assert_that(!purrr::is_null(deletion_report),
                          msg = "Build deletion report first")

  # TODO add row count -- all equal?
  # Warning--Check for duplicated table titles
  # make sure you're not duplicating tables in listings
  existing_titles <- purrr::map_chr(which(seq_along(deletion_report$content)%%2==1),
                                    ~deletion_report$content[[.x]]$object$titles[[1]][[1]])


  title <- listing$titles[[1]]$titles

  if(title %in% existing_titles){
    cli::cli_warn(c("Table {.val {title}} already exists in listings report",
                    "1. Verify data have not been added",
                    "2. Consider changing the title"))
  }

  # early return
  if(nrow(listing$data)==0){
    cli::cli_inform(c("i"="{.val {title}} has 0 rows and will be ignored"))
    return(deletion_report)
  }

  # broke out the create_table so that users can customize
  # individual tables and variables more easily using reporter::define()
  new_table <- listing


  # Row counts
  row_count <- reporter::create_text(paste("",paste0("N=",nrow(listing$data)),sep="\n"),
                                     width = NULL, align = "left",
                                     borders = "none")
  # Deletion listings
  updated_report <- reporter::add_content(x = deletion_report,new_table,
                                          align = "left",
                                          blank_row = "none")
  # Add row counts below table
  updated_report2 <- reporter::add_content(x = updated_report,row_count,
                                           align = "left",
                                           blank_row = "none")

  #TODO why won't the deletion_report play nicely inside cli::alert
  cli::cli_inform(c("v"="Listing added to report.",
                    "i"="There are now {.val {length(updated_report2$content)/2}} listings total.",
                    "i"="New listing attributes:"))
  #cli::cli_h1()
  # reporter:::print.table_spec
  print(new_table)

  invisible(updated_report2)

}
