#' sweet sponsor
#'
#' Checks if you're working in a client directory
#' @return TRUE or FALSE
#' @export
#' @examples
#' \dontrun{
#' sweet_sponsor()
#' }

sweet_sponsor <- function(){

  tryCatch(
    {utilscognigen::path_sponsor()
      TRUE},
    error=function(e){
      FALSE
    })
}

#' sweet asmbdat
#'
#' Checks if you're working in asmbdat directory
#' @return TRUE or FALSE
#' @export
#' @examples
#' \dontrun{
#' sweet_asmbdat()
#' }

sweet_asmbdat <- function(){
  return(basename(getwd())=="asmbdat")
}


#' sour paths
#'
#' a table of file paths ready to be pasted into an email
#'
#' @param path The directory path to pull from. Default is \code{'../data/'}
#' @param full.names Default is TRUE
#' @param ... additional arguments passed to list.files()
#'
#' @return a html DT::datatable() with a 'Copy' button
#' @export
#'
#' @examples
#' \dontrun{
#' sourpaths(path="data/",pattern="pk")
#' }

sourpaths <- function(full.names=TRUE,path=NULL,
                      ...) {


  if(missing(path) || is.null(path)){
  if(sweet_asmbdat()){
    path <- "../data/"
  } else {
    path <- getwd()
  }
}


 df <-  list.files(path=path,full.names=full.names,
             ...) %>%
    normalizePath() %>%
    data.frame()


  if(!nrow(df)>=1){
   cli::cli_alert_info(text = "no filepaths found")
  }


 if(nrow(df)>=1){
  df %>%
   dplyr::bind_rows(data.frame(.=""),.)%>%
    DT::datatable(
      colnames = "The following datasets have been created as requested:",
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip', buttons = list(
        list(
          extend = 'copy',
          text = 'Copy',
          title = NULL,
          footer = FALSE,
          exportoptions =
            list(
              stripHtml = TRUE,
              columns =
                ':visible',
              modifier =
                list(page = 'all')
            )
        )
      ))
    )

 }
    }







#' most recent
#'
#' @param path
#' @param pattern one or more regex patters e.g. c("dm","lb")
#'
#' @return
#' @export
#'
#' @examples
#'


most_recent <- function(path=NULL,pattern=NULL,newest_only=TRUE){


  if(!sweet_asmbdat()){
    cli::cli_warn("Intended for use in 'asmbdat' directory")
  }


  if(missing(path) || is.null(path)){
    if(sweet_asmbdat()){
      path <- "../../../dataorig/"
    } else {
      path <- getwd()
    }
  }



  if(missing(pattern) || is.null(pattern)){
    paths <-fs::dir_ls(path =path,fail=FALSE,recurse = TRUE) %>%
       purrr::map_dfr(~fs::file_info(.x))
    } else {
      paths <- pattern %>%
        purrr::map_dfr(~fs::file_info(fs::dir_ls(path=path,fail=FALSE,recurse = TRUE,regex=.x)))

  }

  if(!nrow(paths)>=1){
    cli::cli_abort(message = "no file paths found matching pattern {.arg {pattern}}")
  }


  if(nrow(paths)>=1){
    paths_table <- paths %>%
      dplyr::mutate(short=tools::file_path_sans_ext(basename(path)),
           short_date=basename(dirname(path))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(short) %>%
      dplyr::mutate(newest=ifelse(modification_time==max(modification_time),1,0))%>%
      dplyr::arrange(short,desc(modification_time)) %>%
    dplyr::select(short,newest,short_date,modification_time,path)
  }

  if(newest_only){
    paths_table <- paths_table %>%
      dplyr::filter(newest==1) %>%
      dplyr::select(-newest)
  }


  return(paths_table)


}
