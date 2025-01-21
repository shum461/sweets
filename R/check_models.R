#' Check if modeling in underway
#'
#' @param paths to model files. Defaults to `paths=c("../monolix/","../nm/")`
#' Provide the path(s) if you're not working in asmbdat directory
#'
#' @description Sometimes it is useful for programmers to know if and when modeling has occurred
#' @return A data frame with software,output,user,min and max modification times
#'
#' @examples
#' \dontrun{
#'
#' # if working in asmbdat and looking just for monolix or nonmem, no arguments needed.
#'   check_models()
#'
#' # otherwise provide the path(s)
#' check_models(path = "../misc/sponsor/sponsor-108/123456/d1pkpd/monolix/")
#' }
#' @export
#'

check_models <- function(paths=c("../monolix/","../nm/")){


  invalid_paths <- paths[!file.exists(paths)]

  if(!sweet_asmbdat()){
    cli::cli_warn("Intended for use in admbdat directory")
  }

  if(length(invalid_paths>=1)){

    cli::cli_warn(message = "{.arg {invalid_paths}}, are invalid paths")
  }

  paths <- paths[file.exists(paths)]

  if(length(paths)>=1){
   paths %>%
    purrr::map_dfr(~fs::dir_info(.x,recurse = TRUE) %>%
              dplyr::filter( type =="file") %>%
              dplyr::mutate(output=basename(dirname(path)),
                     software=basename(.x)) %>%
                dplyr::group_by(user,output) %>%
                dplyr::mutate(
                min_modification_time=min(modification_time),
                max_modification_time=max(modification_time)) %>%
                dplyr::distinct(software,output,user,min_modification_time,max_modification_time) %>%
                dplyr::filter(!user=="root"))
}
          }
