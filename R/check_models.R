

check_models <- function(paths=c("../monolix/","../nm/")){


  invalid_paths <- paths[!file.exists(paths)]

  if(length(invalid_paths>1))

    purrr::map_dfr(~fs::dir_info(.x,recurse = TRUE) %>%
              dplyr::filter( type =="file") %>%
              dplyr::mutate(output=basename(dirname(path)),
                     software=basename(.x)) %>%
                dplyr::group_by(user,output) %>%
                dplyr::mutate(
                min_modification_time=min(modification_time),
                max_modification_time=max(modification_time)) %>%
                dplyr::distinct(software,output,user,min_modification_time,max_modification_time) %>%
                dplyr::filter(!user=="root")

              )}
