#' Create Directories
#'
#' @param paths Vector of full file path you would like to request
#'               e.g. paste0("/doc/bms/nivolumab/100164/",c("d1pk","d1er-eff","d1er-saf")
#' @param ask           Logical indicating whether to interactively ask for input. This is particularly helpful for file paths including single backslashes (Windows paths).
#' @param multi         Is this a multi-drug project. This will apply to all paths provided, but there are checks for each path processed
#' @param descriptor    The <descriptor> should reflect the known scope of work (for example, 'multidrug', 'consulting', 'biomarker').
#' @param stddir        For multidrug projects, would you like to create the standard directories under the project number
#' @param send.to.term  Do you want to sent the compiled commands to create the requested directories to your terminal?
#' @param activate.term Do you want to activate the terminal to which the compiled commands were sent?
#' @param sleep         How many seconds to wait before activating the terminal window. Will be converted to a positive integer.
#' @return a  directory
#' @examples
#'
#'
#'
#'



# Code that Would be Nice to Make Work ------------------------------------


# The showPrompt does not process newline (\n) nor HTML paragraph markers (<p>)
# appropriately in the displayed text box.
# rstudioapi::showQuestion allows for newline (\n), but only accepts an 'OK' (TRUE)
#                          or 'Cancel' (FALSE) response.
# rstudioapi::showDialog allows for HTML paragraph markers (<p>), but is simply a
#                        message box that does not return a response value
#
# Using rstudioapi::showPrompt would be preferred since it would not show the dialogue
# in the Console and would result in a cleaner result.
#
# An alternate method where the final text to display is collected for each value
# in the loop and then displayed at terminus has been employed.

# result <- rstudioapi::showPrompt(title = "Would you like to change the multi parameter to TRUE?",
#                                  message = paste0("Please enter the numeric response.\n",
#                                                   "<p>  1: Yes</p>",
#                                                   "<p>  2: No</p>",
#                                                   "<p>  3: Next path, if one is available</p>",
#                                                   "  4: Exit")) %>%
#   toupper()
#
# rstudioapi::showQuestion(title = "Would you like to change the multi parameter to TRUE?",
#                          message = paste0("Please enter the numeric response.\n",
#                                           "\n  1: Yes</p>",
#                                           "\n  2: No</p>",
#                                           "\n  3: Next path, if one is available</p>",
#                                           "  4: Exit")) %>%
#   toupper()
#
# rstudioapi::showDialog(title = "Would you like to change the multi parameter to TRUE?",
#                        message = paste0("Please enter the numeric response.\n",
#                                         "\n  1: Yes</p>",
#                                         "\n  2: No</p>",
#                                         "\n  3: Next path, if one is available</p>",
#                                         "  4: Exit"))

# What is this Code? ------------------------------------------------------
# see if the drug directory exists
# JN 2023-03-09
# No idea what the following section does, if the drugdir does not currently exist,
# the code in update_drug will never return anything
# if (!dir.exists(drugdir)) {
#   # get list of directories at a specified depth from the provided path
#   list.dirs.depth.n <- function(p, n) {
#     res <- list.dirs(p, recursive = FALSE)
#     if (n > 1) {
#       add <- list.dirs.depth.n(res, n-1)
#       c(res, add)
#     } else {
#       res
#     }
#   }
#
#   update_drug <-
#     suppressWarnings(
#       unlist(
#         stringr::str_split(
#           list.dirs.depth.n(sponsordir, n = path_length - 4)[stringr::str_detect(
#                                                               list.dirs.depth.n(sponsordir, n = path_length - 4),
#                                                               stringr::fixed(drugdir, ignore_case = TRUE)
#                                                               )],
#           pattern = "/"))[4])
#
#   if(all(is.na(update_drug))){
#     message("\nThe drug directory provided does not exist.")
#   } else {
#     path <- unlist(stringr::str_split(path, pattern = "/"))
#     path[4] <- update_drug
#     path <- paste0(path, collapse = "/")
#   }
# }


# Continue ----------------------------------------------------------------

# for testing
# paths <- c("/misc/fake/acanthus/","/doc/fake2/acanthus/123456/d1pk/fakedir","/doc/merck/anacetrapib/002480/d1pk/")
# paths <- c("/misc/fake/","/doc/fake2/acanthus/123456/d1pk/fakedir","test/merck/anacetrapib/002480/d1pk/")
#
#
# test_paths <- c("/misc/fake/acanthus/",
#            "/doc/fake2/acanthus/123456/d1pk/fakedir",
#            "/doc/fake2/acanthus/123456/d1pk/fakedir",
#            "/doc/fake2/acanthus/123456/d1pkpd",
#            "/doc/fake2/acanthus/123456/d1pk/", # this will NOT match as a substring of the previous path b/c a trailing "/" will be added
#            "/doc/fake2/acanthus/123", # this will match as a substring of other paths
#            "/doc/fake2/acanthus/1234567/d1pk/fakedir",
#            "/doc/fake2/acanthus/12345678/d1pk/fakedir",
#            "/misc/akebia/anacetrapib/002480/d1pk/",
#            "/misc/akebia/anacetrapib/002481/d1pk/"
#            )

# paths        : Vector of full file path you would like to request
#                ex: paste0("/doc/bms/nivolumab/100164/",c("d1pk","d1er-eff","d1er-saf"))
# ask          : Logical indicating whether to interactively ask for input. This is particularly helpful for file paths including single backslashes (Windows paths).
# multi        : Is this a multi-drug project. This will apply to all paths provided, but there are checks for each path processed
# descriptor   : The <descriptor> should reflect the known scope of work (for example, 'multidrug', 'consulting', 'biomarker').
# stddir       : For multidrug projects, would you like to create the standard directories under the project number
# send.to.term : Do you want to sent the compiled commands to create the requested directories to your terminal?
# activate.term: Do you want to activate the terminal to which the compiled commands were sent?
# sleep        : How many seconds to wait before activating the terminal window. Will be converted to a positive integer.
orange_julius <- function(paths){


                      # ask = FALSE,
                      # multi = FALSE,
                      # descriptor = "consulting",
                      # stddir = FALSE,
                      # send.to.term = TRUE,
                      # activate.term = TRUE,
                      # sleep = 5) {


  args <- ls()
  supplied_args <- names(as.list(match.call())[-1])


  # paths not provided
  if (any(args[!args %in% supplied_args]=="paths")) {
    cli::cli_abort("No file paths provided to {.arg paths}")
  }

cli::cli_alert_info("Prepping the Requested {.val {length(paths)}} File Paths ")
cli::cli_progress_bar("File Paths", total = 100, clear = FALSE)




  # message("Prepping the Requested File Paths\n",
  #         paste0(rep("=", 63), collapse = ""), "\n")

  nopaths <- function() {
    if (length(paths) == 0) {
      cli::cli_abort("There are no valid file paths remaining")
    }
  }

   # convert sleep to a postive integer

  #sleep <- as.integer(abs(sleep))

  # Prep: All Paths ---------------------------------------------------------





# Clean paths -------------------------------------------------------------


# remove all white space as file paths should not have any
paths <- utilscognigen:::clean_path(gsub(" ", "", paths))
# convert all paths to unix, add leading "/"
paths <- utilscognigen:::clean_path(paste0("/", utilscognigen::path_to_unix(paths, normalize = FALSE)))
# normalize paths and only keep unique values
paths <- unique(normalizePath(paths, mustWork = FALSE))


# Drop missing doc or misc prefix -----------------------------------------

# garbage paths are paths that don't start with misc or doc
garbage_paths <- paths %>%
  str_subset(pattern = "^/doc|^/misc",negate = TRUE)

# count and print garbage paths
if(length(garbage_paths)>=0){

  cli::cli_alert_warning("Paths must start with /doc or /misc.
                         Dropping {.val {length(garbage_paths)}} invalid paths
                       {.file {garbage_paths}}")

# subset paths to keep valid paths
paths <- paths %>%
   str_subset(pattern = "^/doc|^/misc")

# only show remaining path count if some were dropped
if(length(garbage_paths)>=0){
cli::cli_alert_info("{.val {length(paths)}} file paths remaining")
}


nopaths()

cli::cli_progress_update()
} # {------------- END FUNCTION ----------------------}


  # if (is.null(paths) | ask) {
  #   readlinepaths <- readline("Enter the full file paths, comma-separated:")
  #   # readlinepaths <- rstudioapi::showPrompt(title = "Additional Paths",
  #   #                                         message = "Enter the full file paths, comma-separated:",
  #   #                                         default = " ") %>% trimws()
  #   paths <- c(paths, readlinepaths)
  #   if (all(paths == "")) {
  #     paths <- NULL
  #     stop("No file paths were entered.\n", call. = FALSE)
  #   }
  # }



  nopaths()

  cli::cli_progress_update()


  # must start with leading /
  # lead_slashes <- sapply(fs::path_split(paths), '[', 1)
  # if (any(lead_slashes != "/")) {
  #   stop("The following requested paths do not start with a '/':\n  ",
  #        paste0(paths[which(lead_slashes != "/")], collapse = "\n  "),"\n",
  #        call. = FALSE)
  # }
  # must be /doc or /misc
  doc_miscs    <- sapply(fs::path_split(paths), '[', 2)
  if (any(!(doc_miscs %in% c("doc", "misc")))) {
    warning("The following paths are invalid and will be removed for the remainder of the script.\n",
            "  The paths must start with /doc or /misc.\n  ",
            paste0(paths[which(!(doc_miscs %in% c("doc", "misc")))], collapse = "\n  "),"\n",
            call. = FALSE, immediate. = TRUE)

    paths <- paths[which(doc_miscs %in% c("doc", "misc"))]
    nopaths()
  }

  # update paths, change all "misc" to "doc", get new unique paths
  paths <- unique(stringr::str_replace(normalizePath(paths, mustWork = FALSE), "/misc/", "/doc/"))

  # remove paths that are subsets of other paths, no need to repeat scripts when they will be made for the longest match
  pathcounts <- unlist(lapply(paths, function(x) {sum(grepl(x, paths))}) %>% invisible())
  if (any(pathcounts > 1)) {
    warning("The following paths are substrings of other requested paths and will be removed for the remainder of the script.\n  ",
            paste0(paths[which(pathcounts > 1)], collapse = "\n  "),"\n")
    paths <- paths[which(pathcounts == 1)]
  }

  # all project numbers must be 6 or 8 digits long
  proj_lengths <- nchar(sapply(fs::path_split(paths), '[', 5))
  if (any(!(is.na(proj_lengths) | proj_lengths %in% c(6, 8)))) {
    warning("The project numbers in the following paths are not 6 or 8 digits and will be removed for the remainder of the script.\n  ",
            paste0(paths[which(!(is.na(proj_lengths) | proj_lengths %in% c(6, 8)))],
                   collapse = "\n  "),"\n")
    paths <- paths[which((is.na(proj_lengths) | proj_lengths %in% c(6, 8)))]
  }

  path_lists <- fs::path_split(paths)
  path_lengths <- lengths(fs::path_split(paths))

  # subset the paths to not be longer than the stage level
  if (any(path_lengths > 6)) {
    message("The following requested file paths will only be created through the stage level.\n  ",
            paste0(paths[which(path_lengths > 6)], collapse = "\n  "),"\n")

    paths <- unique(lapply(1:length(paths),
                           FUN = function(.pln) {
                             fs::path_join(path_lists[[.pln]][1:min(path_lengths[.pln], 6)])
                           }) %>%
                      invisible())

    path_lists <- fs::path_split(paths)
    path_lengths <- lengths(fs::path_split(paths))
  }

  # see if the requested sponsor directories already exist
  sponsordirs <- lapply(path_lists, FUN = function(x) {fs::path_join(x[1:3])}) %>% invisible()
  sponsordirs.exist <- dir.exists(unlist(sponsordirs))
  if (any(!sponsordirs.exist)) {
    warning("The following sponsor directories do not exist. Please contact the helpdesk to have them created.\n  ",
            paste0(unique(sponsordirs[which(!sponsordirs.exist)]), collapse = "\n  "),
            "\n\n",
            "  The following requested paths will be removed for the remainder of the script:\n    ",
            paste0(paths[which(!sponsordirs.exist)], collapse = "\n    "),"\n",
            call. = FALSE, immediate. = TRUE)

    paths <- paths[which(sponsordirs.exist)]
    nopaths()

    sponsordirs <- sponsordirs[which(sponsordirs.exist)]
    path_lists <- fs::path_split(paths)
    path_lengths <- lengths(fs::path_split(paths))
  }

  # the requested paths must be to at least the drug level
  path_lengths <- lengths(fs::path_split(paths))
  if (any(path_lengths < 4)) {
    warning("The following paths are invalid and will be removed for the remainder of the script.\n",
            "  The file paths need to include /misc/sponsor/drug, at minimum.\n  ",
            paste0(paths[which(path_lengths < 4)], collapse = "\n  "),"\n",
            call. = FALSE, immediate. = TRUE)

    paths <- paths[which(path_lengths >= 4)]
    nopaths()
  }

  drugdirs    <- lapply(path_lists, FUN = function(x) {fs::path_join(x[1:4])}) %>% invisible()
  projdirs    <- lapply(path_lists, FUN = function(x) {fs::path_join(x[1:5])}) %>% invisible()
  stagedirs   <- lapply(path_lists, FUN = function(x) {fs::path_join(x[1:6])}) %>% invisible()

  sponsors     <- basename(unlist(sponsordirs))
  drugs        <- basename(unlist(drugdirs))
  projects     <- basename(unlist(projdirs))
  stages       <- basename(unlist(stagedirs))

  # get the list of all directories with the requested project numbers
  allreqprojpaths <- stringr::str_replace(normalizePath(Sys.glob(file.path("/doc/*/*",projects)), mustWork = FALSE), "/misc/", "/doc/")
  # remove concepts folders
  allreqprojpaths <- allreqprojpaths[which(!(sapply(fs::path_split(allreqprojpaths), '[', 4) %in% c("concepts")))]


  # Check if a user is part of the datalib group ----
  username <- system("whoami", intern = TRUE)
  datalib_users <- stringr::word(system("getent group datalib;", intern = TRUE), 4L, sep = ":")
  isdatlibuser <- grepl(username, datalib_users)
  do.not.activate.terminal <- FALSE

  # initialize the object which will collect the messages to be displayed at the end of the loop
  combined_message_to_display <- vector(mode = "list", length = length(paths))

  # initialize the object which will collect the UNIX commands to make the desired directory structures
  mkcmds <- vector(mode = "list", length = length(paths))

  message("Looping Through the Valid File Paths\n",
          paste0(rep("=", 63), collapse = ""), "\n")


  # Loop Through the Remaining Paths ----
  for (.pathn in 1:length(paths)) {
    # initialize mk* objects
    path <- unlist(paths[.pathn])
    mkdrug <- NULL
    mkconsultgen <- NULL
    mkconsultproj <- NULL
    mkproj <- NULL
    mkstddir <- NULL
    update_readme <- NULL
    mkstage <- NULL
    .multi <- multi

    path_list <- unlist(path_lists[.pathn])
    path_length <- unlist(path_lengths[.pathn])

    sponsordir <- unlist(sponsordirs[.pathn])
    drugdir    <- unlist(drugdirs[.pathn])
    projdir    <- unlist(projdirs[.pathn])
    stagedir   <- unlist(stagedirs[.pathn])

    sponsor    <- unlist(sponsors[.pathn])
    drug       <- unlist(drugs[.pathn])
    proj       <- unlist(projects[.pathn])
    proj_nchar <- nchar(proj)
    stage      <- unlist(stages[.pathn])

    message("Assessing Valid File Path: ", path, "\n",
            paste0(rep("=", 63), collapse = ""), "\n")

    if (!dir.exists(drugdir)) {
      mkdrug <- paste0(paste("mkdrug", drug, sponsordir, sep = " "), ";")
    }

    # checks performed if the project directory is in the requested file path
    if (path_length > 4) {

      allprojpaths <- allreqprojpaths[grepl(proj, allreqprojpaths)]
      allprojsponsors <- sapply(fs::path_split(allprojpaths), '[', 3)
      other.drug.projpaths <- stringr::str_replace(utilscognigen::path_project(project_number = proj), "/misc/", "/doc/")
      other.nondrug.projpaths <- allprojpaths[which(!(allprojpaths %in% other.drug.projpaths))]

      # Does the requested project number exist under a different sponsor?
      if (any(allprojsponsors != sponsor)) {
        warning("The project number for the requested file path ", path, ", already exists under different sponsors.\n",
                "  Other sponsor directories:\n    ",paste0(allprojpaths[which(allprojsponsors != sponsor)], collapse = "\n    "),
                call. = FALSE,
                immediate. = TRUE)
        Sys.sleep(3)

        seekANSWER <- function() {

          result <- NA

          valid.selection <- c(1:3, "EXIT")

          while (!(result %in% valid.selection)) {
            result <- readline(cat("Would you like to proceed with the creation of the directories using the sponsor in the requested path?\n",
                                   "Please enter the numeric response.\n",
                                   "  1: Yes\n",
                                   "  2: No\n",
                                   "  3: Exit")) %>%
              toupper()

            if (result %in% c(3, "EXIT")) {
              stop("User requested exit from script.", call. = FALSE)
            }

            if (!(result %in% valid.selection)) {
              warning(result," is an invalid selection. Please reselect.", call. = FALSE, immediate. = TRUE)
            }
          }

          return(result)
        }
        switch(seekANSWER(),
               '1' = {message("Continuing...")
                 # only keep other.nondrug.projpaths from the requested sponsor
                 other.nondrug.projpaths <- other.nondrug.projpaths[which(sapply(fs::path_split(other.nondrug.projpaths), '[', 3) == sponsor)]
               },
               '2' = {message("No mk* commands will be created for ", path, ".")
                 break
               }
        )
      }

      # construct possible 'consulting' directories
      consultdir <- file.path(sponsordir, "consult_gen")
      consultingdir <- file.path(sponsordir, "consulting")
      descriptordir <- file.path(sponsordir, descriptor)
      consultdirs <- unique(c(consultdir, consultingdir, descriptordir, dirname(other.nondrug.projpaths)))
      exist.consultdirs <- consultdirs[which(dir.exists(consultdirs))]
      exist.consultdirs.length <- length(exist.consultdirs)

      # construct 'consulting/project' directories
      consultprojdirs <- unique(c(file.path(c(consultdir, consultingdir, descriptordir),proj), other.nondrug.projpaths))
      exist.consultprojdirs <- consultprojdirs[which(dir.exists(consultprojdirs))]

      # if .multi=FALSE, see if a consult*/proj directory already exists
      # if it does, switch .multi to TRUE
      if (!.multi & length(exist.consultprojdirs) > 0) {
        warning("\nThe multi parameter was set to FALSE, but the following non-drug directory(ies) exists for the, ",proj," project.\n",
                "    ", paste0(exist.consultprojdirs, collapse = "\n    "), call. = FALSE, immediate. = TRUE)

        seekANSWER <- function() {

          result <- NA

          valid.selection <- c(1:4, "EXIT")

          while (!(result %in% valid.selection)) {
            result <- readline(cat("Would you like to change the multi parameter to TRUE?\n",
                                   "Please enter the numeric response.\n",
                                   "  1: Yes\n",
                                   "  2: No\n",
                                   "  3: Next path, if one is available\n",
                                   "  4: Exit")) %>%
              toupper()

            if (result %in% c(4, "EXIT")) {
              stop("User requested exit from script.", call. = FALSE)
            }

            if (!(result %in% valid.selection)) {
              warning(result," is an invalid selection. Please reselect.", call. = FALSE, immediate. = TRUE)
            }
          }

          return(result)
        }
        switch(seekANSWER(),
               '1' = {.multi <- TRUE},
               '2' = {.multi <- FALSE},
               '3' = {message("No mk* commands will be created for ", path, ".")
                 break
               }
        )
      }

      # if .multi=FALSE, see if other-drug/proj directory already exists
      # if it does, switch .multi to TRUE
      # remove other.drug.projpaths where the drug is the requested drug
      other.drug.projpaths <- other.drug.projpaths[which(!(sapply(fs::path_split(other.drug.projpaths), '[', 4) == drug))]
      if (!.multi & length(other.drug.projpaths) > 0) {
        warning("\nThe multi parameter was set to FALSE, but the following directory(ies) exists for the, ",proj," project in other drugs.\n",
                "    ", paste0(other.drug.projpaths, collapse = "\n    "), call. = FALSE, immediate. = TRUE)
        Sys.sleep(3)

        seekANSWER <- function() {

          result <- NA

          valid.selection <- c(1:4, "EXIT")

          while (!(result %in% valid.selection)) {
            result <- readline(cat("Would you like to change the multi parameter to TRUE?\n",
                                   "Please enter the numeric response.\n",
                                   "  1: Yes\n",
                                   "  2: No\n",
                                   "  3: Next path, if one is available\n",
                                   "  4: Exit")) %>%
              toupper()

            if (result %in% c(4, "EXIT")) {
              stop("User requested exit from script.", call. = FALSE)
            }

            if (!(result %in% valid.selection)) {
              warning(result," is an invalid selection. Please reselect.", call. = FALSE, immediate. = TRUE)
            }
          }

          return(result)
        }
        switch(seekANSWER(),
               '1' = {.multi <- TRUE},
               '2' = {.multi <- FALSE},
               '3' = {message("No mk* commands will be created for ", path, ".")
                 break
               }
        )
      }

      # multidrug checks
      if (.multi) {

        switch(as.character(min(which(c(
          # 1: The descriptor directory exists or no "non-drug" directories exist
          (dir.exists(descriptordir) | exist.consultdirs.length == 0),
          # 2: The descriptor directory does not exist, but other "non-drug directories do
          # (exist.consultdirs.length > 0),
          (!dir.exists(descriptordir))
        )),
        # 999: any other condition that was not accounted for
        999)),
        # 1: The descriptory directory will be used
        '1' = {consultdir <- descriptordir},
        # 2: The user chooses from the existing "non-drug" directories and the descriptor directory
        '2' = {seekANSWER <- function() {

          result <- NA

          valid.selection <- c(1:(exist.consultdirs.length + 2), "EXIT")

          while (!(result %in% valid.selection)) {
            result <- readline(cat("Consulting directories currently exist with a different name than the requested 'descriptor' directory.\n",
                                   "  Requested 'descriptor' directory: ",descriptordir,"\n",
                                   "Which directory would you like to use as the 'descriptor' directory?\n",
                                   "Please enter the numeric response.\n ",
                                   paste0(paste0(1:(exist.consultdirs.length + 1),": "), c(exist.consultdirs, descriptordir), collapse = "\n  "),
                                   paste0("\n  ",exist.consultdirs.length + 2,": Exit"),
                                   "\n")) %>%
              toupper()


            if (result %in% c(exist.consultdirs.length + 2, "EXIT")) {
              stop("User requested exit from script.")
            }

            if (!(result %in% valid.selection)) {
              warning(result," is an invalid selection. Please reselect.", call. = FALSE, immediate. = TRUE)
            }
          }
          return(as.numeric(result))
        }
        consultdir <- c(exist.consultdirs, descriptordir)[seekANSWER()]
        # remove the selected directory from exist.consultdirs if the descriptordir is not selected
        exist.consultdirs <- exist.consultdirs[which(exist.consultdirs != consultdir)]
        exist.consultdirs.length <- length(exist.consultdirs)
        },
        # 999 Exit the script
        '999' = {stop("Logic was not setup for this current combniation of consulting/descriptor directories.")}
        )

        # messages/warnings
        switch(as.character(min(which(c(
          # 1: The descriptor directory exists and so do other "non-drug directories
          (dir.exists(descriptordir) & exist.consultdirs.length > 0 & any(exist.consultdirs != descriptordir)),
          # 2: The descriptor directory does not exist, but other "non-drug directories do
          (!dir.exists(descriptordir) & exist.consultdirs.length > 0)
        )),
        # 999: any other condition that was not accounted for
        999)),
        # 1: list other "non-drug" directories
        '1' = {msg.ecpdirs <- exist.consultprojdirs[which(dirname(exist.consultprojdirs) != descriptordir)]
        msg.ecdirs <- exist.consultdirs[which(exist.consultdirs != descriptordir & !(exist.consultdirs %in% dirname(msg.ecpdirs)))]
        msg.dirs <- sort(c(msg.ecpdirs, msg.ecdirs))
        message("NOTE: The requested 'descriptor' directory already exists and there are other existing 'non-drug' directories.\n",
                "  Requested 'descriptor' directory: ",descriptordir,"\n",
                "  Additional existing 'non-drug directories:\n  ",
                paste0(msg.dirs, collapse = "\n  "),"\n")
        },
        # 2: list other "non-drug" directories than was selected
        '2' = {msg.ecpdirs <- exist.consultprojdirs[which(dirname(exist.consultprojdirs) != consultdir)]
        msg.ecdirs <- exist.consultdirs[which(exist.consultdirs != consultdir & !(exist.consultdirs %in% dirname(msg.ecpdirs)))]
        msg.dirs <- sort(c(msg.ecpdirs, msg.ecdirs))
        message("NOTE: The requested 'descriptor' directory does not exist and there are other existing 'non-drug' directories.\n",
                "  Requested 'descriptor' directory: ",descriptordir,"\n",
                ifelse(consultdir != descriptordir,c("  Selected 'descriptor' directory: ",consultdir,"\n",),""),
                "  Additional existing 'non-drug directories:\n  ",
                paste0(msg.dirs, collapse = "\n  "),"\n")
        },
        '999' = {})

        consultdir_bn <- basename(consultdir)

        # make the multi-drug version of the 'descriptor' directory, if needed
        if (!dir.exists(consultdir)) {
          mkconsultgen <- paste0(paste("mkdrug -m", consultdir_bn, sponsordir, sep = " "), ";")
        }

        consultprojdir <- consultprojdirs[which(dirname(consultprojdirs) == consultdir)]

        # make the multi-drug version of the 'descriptor'/project directory, if needed
        if (!dir.exists(consultprojdir)) {
          mkconsultproj <- paste0(paste("mkproj -m",
                                        proj,
                                        consultdir,
                                        sep = " "), ";")
        }

      }

      if (!dir.exists(projdir)) {
        mkproj <- paste0(ifelse(!is.null(mkproj), paste0(mkproj," "), ""),
                         paste("mkproj",
                               ifelse(.multi, "-m", ""),
                               proj,
                               drugdir,
                               sep = " "), ";")

      }

      if (.multi & stddir) {
        mkstddir <- paste0(paste("mkproj",
                                 ifelse(!dir.exists(projdir),
                                        # multi-drug and standard directory structure requested
                                        "-u",
                                        # existing project that is being converted to multi-drug
                                        "-mu"),
                                 proj,
                                 drugdir,
                                 sep = " "), ";")

        # update the drug/project/README.txt to indicate that this is a departure
        # from QMS regarding multi-drug directory structure
        update_readme <- paste0(paste0("echo ",'"Per planned non-compliance with multi-drug directory structure (CAPA # 2023-03),\\n',
                                       'the project folder has been updated with traditional {map, projman, proposal, rpt}\\n',
                                       'directories to facilitate the creation of drug-independent reports under each\\n',
                                       'project number. For combined, multiple drug reports, the work should be done in\\n',
                                       'the appropriate ', consultdir, ' project folder.\\n\\n" | cat - ',fs::path_join(c(projdir, "README.txt")),
                                       ' > ~/temp_update_README.txt; cat ~/temp_update_README.txt > ',fs::path_join(c(projdir, "README.txt")),
                                       '; /usr/bin/rm -f ~/temp_update_README.txt;'))
        # update_readme <- paste0(paste0("awk 'BEGIN{print ",
        #                                            '"Per planned non-compliance with multi-drug directory structure (CAPA # 2023-03),\\n',
        #                                            'the project folder has been updated with traditional {map, projman, proposal, rpt}\\n',
        #                                            'directories to facilitate the creation of drug-independent reports under each\\n',
        #                                            'project number. For combined, multiple drug reports, the work should be done in\\n',
        #                                            'the appropriate ', consultdir, ' project folder.\\n\\n"',"}{print}' ",
        #                                            fs::path_join(c(projdir, "README.txt")), " > tempREADME.txt; mv tempREADME.txt > ",
        #                                            fs::path_join(c(projdir, "README.txt")), ";"))
        # update_readme <- paste0(paste0("sed -i '1i ",
        #                                "Per planned non-compliance with multi-drug directory structure (CAPA # 2023-03),\\n",
        #                                "the project folder has been updated with traditional {map, projman, proposal, rpt}\\n",
        #                                "directories to facilitate the creation of drug-independent reports under each\\n",
        #                                "project number. For combined, multiple drug reports, the work should be done in\\n",
        #                                "the appropriate ", consultdir, " project folder.\\n\\n' ",
        #                                fs::path_join(c(projdir, "README.txt"))), ";")
      }

      if (path_length > 5 & !dir.exists(stagedir)) {
        mkstage <- paste0(paste(ifelse(proj_nchar == 8,
                                       "mkstage8",
                                       "mkstage"),
                                stage, projdir, sep = " "), ";")
      }

    }

    mkcmd <- stringr::str_squish(paste(mkconsultgen,
                                       mkconsultproj,
                                       mkdrug,
                                       mkproj,
                                       mkstddir,
                                       update_readme,
                                       mkstage,
                                       sep = " ")
    )

    do.not.activate.terminal <<- !isdatlibuser & any(!is.null(c(mkconsultgen, mkconsultproj, mkdrug, mkproj, mkstddir)))

    combined_message_to_display[[.pathn]] <-
      paste0(
        "\n\nRequested File Path: ", path, "\n",
        if (length(mkcmd) == 0) {
          paste0("\n  Warning message:\n    All of the requested directories in already exist.")
        } else {
          paste0(
            if (do.not.activate.terminal) {
              paste0("\n  Warning message:",
                     "\n    Only SDR can run the mkdrug and mkproj scripts.",
                     "\n      Please submit the function call mkscripts(paths = '", path, ifelse(.multi, paste0("', multi = ", .multi, ", descriptor = '", basename(consultdir), "', stddir = ", stddir), "'"), ")",
                     "\n      or the following output to SDR for processing.\n")
            },
            if (.multi & path_length > 4) {
              paste0("\n  Existing project within a multi-drug directory that you would like to associate this project directory with:\n    ",
                     consultprojdir,"\n")
            },
            paste0("\n  Consolidated mk* scripts:\n    ", mkcmd),
            if (send.to.term & !do.not.activate.terminal) {
              paste0("\n\n  The consolidated scripts have been sent to the active terminal window.")
            } else {
              paste0("\n\n  The consolidated scripts have NOT been sent to the active terminal window.")
            }
          )
        })

    if (length(mkcmd) > 0 & send.to.term & !do.not.activate.terminal) {
      mkcmds[[.pathn]] <- mkcmd %>% invisible()
    }

  }

  # remove NULL values from the combined_message_to_display object
  combined_message_to_display <- Filter(Negate(is.null), combined_message_to_display)

  # display the combined messages
  if (length(combined_message_to_display) > 0) {
    message("\n\nDisplaying the Combined Messages for All of the Requested Paths\n",
            paste0(rep("=", 63), collapse = ""),
            combined_message_to_display)
  }

  # remove NULL values from the mkcmds object
  mkcmds <- Filter(Negate(is.null), mkcmds)

  if (send.to.term & length(mkcmds) > 0) {
    terminal.id <- rstudioapi::terminalList()
    if (length(terminal.id) == 0) {
      rstudioapi::terminalCreate(show = FALSE)
    }
    rstudioapi::terminalSend(id = rstudioapi::terminalList(), text = paste0(mkcmds, collapse = " "))
    message("\nAll of the consolidated scripts have been sent to the active terminal window.")
  }

  if (send.to.term & activate.term & length(mkcmds) > 0) {
    message("\nThe active terminal window will be activated in ", sleep, " seconds.")
    for (sec_ in sleep:min(sleep, 1)) {
      cat(sec_, "\r")
      flush.console()
      Sys.sleep(1)
    }
    # Sys.sleep(sleep)
    rstudioapi::terminalActivate(rstudioapi::terminalList())
  }

}
