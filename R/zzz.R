.onAttach <- function(libname,pkgname){
packageStartupMessage("Welcome \U0001F369 to \U0001F370 sweets \U0001F36D")
}

# Suppress R CMD check NOTEs for magrittr pipe placeholder
utils::globalVariables(".")
