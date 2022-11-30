#' fileutils: Collection of functions to handle and maniputate filenames.
#'
#' This package does not export any objects.
#'
#' This package exports the following functions:
#'
#' * \code{\link{get_filename_extension}}
#' * \code{\link{change_filename_extension}}
#' * \code{\link{bare_filename}}
#' * \code{\link{pks_dependencies}}
#' * \code{\link{get_relative_filename}}
#' * \code{\link{get_inp_filename}}
#' * \code{\link{create_dates_dataframe}}
#' * \code{\link{rSIF_repair_exprstr_from_batch}}
#' * \code{\link{rSIF_get_label_and_exprstr}}

#'
#' @docType package
#' @name fileutils
#'
#' @importFrom tools file_ext package_dependencies
#' @importFrom magrittr "%<>%"
#' @importFrom utils "contrib.url" "available.packages"
#' @importFrom R.utils "getRelativePath"
#' @importFrom scriptName "current_filename"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr %>%
#' @importFrom lubridate "year"
#' @importFrom lubridate "month"
#' @importFrom lubridate "day"
NULL
