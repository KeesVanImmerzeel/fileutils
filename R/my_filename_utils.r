# ----------------------------------------------------------------------------
#' Get filename extension (with leading dot).
#'
#' @param a_filename filename (character).
#' @return filename extension with leading dot (character).
#' @examples
#' get_filename_extension("test.dat")
#' get_filename_extension("test")
#'
#' @export
get_filename_extension <- function(a_filename) {
   return(paste0(".", tools::file_ext(a_filename)))
}

# ----------------------------------------------------------------------------
#' Change filename extension.
#'
#' @inheritParams get_filename_extension
#' @param new_ext new extension (with leading dot) (character)
#' @return filename with new extension (character)
#' @details a_filename can also be a character vector or list of character.
#' @examples
#'
#' change_filename_extension("test.dat")
#' change_filename_extension("test")
#' change_filename_extension("test.dat", ".txt")
#' change_filename_extension(c("test.txt", "test.dat"))
#' change_filename_extension(as.list(c("test.txt", "test.dat")))
#' @export
change_filename_extension <-
      function(a_filename, new_ext = ".tif") {
            if ((typeof(a_filename) == "character") & (length(a_filename) == 1)) {
                  if (basename(a_filename) != "") {
                        if ((nchar(new_ext) > 0) &
                            (substring(new_ext, 1, 1) != ".")) {
                              new_ext <- paste0(".", new_ext)
                        }
                        oldExt <- get_filename_extension(a_filename)
                        if (oldExt != ".") {
                              a_filename <- gsub(
                                    pattern = paste0(oldExt, "$"),
                                    replacement = new_ext,
                                    x = a_filename
                              )
                        } else {
                              a_filename <- paste0(a_filename, new_ext)
                        }
                  }
                  return(a_filename)
            } else if ((typeof(a_filename) == "list") |
                       ((typeof(a_filename) == "character") & (length(a_filename) > 1)))  {
                  sapply(a_filename,
                         change_filename_extension,
                         new_ext = new_ext) %>% unname()
            } else {
                  return(NA)
            }
      }
# ----------------------------------------------------------------------------

#' Return the bare filename (no path, no extension)
#'
#' @inheritParams get_filename_extension
#' @return filename without path and extension (character)
#' @details a_filename can also be a character vector or list of character.
#' @examples
#' bare_filename(file.path("tmp", "test.dat"))
#' bare_filename(c(file.path("tmp", "test1.dat"), file.path("tmp", "test2.dat")))
#' bare_filename(as.list(c(
#' file.path("tmp", "test1.dat"), file.path("tmp", "test2.dat")
#' )))
#' @export
bare_filename <- function( a_filename ) {
      #a_filename %>% change_filename_extension(.,"" ) %>% basename()
      if ((typeof(a_filename) == "character") & (length(a_filename) == 1)) {
            a_filename %<>% change_filename_extension("") %>% basename()
            return(a_filename)
      } else if ((typeof(a_filename) == "list") |
                 ((typeof(a_filename) == "character") & (length(a_filename) > 1)))  {
            sapply(a_filename,
                   bare_filename) %>% unname()
      } else {
            return(NA)
      }
}

# ----------------------------------------------------------------------------

#' Return names of all dependent packages
#'
#' @param pks Names of packages (character)
#' @return Names of all packages that depend on packages specified (character)
#' @examples
#' pks_dependencies(c("magrittr","dplyr"))
#' @export
pks_dependencies <- function(pks) {
   . = NULL
   .list_package_dependencies <- function(pkg) {
      "https://cloud.r-project.org/" %>% utils::contrib.url() %>%
         utils::available.packages() %>%
         tools::package_dependencies(pkg, db = ., recursive = TRUE)
   }
   apply(array(pks), 1, .list_package_dependencies) %>%
      unlist() %>%
      unique()
}

# ----------------------------------------------------------------------------

#' Construct the pathname/filename with new basepath.
#'
#' @inheritParams get_filename_extension
#' @inheritParams change_filename_extension
#' @param relativeTo Reference pathname (character)
#' @param new_basepath new base path (character)
#' @param create_dir Create folder (boolean)
#' @examples
#' a_filename <- "c:/input/input_sub/test.txt"
#' relativeTo <- "c:/input"
#' new_basepath <- "d:/results"
#' new_ext <- ".tif"
#' create_dir <- FALSE
#' get_relative_filename( a_filename, relativeTo, new_basepath, new_ext, create_dir )
#' @export
get_relative_filename <-
   function(a_filename,
            relativeTo,
            new_basepath,
            new_ext = NULL,
            create_dir = FALSE) {
      . = NULL
      pth <- new_basepath
      reldir <-
         a_filename %>% dirname() %>% R.utils::getRelativePath(relativeTo)
      if (reldir != ".") {
         pth <-
            file.path(pth, reldir)
      }
      if ((create_dir) & (!dir.exists(pth))) {
         dir.create(pth, recursive = TRUE)
      }
      a_filename %<>% basename() %>% file.path(pth, .)
      if (!is.null(new_ext)) {
         a_filename %<>% fileutils::change_filename_extension(new_ext)
      }
      return(a_filename)
   }

# ----------------------------------------------------------------------------

#' Determine a script’s filename from within the script itself to create a new filename with a specified extension.
#'
#' @inheritParams change_filename_extension
#' @export
get_inp_filename <- function(new_ext = ".inp") {
      scriptName::current_filename() %>%
      fileutils::bare_filename() %>%
      fileutils::change_filename_extension( new_ext )
}


# ----------------------------------------------------------------------------

#' Create a data.frame with fields derived from the dates specified.
#'
#' The newly created fiels can be used in selections
#'
#' @param dates Dates (character)
#' @return data.frame with date related fields (date, year, month, day, hydro_year, season, hydr_season, apr1okt1 )
#' @examples
#' dates <- as.Date( c("2009-01-02", "2009-01-03", "2009-01-04", "2009-01-05", "2009-01-06") )
#' dates_dataframe( dates )
#' @export
dates_dataframe <- function(dates) {
      month <- NULL
      day <- NULL
      df <-
            data.frame(
                  date = dates,
                  year = lubridate::year(dates),
                  month = lubridate::month(dates),
                  day = lubridate::day(dates)
            )
      df %<>% dplyr::mutate(hydro_year = ifelse(month %in% 10:12, year + 1, year))
      df %<>% dplyr::mutate(season = ifelse(
            month %in% 9:11,
            "herfst",
            ifelse(
                  month %in% c(12, 1, 2),
                  "Winter",
                  ifelse(month %in% 3:5, "voorjaar",
                         "zomer")
            )
      ))
      df %<>% dplyr::mutate(hydr_season = ifelse(month %in% c(10:12, 1:3),
                                          "hydr_wintr",
                                          "hydr_summr"))
      df %<>% dplyr::mutate(apr1okt1 = ifelse(
            month == 4 & day == 1,
            "apr_1",
            ifelse(month == 10 & day == 1,
                   "okt_1", "other")
      ))
      return(df)
}
