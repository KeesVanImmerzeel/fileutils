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
#' @param pks Names of packages (chr)
#' @return Names of all packages that depend on packages specified (chr)
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
