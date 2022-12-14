# fileutils

<!-- badges: start -->
[![R-CMD-check](https://github.com/KeesVanImmerzeel/fileutils/workflows/R-CMD-check/badge.svg)](https://github.com/KeesVanImmerzeel/fileutils/actions)
<!-- badges: end -->

Collection of functions to handle and manipulate filenames.

## Installation

`install_github("KeesVanImmerzeel/fileutils")`

Then load the package with:

`library("fileutils")` 

## Functions in this package
- `get_filename_extension()`: Get filename extension (with leading dot).
- `change_filename_extension()`: Change filename extension.
- `bare_filename()`: Return the bare filename (no path, no extension).
- `pks_dependencies`: Return names of all dependent packages.
- `get_relative_filename`: Construct the pathname/filename with new basepath.
- `get_inp_filename`: Determine a script’s filename from within the script itself to create a new filename with a specified extension.
- `create_dates_dataframe`: Create a data.frame with fields derived from the dates specified.
- `rSIF_repair_exprstr_from_batch`: Repair an expression string that is imported from a DOS-batch file as an environment variable. 
- `rSIF_get_label_and_exprstr`: Return the label and expression part of a string created in DOS-batch file as an environment variable.

## Get help

To get help on the functions in this package type a question mark before the function name, like `?get_filename_extension()`
