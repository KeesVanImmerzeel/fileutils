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

## Get help

To get help on the functions in this package type a question mark before the function name, like `?get_filename_extension()`
