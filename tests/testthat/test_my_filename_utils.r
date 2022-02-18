library(fileutils)

context("Filename utilities")

test_that("Filename extension is last characters after last point in filename (or empty string).",
          {
                expect_equal(get_filename_extension("test.dat"), ".dat")
                expect_equal(get_filename_extension("test"), ".")
          })

test_that("Filename extension is modified to desired extension.",
          {
                expect_equal(change_filename_extension("test.dat", ".txt"),
                             "test.txt")
                expect_equal(change_filename_extension("test.dat"), "test.tif")
                expect_equal(change_filename_extension("test"), "test.tif")
                expect_equal(change_filename_extension(""), "")
                expect_equal(change_filename_extension(file.path("tmp", "test.dat")),
                             file.path("tmp", "test.tif"))
                expect_equal(change_filename_extension(c("test.txt", "test.dat")),
                             c("test.tif", "test.tif"))
                expect_equal(change_filename_extension(as.list(c(
                      "test.txt", "test.dat"
                ))),
                c("test.tif", "test.tif"))
          })

test_that("Bare filenam(s) are returned.",
          {
                expect_equal(bare_filename("test.dat"), "test")

                expect_equal(bare_filename("test"), "test")
                expect_equal(bare_filename(""), "")
                expect_equal(bare_filename(file.path("tmp", "test.dat")), "test")
                expect_equal(bare_filename(c("test1.txt", "test2.dat")), c("test1", "test2"))
                expect_equal(bare_filename(as.list(c(
                      "test1.txt", "test2.dat"
                ))),
                c("test1", "test2"))
          })
