# This checks the serviceInfo getter.
# library(gobbler); library(testthat); source("test-serviceInfo.R")

info <- startGobbler()

test_that("serviceInfo works correctly", {
    deets <- serviceInfo(url=info$url)

    # Normalizing paths to avoid string mismatches when Gobbler cleans the path.
    # This is relevant when tempfile() does not return a cleaned path, e.g.,
    # on Macs, there are instances of redundant '//'.
    expect_identical(normalizePath(deets$staging), normalizePath(info$staging))
    expect_identical(normalizePath(deets$registry), normalizePath(info$registry))
})
