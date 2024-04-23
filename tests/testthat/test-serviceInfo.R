# This checks the serviceInfo getter.
# library(gobbler); library(testthat); source("test-serviceInfo.R")

info <- startGobbler()

test_that("serviceInfo works correctly", {
    deets <- serviceInfo(url=info$url)
    expect_identical(deets$staging, info$staging)
    expect_identical(deets$registry, info$registry)
})
