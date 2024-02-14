# Test some miscellaneous functions.
# library(testthat); library(gobbler); source("test-misc.R")

test_that("casting of datetimes works as expected", {
    out <- gobbler:::cast_datetime("2023-12-04T14:41:19+01:00")
    expect_false(is.na(out))

    # Behaves with fractional seconds.
    out <- gobbler:::cast_datetime("2023-12-04T14:41:19.21323+01:00")
    expect_false(is.na(out))

    # Behaves with 'Z'.
    out <- gobbler:::cast_datetime("2023-12-04T14:41:19Z")
    expect_false(is.na(out))
})
