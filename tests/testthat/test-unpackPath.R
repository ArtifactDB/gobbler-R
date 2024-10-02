# library(gobbler); library(testthat); source("test-unpackPath.R")

test_that("unpackPath works as expected", {
    out <- unpackPath("project/asset/version/path")
    expect_identical(out, list(project="project", asset="asset", version="version", path="path"))

    out <- unpackPath("project/asset/version")
    expect_identical(out, list(project="project", asset="asset", version="version", path=NULL))

    out <- unpackPath("project/asset/version/")
    expect_identical(out, list(project="project", asset="asset", version="version", path=NULL))
})
