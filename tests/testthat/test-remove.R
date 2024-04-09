# This tests the removal functions 
# library(testthat); library(gobbler); source("test-remove.R")

info <- startGobbler()
removeProject("test-R-remove", staging=info$staging)
createProject("test-R-remove", staging=info$staging)

src <- allocateUploadDirectory(info$staging)
res <- uploadDirectory("test-R-remove", "sacrifice", "v1", src, staging=info$staging)
Sys.sleep(1.1) # force timestamps to be different for next versions.
res <- uploadDirectory("test-R-remove", "sacrifice", "v2", src, staging=info$staging)

test_that("removal functions work as expected", {
    expect_true(file.exists(file.path(info$registry, "test-R-remove", "sacrifice", "v2")))
    removeVersion("test-R-remove", "sacrifice", "v2", staging=info$staging)
    expect_false(file.exists(file.path(info$registry, "test-R-remove", "sacrifice", "v2")))

    expect_true(file.exists(file.path(info$registry, "test-R-remove", "sacrifice")))
    removeAsset("test-R-remove", "sacrifice", staging=info$staging)
    expect_false(file.exists(file.path(info$registry, "test-R-remove", "sacrifice")))

    expect_true(file.exists(file.path(info$registry, "test-R-remove")))
    removeProject("test-R-remove", staging=info$staging)
    expect_false(file.exists(file.path(info$registry, "test-R-remove")))
})
