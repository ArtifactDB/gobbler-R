# This tests the latest refresher functions 
# library(testthat); library(gobbler); source("test-latest.R")

info <- startGobbler()
removeProject("test", staging=info$staging)
createProject("test", staging=info$staging)

src <- allocateUploadDirectory(info$staging)
res <- uploadDirectory("test", "latest", "v1", src, staging=info$staging)
Sys.sleep(1.1) # force timestamps to be different for next versions.
res <- uploadDirectory("test", "latest", "v2", src, staging=info$staging)
Sys.sleep(1.1)
res <- uploadDirectory("test", "latest", "v3", src, staging=info$staging)

test_that("latest setting works as expected", {
    expect_identical(fetchLatest("test", "latest", registry=info$registry), "v3")

    unlink(file.path(info$registry, "test", "latest", "..latest")) 
    expect_null(fetchLatest("test", "latest", registry=info$registry))

    v <- refreshLatest("test", "latest", staging=info$staging)
    expect_identical(fetchLatest("test", "latest", registry=info$registry), "v3")
})
