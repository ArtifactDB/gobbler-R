# library(testthat); library(gobbler); source("test-validateVersion.R")

info <- startGobbler()
removeProject("test-R-validate", staging=info$staging, url=info$url)
createProject("test-R-validate", staging=info$staging, url=info$url)

test_that("validation functions work as expected", {
    src <- allocateUploadDirectory(info$staging)
    write(file=file.path(src, "foo"), "BAR")
    uploadDirectory("test-R-validate", "simple", "v1", src, staging=info$staging, url=info$url)
    expect_error(validateVersion("test-R-validate", "simple", "v1", staging=info$staging, url=info$url), NA)

    # Let's add a new file directly to the directory.
    write(file=file.path(info$registry, "test-R-validate", "simple", "v1", "whee"), "stuff")
    expect_error(validateVersion("test-R-validate", "simple", "v1", staging=info$staging, url=info$url), "extra file \"whee\"")
})
