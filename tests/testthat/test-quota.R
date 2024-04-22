# This tests the quota setter functions 
# library(testthat); library(gobbler); source("test-quota.R")

info <- startGobbler()
removeProject("test-usage", info$staging, url=info$url)
createProject("test-usage", info$staging, url=info$url)

# Mocking up an upload. 
src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
write(file=file.path(src, "whee"), "stuff")
uploadDirectory("test-usage", "simple", "v1", src, staging=info$staging, url=info$url)

test_that("usage refreshment works as expected", {
    write(file=file.path(info$registry, "test-usage", "..usage"), '{ "total": 0 }')
    expect_identical(fetchUsage("test-usage", registry=info$registry), 0L)

    # Fixing the project usage.
    refreshUsage("test-usage", staging=info$staging, url=info$url)
    expect_identical(fetchUsage("test-usage", registry=info$registry), 10L)
})
