# This checks the cloneVersion function.
# library(testthat); library(gobbler); source("test-cloneVersion.R")

info <- startGobbler()
removeProject("test", staging=info$staging, url=info$url)
createProject("test", staging=info$staging, url=info$url)

src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
dir.create(file.path(src, "whee"))
write(file=file.path(src, "whee", "blah"), "stuff")

uploadDirectory("test", "clone", "v1", src, staging=info$staging, url=info$url)

test_that("cloneVersion works as expected with existing files", {
    dest <- tempfile()
    out <- cloneVersion("test", "clone", "v1", dest, registry=info$registry)
    expect_identical(readLines(file.path(dest, "foo")), "BAR")
    expect_true(file.exists(Sys.readlink(file.path(dest, "foo"))))
    expect_identical(readLines(file.path(dest, "whee", "blah")), "stuff")
    expect_true(file.exists(Sys.readlink(file.path(dest, "whee", "blah"))))
})
