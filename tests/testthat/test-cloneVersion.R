# This checks the cloneVersion function.
# library(testthat); library(gobbler); source("test-cloneVersion.R")

info <- startGobbler()
removeAsset("test", "clone", staging=info$staging)

src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
dir.create(file.path(src, "whee"))
write(file=file.path(src, "whee", "blah"), "stuff")

res <- uploadDirectory("test", "clone", "v1", src, staging=info$staging)

test_that("cloneVersion works as expected with existing files", {
    dest <- tempfile()
    out <- cloneVersion("test", "clone", "v1", dest, registry=info$registry)
    expect_identical(readLines(file.path(dest, "FOO")), "BAR")
    expect_true(file.exists(Sys.readlink(file.path(dest, "FOO"))))
    expect_identical(readLines(file.path(dest, "whee", "blah")), "stuff")
    expect_true(file.exists(Sys.readlink(file.path(dest, "whee", "blah"))))
})
