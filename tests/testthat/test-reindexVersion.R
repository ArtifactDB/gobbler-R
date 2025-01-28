# library(testthat); library(gobbler); source("test-reindexVersion.R")

info <- startGobbler()
removeProject("test-R-reindex", staging=info$staging, url=info$url)
createProject("test-R-reindex", staging=info$staging, url=info$url)

test_that("reindexing functions work as expected", {
    src <- allocateUploadDirectory(info$staging)
    write(file=file.path(src, "foo"), "BAR")
    uploadDirectory("test-R-reindex", "simple", "v1", src, staging=info$staging, url=info$url)

    # Let's add a new file directly to the directory.
    write(file=file.path(info$registry, "test-R-reindex", "simple", "v1", "whee"), "stuff")

    # This does not show up in the manifest...
    man <- fetchManifest("test-R-reindex", "simple", "v1", registry=info$registry, url=info$url)
    expect_identical(names(man), "foo")

    # Until we reindex the version.
    reindexVersion("test-R-reindex", "simple", "v1", staging=info$staging, url=info$url)
    man <- fetchManifest("test-R-reindex", "simple", "v1", registry=info$registry, url=info$url)
    expect_identical(names(man), c("foo", "whee"))
})
