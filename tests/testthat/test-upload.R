# This tests the upload functions. 
# library(testthat); library(gobbler); source("test-upload.R")

info <- startGobbler()
removeProject("test-upload", staging=info$staging, url=info$url)
removeProject("test-more-upload", staging=info$staging, url=info$url)
removeProject("test-upload-perms", staging=info$staging, url=info$url)
createProject("test-upload", staging=info$staging, url=info$url)
createProject("test-more-upload", staging=info$staging, url=info$url)
createProject("test-upload-perms", staging=info$staging, url=info$url)

tmp <- tempfile()
dir.create(tmp)
write(file=file.path(tmp, "blah.txt"), LETTERS)
dir.create(file.path(tmp, "foo"))
write(file=file.path(tmp, "foo", "bar.txt"), 1:10)

test_that("upload works as expected for regular files", {
    uploadDirectory(
        project="test-upload", 
        asset="jennifer", 
        version="1", 
        directory=tmp,
        staging=info$staging, 
        url=info$url
    )

    # Checking that the files were, in fact, correctly uploaded.
    man <- fetchManifest("test-upload", "jennifer", "1", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))
    expect_false(any(vapply(man, function(x) !is.null(x$link), TRUE)))

    # Deduplication happens naturally.
    uploadDirectory(
        project="test-upload", 
        asset="jennifer", 
        version="2", 
        directory=tmp,
        staging=info$staging,
        url=info$url
    )

    man <- fetchManifest("test-upload", "jennifer", "2", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))
    expect_true(all(vapply(man, function(x) !is.null(x$link), TRUE)))
})

test_that("upload works as expected for links", {
    dest <- tempfile()
    out <- cloneVersion("test-upload", "jennifer", "2", dest, registry=info$registry)
    write(file=file.path(dest, "whee"), "BLAH")

    uploadDirectory(
        project="test-more-upload", 
        asset="natalie", 
        version="1", 
        directory=dest,
        staging=info$staging,
        url=info$url
    )

    man <- fetchManifest("test-more-upload", "natalie", "1", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt", "whee"))
    expect_false(is.null(man[["blah.txt"]]$link))
    expect_false(is.null(man[["foo/bar.txt"]]$link))
    expect_null(man[["whee"]]$link)
})

test_that("upload works directly from the staging directory", {
    dir <- allocateUploadDirectory(info$staging)
    write(file=file.path(dir, "blah.txt"), letters)
    dir.create(file.path(dir, "foo"))
    write(file=file.path(dir, "foo", "bar.txt"), 1:10)

    uploadDirectory(
        project="test-upload", 
        asset="jennifer", 
        version="3", 
        directory=dir,
        staging=info$staging,
        url=info$url
    )

    man <- fetchManifest("test-upload", "jennifer", "3", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))
    expect_null(man[["blah.txt"]]$link)
    expect_false(is.null(man[["foo/bar.txt"]]$link))
})
