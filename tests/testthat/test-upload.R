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

test_that("upload works as expected for absolute links", {
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

test_that("upload works as expected for relative links", {
    dest <- tempfile()
    dir.create(dest)
    write(file=file.path(dest, "blah.txt"), letters)
    file.symlink("blah.txt", file.path(dest, "whee.txt")) # relative links within the directory are preserved.
    dir.create(file.path(dest, "foo"))
    file.symlink("../whee.txt", file.path(dest, "foo/bar.txt")) 

    outside <- tempfile(tmpdir=dirname(dest))
    write(file=outside, "FOOBLEWOOBLE")
    file.symlink(file.path("../../", basename(outside)), file.path(dest, "foo/outer.txt")) # relative links outside the directory are lost.

    uploadDirectory(
        project="test-more-upload", 
        asset="nicole", 
        version="1", 
        directory=dest,
        staging=info$staging,
        url=info$url
    )

    man <- fetchManifest("test-more-upload", "nicole", "1", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt", "foo/outer.txt", "whee.txt"))
    expect_false(is.null(man[["whee.txt"]]$link))
    expect_null(man[["foo/outer.txt"]]$link)
    expect_false(is.null(man[["foo/bar.txt"]]$link))
    expect_null(man[["blah.txt"]]$link)
    expect_identical(13L, man[["foo/outer.txt"]]$size)
    expect_identical(man[["whee.txt"]]$size, man[["foo/bar.txt"]]$size)
    expect_identical(man[["whee.txt"]]$size, man[["blah.txt"]]$size)
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

test_that("upload consumes files by default", {
    dir <- allocateUploadDirectory(info$staging)
    write(file=file.path(dir, "blah.txt"), letters)
    dir.create(file.path(dir, "foo"))
    write(file=file.path(dir, "foo", "bar.txt"), 1:10)

    # Use different names here to avoid issues with MD5 deduplication.
    uploadDirectory(
        project="test-upload", 
        asset="anastasia", 
        version="1", 
        directory=dir,
        staging=info$staging,
        url=info$url,
        consume=FALSE
    )
    expect_true(file.exists(file.path(dir, "blah.txt")))
    expect_true(file.exists(file.path(dir, "foo/bar.txt")))

    uploadDirectory(
        project="test-upload", 
        asset="victoria", 
        version="1", 
        directory=dir,
        staging=info$staging,
        url=info$url,
        consume=TRUE
    )
    expect_false(file.exists(file.path(dir, "blah.txt")))
    expect_false(file.exists(file.path(dir, "foo/bar.txt")))
})


test_that("upload ignores dotfiles by default", {
    dir <- tempfile()
    dir.create(dir)
    write(file=file.path(dir, ".blah.txt"), letters)
    dir.create(file.path(dir, ".foo"))
    write(file=file.path(dir, ".foo", "bar.txt"), 1:10)

    uploadDirectory(
        project="test-upload", 
        asset="annabelle", 
        version="0", 
        directory=dir,
        staging=info$staging,
        url=info$url
    )
    man <- fetchManifest("test-upload", "annabelle", "0", registry=info$registry)
    expect_identical(length(man), 0L)

    # unless we disable the ignorance.
    uploadDirectory(
        project="test-upload", 
        asset="annabelle", 
        version="1", 
        directory=dir,
        staging=info$staging,
        url=info$url,
        ignore..=FALSE
    )
    man <- fetchManifest("test-upload", "annabelle", "1", registry=info$registry)
    expect_identical(sort(names(man)), c(".blah.txt", ".foo/bar.txt"))
})

test_that("allocation works when creation is disabled", {
    expect_true(file.exists(allocateUploadDirectory(info$staging)))
    expect_false(file.exists(allocateUploadDirectory(info$staging, create=FALSE)))
})
