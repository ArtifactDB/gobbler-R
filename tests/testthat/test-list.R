# This tests the various listing functions.
# library(testthat); library(gobbler); source("test-list.R")

info <- startGobbler()
removeProject("test", staging=info$staging, url=info$url)
removeProject("more-list-test", staging=info$staging, url=info$url)
createProject("test", staging=info$staging, url=info$url)
createProject("more-list-test", staging=info$staging, url=info$url)

src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
dir.create(file.path(src, "whee"))
write(file=file.path(src, "whee", "blah"), "stuff")
write(file=file.path(src, "whee2"), LETTERS)

uploadDirectory("test", "list", "v1", src, staging=info$staging, url=info$url)
uploadDirectory("test", "list", "v2", src, staging=info$staging, url=info$url)
uploadDirectory("test", "more-list", version="foo", src, staging=info$staging, url=info$url)
uploadDirectory("more-list-test", "list", version="bar", src, staging=info$staging, url=info$url)

test_that("listVersions works as expected", {
    versions <- listVersions("test", "list", registry=info$registry, url=info$url)
    expect_true("v1" %in% versions)
    expect_true("v2" %in% versions)

    rversions <- listVersions("test", "list", registry=info$registry, url=info$url, forceRemote=TRUE)
    expect_identical(sort(versions), sort(rversions))
})

test_that("listAssets works as expected", {
    assets <- listAssets("test", registry=info$registry, url=info$url)
    expect_true("list" %in% assets)
    expect_true("more-list" %in% assets)

    rassets <- listAssets("test", registry=info$registry, url=info$url, forceRemote=TRUE)
    expect_identical(sort(assets), sort(rassets))
})

test_that("listProjects works as expected", {
    projects <- listProjects(registry=info$registry, url=info$url)
    expect_true("test" %in% projects)
    expect_true("more-list-test" %in% projects)

    rprojects <- listProjects(registry=info$registry, url=info$url, forceRemote=TRUE)
    expect_identical(sort(projects), sort(rprojects))
})

test_that("listFiles works as expected", {
    files <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url))
    expect_identical(files, sort(c("..summary", "..manifest", "foo", "whee/blah", "whee2")))
    rfiles <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url, forceRemote=TRUE))
    expect_identical(files, rfiles)

    files <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url, include..=FALSE))
    expect_identical(files, sort(c("foo", "whee/blah", "whee2")))
    rfiles <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url, include..=FALSE, forceRemote=TRUE))
    expect_identical(files, rfiles)

    files <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url, prefix="whee"))
    expect_identical(files, sort(c("whee/blah", "whee2")))
    rfiles <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url, forceRemote=TRUE, prefix="whee"))
    expect_identical(files, rfiles)

    files <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url, prefix="whee/"))
    expect_identical(files, sort(c("whee/blah")))
    rfiles <- sort(listFiles("test", "list", "v1", registry=info$registry, url=info$url, forceRemote=TRUE, prefix="whee/"))
    expect_identical(files, rfiles)
})

test_that("listFiles works with some empty directories", {
    src.empty <- allocateUploadDirectory(info$staging)
    write(file=file.path(src.empty, "yay"), "BAR")
    dir.create(file.path(src.empty, "whee"))
    write(file=file.path(src.empty, "whee", "blah"), "stuff")
    dir.create(file.path(src.empty, "whee", "stuff"))
    dir.create(file.path(src.empty, "foo", "bar"), recursive=TRUE)
    uploadDirectory("test", "list-empty", "v1", src.empty, staging=info$staging, url=info$url)

    files <- sort(listFiles("test", "list-empty", "v1", registry=info$registry, url=info$url))
    expect_identical(files, sort(c("..summary", "..manifest", "foo/bar/", "whee/blah", "whee/stuff/", "yay")))
    rfiles <- sort(listFiles("test", "list-empty", "v1", registry=info$registry, url=info$url, forceRemote=TRUE))
    expect_identical(files, rfiles)
})
