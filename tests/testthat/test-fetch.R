# This tests the various fetching functions.
# library(testthat); library(gobbler); source("test-fetch.R")

info <- startGobbler()
removeProject("test", staging=info$staging, url=info$url)
createProject("test", staging=info$staging, url=info$url)

src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
dir.create(file.path(src, "whee"))
write(file=file.path(src, "whee", "blah"), "stuff")

uploadDirectory("test", "fetch", "v1", src, staging=info$staging, url=info$url)
uploadDirectory("test", "fetch", "v2", src, staging=info$staging, url=info$url)

test_that("fetchManifest works as expected", {
    man <- fetchManifest("test", "fetch", "v1", registry=info$registry, url=info$url)
    expect_identical(man[["foo"]]$size, 4L)
    expect_identical(man[["whee/blah"]]$size, 6L)

    rman <- fetchManifest("test", "fetch", "v1", registry=info$registry, url=info$url, cache=tempfile(), forceRemote=TRUE)
    expect_identical(man, rman)
})

test_that("fetchSummary works as expected", {
    summ <- fetchSummary("test", "fetch", "v1", registry=info$registry, url=info$url)
    expect_type(summ$upload_user_id, "character")
    expect_s3_class(summ$upload_start, "POSIXct")
    expect_s3_class(summ$upload_finish, "POSIXct")

    rsumm <- fetchSummary("test", "fetch", "v1", registry=info$registry, url=info$url, cache=tempfile(), forceRemote=TRUE)
    expect_identical(summ, rsumm)
})

test_that("fetchLatest works as expected", {
    expect_identical(fetchLatest("test", "fetch", registry=info$registry, url=info$url), "v2")
    expect_null(fetchLatest("test", "missing", registry=info$registry, url=info$url))

    expect_identical(fetchLatest("test", "fetch", registry=info$registry, url=info$url, forceRemote=TRUE), "v2")
    expect_null(fetchLatest("test", "missing", registry=info$registry, url=info$url, forceRemote=TRUE))
})

test_that("fetchUsage works as expected", {
    expect_true(fetchUsage("test", registry=info$registry, url=info$url) > 0)
    expect_true(fetchUsage("test", registry=info$registry, url=info$url, forceRemote=TRUE) > 0)
})

test_that("fetchPermissions works as expected", {
    perms <- fetchPermissions("test", registry=info$registry, url=info$url)
    expect_type(perms$owners, "list")
    expect_type(perms$uploaders, "list")

    rperms <- fetchPermissions("test", registry=info$registry, url=info$url, forceRemote=TRUE)
    expect_identical(perms, rperms)
})

test_that("fetchFile works as expected", {
    p <- fetchFile("test/fetch/v1/foo", registry=info$registry, url=info$url)
    expect_identical(readLines(p), "BAR")
    expect_true(startsWith(p, info$registry))

    cache <- tempfile()
    p <- fetchFile("test/fetch/v1/whee/blah", registry=info$registry, url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(readLines(p), "stuff")
    expect_true(startsWith(p, cache))
})

test_that("fetchDirectory works as expected", {
    dir <- fetchDirectory("test/fetch/v2", registry=info$registry, url=info$url)
    expect_identical(readLines(file.path(dir, "foo")), "BAR")
    expect_true(startsWith(dir, info$registry))

    cache <- tempfile()
    rdir <- fetchDirectory("test/fetch/v2", registry=info$registry, url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(readLines(file.path(rdir, "foo")), "BAR")
    expect_identical(readLines(file.path(rdir, "whee", "blah")), "stuff")
    expect_true(startsWith(rdir, cache))

    # Subsequent requests are no-ops.
    write(file=file.path(rdir, "foo"), "more-bar")
    rdir2 <- fetchDirectory("test/fetch/v2", registry=info$registry, url=info$url, cache=cache, forceRemote=TRUE)
    expect_identical(rdir, rdir2)
    expect_identical(readLines(file.path(rdir2, "foo")), "more-bar")

    # Unless we force an overwrite.
    rdir2 <- fetchDirectory("test/fetch/v2", registry=info$registry, url=info$url, cache=cache, forceRemote=TRUE, overwrite=TRUE)
    expect_identical(readLines(file.path(rdir2, "foo")), "BAR")

    # Trying with multiple cores.
    cache <- tempfile()
    rdir2 <- fetchDirectory("test/fetch/v2", registry=info$registry, url=info$url, cache=cache, forceRemote=TRUE, concurrent=2)
    expect_identical(readLines(file.path(rdir2, "foo")), "BAR")
})
