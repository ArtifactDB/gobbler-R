# This tests the various fetching functions.
# library(testthat); library(gobbler); source("test-fetch.R")

info <- startGobbler()
removeProject("test", staging=info$staging, url=info$url)
createProject("test", staging=info$staging, url=info$url)

src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
dir.create(file.path(src, "whee"))
write(file=file.path(src, "whee", "blah"), "stuff")

res <- uploadDirectory("test", "fetch", "v1", src, staging=info$staging, url=info$url)
res <- uploadDirectory("test", "fetch", "v2", src, staging=info$staging, url=info$url)

test_that("fetchManifest works as expected", {
    man <- fetchManifest("test", "fetch", "v1", registry=info$registry)
    expect_identical(man[["foo"]]$size, 4L)
    expect_identical(man[["whee/blah"]]$size, 6L)
})

test_that("fetchSummary works as expected", {
    summ <- fetchSummary("test", "fetch", "v1", registry=info$registry)
    expect_type(summ$upload_user_id, "character")
    expect_s3_class(summ$upload_start, "POSIXct")
    expect_s3_class(summ$upload_finish, "POSIXct")
})

test_that("fetchLatest works as expected", {
    expect_identical(fetchLatest("test", "fetch", registry=info$registry), "v2")
})

test_that("fetchUsage works as expected", {
    expect_true(fetchUsage("test", registry=info$registry) > 0)
})

test_that("fetchPermissions works as expected", {
    perms <- fetchPermissions("test", registry=info$registry)
    expect_type(perms$owners, "list")
    expect_type(perms$uploaders, "list")
})
