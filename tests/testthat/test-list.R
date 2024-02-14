# This tests the various listing functions.
# library(testthat); library(gobbler); source("test-list.R")

info <- startGobbler()
removeAsset("test", "list", staging=info$staging)
removeAsset("test", "more-list", staging=info$staging)
removeProject("more-list-test", staging=info$staging)

src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
dir.create(file.path(src, "whee"))
write(file=file.path(src, "whee", "blah"), "stuff")
write(file=file.path(src, "whee2"), LETTERS)

res <- uploadDirectory("test", "list", "v1", src, staging=info$staging)
res <- uploadDirectory("test", "list", "v2", src, staging=info$staging)
res <- uploadDirectory("test", "more-list", version=NULL, src, staging=info$staging)
res <- uploadDirectory("more-list-test", "list", version=NULL, src, staging=info$staging)

test_that("listVersions works as expected", {
    versions <- listVersions("test", "list", registry=info$registry)
    expect_true("v1" %in% versions)
    expect_true("v2" %in% versions)
})

test_that("listAssets works as expected", {
    assets <- listAssets("test", registry=info$registry)
    expect_true("list" %in% assets)
    expect_true("more-list" %in% assets)
})

test_that("listProjects works as expected", {
    projects <- listProjects(registry=info$registry)
    expect_true("test" %in% projects)
    expect_true("more-list-test" %in% projects)
})

test_that("listFiles works as expected", {
    expect_identical(
         sort(listFiles("test", "list", "v1", registry=info$registry)),
         sort(c("..summary", "..manifest", "foo", "whee/blah", "whee2"))
    )

    expect_identical(
        sort(listFiles("test", "list", "v1", registry=info$registry, prefix="whee")),
        sort(c("whee/blah", "whee2"))
    )

    expect_identical(
        sort(listFiles("test", "list", "v1", registry=info$registry, prefix="whee/")),
        sort(c("whee/blah"))
    )
})
