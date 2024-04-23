# This tests the upload functions. 
# library(testthat); library(gobbler); source("test-versionPath.R")

info <- startGobbler()
removeProject("test", staging=info$staging, url=info$url)
createProject("test", staging=info$staging, url=info$url)

tmp <- tempfile()
dir.create(tmp)
write(file=file.path(tmp, "blah.txt"), LETTERS, ncol=1)
dir.create(file.path(tmp, "foo"))
write(file=file.path(tmp, "foo", "bar.txt"), 1:10, ncol=1)

uploadDirectory(
    project="test", 
    asset="annarose", 
    version="v1", 
    directory=tmp,
    staging=info$staging, 
    url=info$url
)

test_that("versionPath works as expected", {
    out <- versionPath("test", "annarose", "v1", registry=info$registry, url=info$url)
    expect_identical(out, file.path(info$registry, "test", "annarose", "v1"))
    expect_identical(readLines(file.path(out, "blah.txt")), LETTERS)

    cache <- tempfile()
    out <- versionPath("test", "annarose", "v1", registry=info$registry, url=info$url, cache=cache, forceRemote=TRUE)
    expect_true(startsWith(out, cache))
    expect_true(endsWith(out, file.path("test", "annarose", "v1")))
    expect_identical(readLines(file.path(out, "foo", "bar.txt")), as.character(1:10))
})
