# library(testthat); library(gobbler); source("test-rerouteLinks.R")

info <- startGobbler()
removeProject("test", info$staging, url=info$url) # start with a clean slate.
createProject("test", info$staging, url=info$url)

# Mocking up an asset so we have something interesting to reroute.
src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)
uploadDirectory("test", "simple", "v2", src, staging=info$staging, url=info$url)
uploadDirectory("test", "simple", "v3", src, staging=info$staging, url=info$url)

test_that("rerouting functions work as expected", {
    actions <- rerouteLinks(list(list(project="test", asset="simple", version="v1")), staging=info$staging, url=info$url, dry.run=TRUE)
    expect_true(all("test/simple/v1/foo" == actions$source))
    expect_true("test/simple/v2/foo" %in% actions$path)
    expect_true("test/simple/v3/foo" %in% actions$path)
    expect_false(actions$copy["test/simple/v3/foo" == actions$path])
    expect_true(actions$copy["test/simple/v2/foo" == actions$path])
    expect_true(Sys.readlink(file.path(info$registry, "test/simple/v2/foo")) != "")

    actions2 <- rerouteLinks(list(list(project="test", asset="simple", version="v1")), staging=info$staging, url=info$url)
    expect_identical(actions, actions2)
    expect_true(Sys.readlink(file.path(info$registry, "test/simple/v2/foo")) == "")
})
