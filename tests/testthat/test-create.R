# This tests the creation functions.
# library(testthat); library(gobbler); source("test-create.R")

info <- startGobbler()
removeProject("test-create", staging=info$staging, url=info$url)

test_that("project creation works as expected for complex permissions", {
    createProject(
        project="test-create",
        owners=c("LTLA", "jkanche"),
        uploaders=list(list(id="lawremi"), list(id="PeteHaitch", until=Sys.time() + 100)),
        staging=info$staging,
        url=info$url
    )

    perms <- fetchPermissions("test-create", registry=info$registry)
    expect_identical(perms$owners, list("LTLA", "jkanche"))
    expect_identical(perms$uploaders[[1]]$id, "lawremi")
    expect_identical(perms$uploaders[[2]]$id, "PeteHaitch")
    expect_s3_class(perms$uploaders[[2]]$until, "POSIXct")
})
