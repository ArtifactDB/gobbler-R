# This tests the permission setter functions 
# library(testthat); library(gobbler); source("test-permissions.R")

info <- startGobbler()
removeProject("test-perms", staging=info$staging, url=info$url)
createProject("test-perms", staging=info$staging, url=info$url, owners="LTLA")

test_that("permission setting works as expected", {
    until <- round(Sys.time() + 1000000)
    setPermissions("test-perms",
        owners="jkanche", 
        uploaders=list(
           list(id="lawremi", until=until)
        ),
        staging=info$staging,
        url=info$url,
        registry=info$registry
    )

    perms <- fetchPermissions("test-perms", registry=info$registry)
    expect_identical(perms$owners, list("LTLA", "jkanche"))
    expect_identical(length(perms$uploaders), 1L)
    expect_identical(perms$uploaders[[1]]$id, "lawremi")
    expect_equal(perms$uploaders[[1]]$until, until)
    expect_null(perms$global_write)

    # Checking uploader appending, while also checking owners=NULL.
    setPermissions("test-perms", uploaders=list(list(id="ArtifactDB-bot", trusted=TRUE)), staging=info$staging, url=info$url, registry=info$registry)
    perms <- fetchPermissions("test-perms", registry=info$registry)
    expect_identical(perms$owners, list("LTLA", "jkanche"))
    expect_identical(length(perms$uploaders), 2L)
    expect_identical(perms$uploaders[[1]]$id, "lawremi")
    expect_identical(perms$uploaders[[2]]$id, "ArtifactDB-bot")
    expect_true(perms$uploaders[[2]]$trusted)

    # Checking union of owners, and also that uploaders=NULL works.
    setPermissions("test-perms", owners=c("PeteHaitch", "LTLA"), staging=info$staging, url=info$url, registry=info$registry)
    perms <- fetchPermissions("test-perms", registry=info$registry)
    expect_identical(perms$owners, list("LTLA", "jkanche", "PeteHaitch"))
    expect_identical(length(perms$uploaders), 2L)

    # Resetting the owners back.
    setPermissions("test-perms", owners="LTLA", append=FALSE, staging=info$staging, url=info$url, registry=info$registry)
    perms <- fetchPermissions("test-perms", registry=info$registry)
    expect_identical(perms$owners, list("LTLA"))
    expect_identical(length(perms$uploaders), 2L)

    # Now resetting the uploaders. 
    setPermissions("test-perms", uploaders=list(), append=FALSE, staging=info$staging, url=info$url, registry=info$registry)
    perms <- fetchPermissions("test-perms", registry=info$registry)
    expect_identical(perms$owners, list("LTLA"))
    expect_identical(length(perms$uploaders), 0L)

    # Enabling global writes.
    setPermissions("test-perms", globalWrite=TRUE, staging=info$staging, url=info$url, registry=info$registry)
    perms <- fetchPermissions("test-perms", registry=info$registry)
    expect_true(perms$global_write)
    setPermissions("test-perms", globalWrite=FALSE, staging=info$staging, url=info$url, registry=info$registry)
    perms <- fetchPermissions("test-perms", registry=info$registry)
    expect_false(perms$global_write)
})
