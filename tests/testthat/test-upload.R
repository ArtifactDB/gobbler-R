# This tests the upload functions. 
# library(testthat); library(gobbler); source("test-upload.R")

info <- startGobbler()
removeProject("test-upload", staging=info$staging)
removeProject("test-more-upload", staging=info$staging)
removeProject("test-upload-perms", staging=info$staging)

tmp <- tempfile()
dir.create(tmp)
write(file=file.path(tmp, "blah.txt"), LETTERS)
dir.create(file.path(tmp, "foo"))
write(file=file.path(tmp, "foo", "bar.txt"), 1:10)

test_that("upload works as expected for regular files", {
    init <- uploadDirectory(
        project="test-upload", 
        asset="jennifer", 
        version="1", 
        dir=tmp,
        staging=info$staging
    )
    expect_identical(init$project, "test-upload")
    expect_identical(init$version, "1")

    # Checking that the files were, in fact, correctly uploaded.
    man <- fetchManifest("test-upload", "jennifer", "1", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))
    expect_false(any(vapply(man, function(x) !is.null(x$link), TRUE)))

    # Deduplication happens naturally.
    init <- uploadDirectory(
        project="test-upload", 
        asset="jennifer", 
        version="2", 
        dir=tmp,
        staging=info$staging
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
        dir=dest,
        staging=info$staging
    )

    man <- fetchManifest("test-more-upload", "natalie", "1", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt", "whee"))
    expect_false(is.null(man[["blah.txt"]]$link))
    expect_false(is.null(man[["foo/bar.txt"]]$link))
    expect_null(man[["whee"]]$link)
})

test_that("upload works as expected for project/version series", {
    init <- uploadDirectory(
        project=NULL,
        prefix="UPLOAD",
        asset="versioning", 
        version=NULL, 
        dir=tmp,
        staging=info$staging
    )
    expect_true(startsWith(init$project, "UPLOAD"))
    expect_identical(init$version, "1")
})

test_that("upload works as expected for direct allocations", {
    dir <- allocateUploadDirectory(info$staging)
    write(file=file.path(dir, "blah.txt"), LETTERS)
    dir.create(file.path(dir, "foo"))
    write(file=file.path(dir, "foo", "bar.txt"), 1:10)

    init <- uploadDirectory(
        project="test-upload", 
        asset="jennifer", 
        version="3", 
        dir=dir,
        staging=info$staging
    )
    expect_identical(init$project, "test-upload")
    expect_identical(init$version, "3")

    man <- fetchManifest("test-upload", "jennifer", "1", registry=info$registry)
    expect_identical(sort(names(man)), c("blah.txt", "foo/bar.txt"))
})

test_that("upload works as expected for complex permissions", {
    init <- uploadDirectory(
        project="test-upload-perms",
        asset="annabelle", 
        version=NULL, 
        dir=tmp,
        owners=c("LTLA", "jkanche"),
        uploaders=list(list(id="lawremi"), list(id="PeteHaitch", until=Sys.time() + 100)),
        staging=info$staging
    )
    expect_identical(init$project, "test-upload-perms")
    expect_identical(init$version, "1")

    perms <- fetchPermissions("test-upload-perms", registry=info$registry)
    expect_identical(perms$owners, list("LTLA", "jkanche"))
    expect_identical(perms$uploaders[[1]]$id, "lawremi")
    expect_identical(perms$uploaders[[2]]$id, "PeteHaitch")
    expect_s3_class(perms$uploaders[[2]]$until, "POSIXct")
})
