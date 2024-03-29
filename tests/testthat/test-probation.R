# This tests the probation approval functions. 
# library(testthat); library(gobbler); source("test-probation.R")

info <- startGobbler()
removeAsset("test", "probation", staging=info$staging)
                                                                                             
# Mocking up an upload. 
src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
res <- uploadDirectory("test", "probation", "good", src, staging=info$staging, probation=TRUE)
res <- uploadDirectory("test", "probation", "bad", src, staging=info$staging, probation=TRUE)
                                                                                             
test_that('probation approval works as expected', {
    expect_true(fetchSummary("test", "probation", "good", registry=info$registry)$on_probation)
    expect_null(fetchLatest("test", "probation", registry=info$registry))

    approveProbation("test", "probation", "good", staging=info$staging)
    expect_null(fetchSummary("test", "probation", "good", registry=info$registry)$on_probation)
    expect_identical(fetchLatest("test", "probation", registry=info$registry), "good")
})

test_that('probation rejection works as expected', {
    expect_true(fetchSummary("test", "probation", "bad", registry=info$registry)$on_probation)

    rejectProbation("test", "probation", "bad", staging=info$staging)
    expect_false("bad" %in% listVersions("test", "probation", registry=info$registry))
})
