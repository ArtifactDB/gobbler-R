# This tests the probation approval functions. 
# library(testthat); library(gobbler); source("test-probation.R")

info <- startGobbler()
removeProject("test", staging=info$staging, url=info$url)
createProject("test", staging=info$staging, url=info$url)

# Mocking up an upload. 
src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
uploadDirectory("test", "probation", "good", src, staging=info$staging, url=info$url, probation=TRUE)
uploadDirectory("test", "probation", "bad", src, staging=info$staging, url=info$url, probation=TRUE)
                                                                                             
test_that('probation approval works as expected', {
    expect_true(fetchSummary("test", "probation", "good", registry=info$registry)$on_probation)
    expect_null(fetchLatest("test", "probation", registry=info$registry))

    approveProbation("test", "probation", "good", staging=info$staging, url=info$url)
    expect_null(fetchSummary("test", "probation", "good", registry=info$registry)$on_probation)
    expect_identical(fetchLatest("test", "probation", registry=info$registry), "good")
})

test_that('probation rejection works as expected', {
    expect_true(fetchSummary("test", "probation", "bad", registry=info$registry)$on_probation)

    rejectProbation("test", "probation", "bad", staging=info$staging, url=info$url)
    expect_false("bad" %in% listVersions("test", "probation", registry=info$registry))
})
