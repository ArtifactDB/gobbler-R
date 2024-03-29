% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/approveProbation.R
\name{approveProbation}
\alias{approveProbation}
\title{Approve a probational upload}
\usage{
approveProbation(project, asset, version, staging)
}
\arguments{
\item{project}{String containing the project name.}

\item{asset}{String containing the asset name.}

\item{version}{String containing the version name.}

\item{staging}{String containing the path to the staging directory.}
}
\value{
\code{NULL} is invisibly returned upon successful approval.
}
\description{
Pretty much as it says: approve a probational upload of a version of a project's asset.
This removes the \code{on_probation} tag from the uploaded version.
}
\examples{
info <- startGobbler()

# Mocking up an upload. 
src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
removeAsset("test", "probation", info$staging) # clean out any existing entry
res <- uploadDirectory("test", "probation", "v1", src, 
    staging=info$staging, probation=TRUE)
fetchSummary("test", "probation", "v1", registry=info$registry)

# After approval, the probation status disappears.
approveProbation("test", "probation", "v1", staging=info$staging)
fetchSummary("test", "probation", "v1", registry=info$registry)

}
\seealso{
\code{\link{rejectProbation}}, to reject the probational upload.

\code{\link{uploadDirectory}}, to specify probational uploads.
}
\author{
Aaron Lun
}
