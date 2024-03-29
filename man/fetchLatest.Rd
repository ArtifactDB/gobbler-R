% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchLatest.R
\name{fetchLatest}
\alias{fetchLatest}
\title{Fetch the latest version}
\usage{
fetchLatest(project, asset, registry)
}
\arguments{
\item{project}{String containing the project name.}

\item{asset}{String containing the asset name.}

\item{registry}{String containing a path to the registry.}
}
\value{
String containing the latest version of the asset. 
This may also be \code{NULL} if the asset has no (non-probational) versions.
}
\description{
Fetch the latest version of a project's asset.
}
\examples{
info <- startGobbler()

# Mocking up a few uploads.
src <- allocateUploadDirectory(info$staging)
removeAsset("test", "simple", info$staging) # clean out existing entries
res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)
res <- uploadDirectory("test", "simple", "v2", src, staging=info$staging)

# Obtaining the latest version of this asset.
fetchLatest("test", "simple", registry=info$registry)

}
\seealso{
\code{\link{refreshLatest}}, to refresh the latest version.
}
\author{
Aaron Lun
}
