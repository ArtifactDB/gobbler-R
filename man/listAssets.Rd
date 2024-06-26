% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listAssets.R
\name{listAssets}
\alias{listAssets}
\title{List assets}
\usage{
listAssets(project, registry, url, forceRemote = FALSE)
}
\arguments{
\item{project}{String containing the project name.}

\item{registry}{String containing a path to the registry.}

\item{url}{String containing the URL to the Gobbler REST API.
Only used for remote access.}

\item{forceRemote}{Logical scalar indicating whether to force remote access via the API,
even if \code{registry} is on the same filesystem as the caller.}
}
\value{
Character vector of asset names.
}
\description{
List all assets in a project.
This will call the REST API if the caller is not on the same filesystem as the registry.
}
\examples{
info <- startGobbler()
removeProject("test", info$staging, url=info$url) # start with a clean slate.
createProject("test", info$staging, url=info$url)

# Mocking up a few uploads.
src <- allocateUploadDirectory(info$staging)
for (ass in c("simple", "more-simple", "even-more-simple")) {
    uploadDirectory("test", ass, "v1", src, staging=info$staging, url=info$url)
}

# Listing available assets:
listAssets("test", registry=info$registry, url=info$url)

# Force remote listing:
listAssets("test", registry=info$registry, url=info$url, forceRemote=TRUE)

}
\author{
Aaron Lun
}
