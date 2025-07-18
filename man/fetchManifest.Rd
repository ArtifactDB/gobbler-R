% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchManifest.R
\name{fetchManifest}
\alias{fetchManifest}
\title{Fetch version manifest}
\usage{
fetchManifest(
  project,
  asset,
  version,
  registry,
  url,
  cache = NULL,
  forceRemote = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{project}{String containing the project name.}

\item{asset}{String containing the asset name.}

\item{version}{String containing the version name.}

\item{registry}{String containing a path to the registry.}

\item{url}{String containing the URL to the Gobbler REST API.
Only used for remote access.}

\item{cache}{String containing a path to a cache directory.
If \code{NULL}, an appropriate location is automatically chosen.
Only used for remote access.}

\item{forceRemote}{Logical scalar indicating whether to force remote access.
This will download all files in the \code{path} via the REST API and cache them locally,
even if \code{registry} is on the same filesystem as the caller.}

\item{overwrite}{Logical scalar indicating whether to overwrite the existing cache.
Only used for remote access.}
}
\value{
List containing the manifest for this version.
Each element is named after the relative path of a file in this version.
The value of each element is another list with the following fields:
\itemize{
\item \code{size}, an integer specifying the size of the file in bytes.
\item \code{md5sum}, a string containing the hex-encoded MD5 checksum of the file.
\item \code{link} (optional): a list specifying the link destination for a file.
This contains the strings \code{project}, \code{asset}, \code{version} and \code{path}.
If the link destination is itself a link, an \code{ancestor} list will be present that specifies the final location of the file after resolving all intermediate links.
}
}
\description{
Fetch the manifest for a version of an asset of a project.
This will call the REST API if the caller is not on the same filesystem as the registry.
}
\examples{
info <- startGobbler()
removeProject("test", info$staging, url=info$url) # start with a clean slate.
createProject("test", info$staging, url=info$url)

# Mocking up an upload.
src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
dir.create(file.path(src, "whee"))
write(file=file.path(src, "whee", "blah"), "stuff")
uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)

# Obtaining the manifest for this version.
fetchManifest("test", "simple", "v1", registry=info$registry, url=info$url)

# Force remote access.
fetchManifest(
    "test",
    "simple",
    "v1",
    registry=info$registry,
    url=info$url,
    forceRemote=TRUE
)
}
\author{
Aaron Lun
}
