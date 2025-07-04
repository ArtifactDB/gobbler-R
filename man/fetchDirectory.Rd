% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchDirectory.R
\name{fetchDirectory}
\alias{fetchDirectory}
\title{Fetch a directory from the registry}
\usage{
fetchDirectory(
  path,
  registry,
  url,
  cache = NULL,
  forceRemote = FALSE,
  overwrite = FALSE,
  concurrent = 1
)
}
\arguments{
\item{path}{String containing the relative path to a subdirectory within the registry.
This usually takes the form of \code{PROJECT/ASSET/VERSION/*}, where path components should be separated by \code{/}.}

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

\item{concurrent}{Integer specifying the number of concurrent downloads.
Only used for remote access.}
}
\value{
Path to the subdirectory on the caller's filesystem.
This is either a path to the registry if it is accessible,
or a path to a local cache of the registry's contents otherwise.
}
\description{
Obtain a path to a subdirectory in the registry,
possibly creating a local copy of the subdirectory's contents if the caller is not on the same filesystem as the registry.
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
write(file=file.path(src, "whee2"), "more-stuff")
uploadDirectory("test", "simple", "v1", src, staging=info$staging, url=info$url)

# Now fetching the directory.
dir <- fetchDirectory("test/simple/v1", registry=info$registry, url=info$url)
dir
list.files(dir, recursive=TRUE)

# Or, forcing remote access:
cache <- tempfile()
dir1 <- fetchDirectory("test/simple/v1",
    registry=info$registry,
    url=info$url,
    cache=cache,
    forceRemote=TRUE
)
dir1
list.files(dir1, recursive=TRUE)

}
\author{
Aaron Lun
}
