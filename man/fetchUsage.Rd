% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchUsage.R
\name{fetchUsage}
\alias{fetchUsage}
\title{Fetch project usage details}
\usage{
fetchUsage(project, registry)
}
\arguments{
\item{project}{String containing the project name.}

\item{registry}{String containing the path to the registry.}
}
\value{
Numeric scalar specifying the quota usage for the project, in bytes.
}
\description{
Fetch the quota usage for a project.
}
\examples{
info <- startGobbler()

# Mocking up an upload. 
src <- allocateUploadDirectory(info$staging)
write(file=file.path(src, "foo"), "BAR")
write(file=file.path(src, "whee"), "stuff")
removeProject("test", info$staging) # clean out any existing entry
res <- uploadDirectory("test", "simple", "v1", src, staging=info$staging)

# Obtaining the project usage.
fetchUsage("test", registry=info$registry)

}
\seealso{
\code{\link{refreshUsage}}, to recompute the used quota.
}
\author{
Aaron Lun
}
