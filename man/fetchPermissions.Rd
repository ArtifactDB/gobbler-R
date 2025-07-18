% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchPermissions.R
\name{fetchPermissions}
\alias{fetchPermissions}
\title{Fetch project permissions}
\usage{
fetchPermissions(project, registry, url, asset = NULL, forceRemote = FALSE)
}
\arguments{
\item{project}{String containing the project name.}

\item{registry}{String containing a path to the registry.}

\item{url}{String containing the URL to the Gobbler REST API.
Only used for remote access.}

\item{asset}{String containing the asset name.
If specified, permissions are retrieved for the asset rather than the entire project.}

\item{forceRemote}{Logical scalar indicating whether to force remote access via the API,
even if \code{registry} is on the same filesystem as the caller.}
}
\value{
List containing the permissions for this project/asset.
For project-level permissions, the list has the following elements:
\itemize{
\item \code{owners}, a character vector containing the user IDs of owners of this project.
\item \code{uploaders}, a list of lists specifying the users or organizations who are authorzied to upload to this project.
Each entry is a list with the following fields:
\itemize{
\item \code{id}, a string containing a user ID that is authorized to upload.
\item (optional) \code{asset}, a string containing the name of the asset that the uploader is allowed to upload to.
If not provided, there is no restriction on the uploaded asset name.
\item (optional) \code{version}, a string containing the name of the version that the uploader is allowed to upload to.
If not provided, there is no restriction on the uploaded version name.
\item (optional) \code{until}, a \link{POSIXct} object containing the expiry date of this authorization.
If not provided, the authorization does not expire.
\item (optional) \code{trusted}, a logical scalar indicating whether the uploader is trusted.
If not provided, defaults to \code{FALSE}.
}
\item (optional) \code{global_write}, a logical scalar indicating whether global writes are enabled.
In this mode, any user can create any number of new assets in this project.
Each user can also upload new versions of any asset that they created in this mode.
}
For asset-level permissions, the list has \code{owners} and \code{uploaders} to describe the owners and uploaders, respectively, for the specified \code{asset}.
}
\description{
Fetch the permissions for a project.
This will call the REST API if the caller is not on the same filesystem as the registry.
}
\examples{
info <- startGobbler()
removeProject("test", info$staging, url=info$url) # start with a clean slate.

# Mocking up a project.upload.
createProject("test", info$staging, url=info$url,
    uploaders=list(list(id="urmom", until=Sys.time() + 1000)))

# Fetching the permissions.
fetchPermissions("test", registry=info$registry, url=info$url)

# Forcing remote access.
fetchPermissions("test", registry=info$registry, url=info$url, forceRemote=TRUE)

}
\seealso{
\code{\link{setPermissions}}, to set the permissions.
}
\author{
Aaron Lun
}
