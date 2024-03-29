% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkHealth.R
\name{checkHealth}
\alias{checkHealth}
\title{Check the Gobbler's health}
\usage{
checkHealth(staging, timeout = 10)
}
\arguments{
\item{staging}{String containing the path to the staging directory.}

\item{timeout}{Integer specifying the number of seconds to wait for a response.}
}
\value{
Logical scalar indicating whether the Gobbler responded within 10 seconds.
}
\description{
Check that the Gobbler is still active and monitoring the staging directory.
}
\examples{
info <- startGobbler()
checkHealth(info$staging)

stopGobbler()
checkHealth(info$staging, timeout=1)

}
\author{
Aaron Lun
}
