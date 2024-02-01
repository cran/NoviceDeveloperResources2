% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NoviceDeveloperResources2.R
\name{bottomUpRecursiveDriver}
\alias{bottomUpRecursiveDriver}
\title{bottomUpRecursiveDriver}
\usage{
bottomUpRecursiveDriver(l, p0, verbose)
}
\arguments{
\item{l}{return value of recursivePackageDependenciesWrapper()}

\item{p0}{list of those packages whose R code has been modified by the developer}

\item{verbose}{if TRUE print line indicating the recursion level}
}
\value{
returns a list of all the packages that either directly or indirectly imports the original query packages
}
\description{
compute a list of all the packages that either directly or indirectly import the original query packages
}
\examples{
\dontrun{
packs<-c("cardUtils","clickableImageMap","editDriver",
"heartsCIM","iterationDriver","logos","playOneTrick",
"playWholeHandDriverPassParams","probTab","relaxDriver")
l<-recursivePackageDependenciesWrapper(packs)
burd<-bottomUpRecursiveDriver(l,c("iterationDriver"),TRUE)
}

}