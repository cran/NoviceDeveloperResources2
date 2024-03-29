% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NoviceDeveloperResources2.R
\name{sortedInputForCheckBuildInstallSourcePackage}
\alias{sortedInputForCheckBuildInstallSourcePackage}
\title{sortedInputForCheckBuildInstallSourcePackage}
\usage{
sortedInputForCheckBuildInstallSourcePackage(retrieve, burd)
}
\arguments{
\item{retrieve}{return value of retrieveLeafNodes()}

\item{burd}{return value of bottomUpRecursiveDriver()}
}
\value{
returns a list of packages in the correct order to input to
checkBuildInstallSourcePackage()
}
\description{
compute a list of packages in the correct order to input to
checkBuildInstallSourcePackage()
}
\examples{
\dontrun{
dir<-"~/inference_packages/"
packs<-c("cardUtils","clickableImageMap","editDriver",
"heartsCIM","iterationDriver","logos","playOneTrick",
"playWholeHandDriverPassParams","probTab","relaxDriver")
master<-PackageDependencies(packs,vector("list",length(packs)),1,TRUE)
l<-recursivePackageDependenciesWrapper(packs)
burd<-bottomUpRecursiveDriver(l,c("iterationDriver"),TRUE)
retrieve<-retrieveLeafNodes(master)
s<-sortedInputForCheckBuildInstallSourcePackage(retrieve,burd)
}

}
