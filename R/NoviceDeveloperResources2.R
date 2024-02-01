#' recursivePackageDependenciesWrapper
#'
#' @import utils
#' @import packrat
#' @import NoviceDeveloperResources
#'
#' @description determine which packages in packs list have length zero dependencies
#'
#' @param packs list of package names
#'
#' @details
#' the return value l is like:
#' 
#' $cardUtils
#' 
#' character(0) \[i.e., cardUtils has no further dependencies within packs list\]
#' 
#' $editDriver
#' 
#' \[1\] "cardUtils"         "clickableImageMap" "heartsCIM"         "logos"             "probTab"
#'
#' @examples
#' \dontrun{
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' l<-recursivePackageDependenciesWrapper(packs)
#' }
#'
#' @return returns a list of lists. Each component is indexed by a package name,
#'	and contains a list of package names that are imported by the index package name
#'
#' @export
recursivePackageDependenciesWrapper<-
	function(packs) {
		l<-list()
		for(pack in packs) {
			l[[pack]]<-intersect(recursivePackageDependencies(pack, ignores = "", lib.loc = .libPaths()[1]),
				packs)
		}
		return(l)
	}

#' reversePackageDependencies
#'
#' @description separate the packages in packs list having length zero or non-zero dependencies
#'
#' @param l return value of recursivePackageDependenciesWrapper()
#'
#' @examples
#' \dontrun{
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' l<-recursivePackageDependenciesWrapper(packs)
#' ll<-reversePackageDependencies(l)
#' }
#'
#' @details
#' the return value ll is like:
#' 
#' $original \[cardUtils is no longer a name of ll$original since it had length 0\]
#'
#' $original$editDriver
#'
#' \[1\] "cardUtils"         "clickableImageMap" "heartsCIM"         "logos"             "probTab"
#'
#' $zeros
#'
#' $zeros$cardUtils cardUtils is an element of ll$zeros since l\[\["cardUtils"\]\] has length 0
#'
#' \[1\] "cardUtils"
#'
#' @return returns a list whose components are 2 lists:
#'	\item{$original}{
#'	a list whose components are lists of package names that have non-zero length import dependencies
#'  ll$original is same as l, but deleting zero-length elements i.e., leaf nodes
#'	}
#'	\item{$zeros}{
#'	a list whose components are lists of package names that have zero length import dependencies (i.e., leaf nodes)
#'  ll$zeros zero-length elements, leaf nodes that had been deleted in ll$original	
#'	} 
#'
#' @export
reversePackageDependencies<-
	# ll$original is same as l, but deleting zero-length elements i.e., leaf nodes
	# ll$zeros zero-length elements, leaf nodes that had been deleted in ll$original
	function(l) {
		ll<-list()
		l0<-list()
		for(p in names(l))
			if(length(l[[p]])==0) {
				l0[[p]]=p # add this string to a list of zero-length elements
				l[[p]]<-NULL # delete zero-length elements from the top level of the original list
				}
		ll[["original"]]<-l
		ll[["zeros"]]<-l0
		return(ll)
	}
	
#' PackageDependencies
#'
#' @description recursively call recursivePackageDependencies2() and reversePackageDependencies()
#' to recursively delete leaf nodes until packs has been depleted to length zero
#'
#' @param packs list of package names
#' @param master list whose componenets are lists indexed by integer recursion level
#'	the components of each recursion level are the return values of
#'	recursivePackageDependenciesWrapper() and reversePackageDependencies()
#' @param n integer recursion level
#' @param verbose if TRUE print line indicating the recursion level
#'
#' @examples
#' \dontrun{
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' master<-PackageDependencies(packs, vector("list",length(packs)),1,TRUE)
#' }
#'
#' @details NOTE that the packages in packs do not need to be loaded or attached to the search() path
#'
#' @return returns a list whose components are lists indexed by the integer recursion level:
#'	\item{l}{
#'	return value of recursivePackageDependenciesWrapper()
#'	}
#'	\item{ll}{
#'	return value of reversePackageDependencies()	
#'	}
#'
#' @export
PackageDependencies<-
	# recursively call recursivePackageDependencies2() and reversePackageDependencies()
	# to delete leaf nodes until packs has been depleted to length zero
	# NOTE that the packages in packs do not need to be loaded or attached to the search() path
	function(packs,master,n,verbose) {
		master[[n]]<-list()
		master[[n]][["l"]]<-list()
		master[[n]][["ll"]]<-list()
		l<-recursivePackageDependenciesWrapper(packs)
		master[[n]][["l"]]<-l
		ll<-reversePackageDependencies(l)
		master[[n]][["ll"]]<-ll
		if(verbose)
			print(c("sortedInputForCheckBuildInstallSourcePackageDriver RECURSION LEVEL...",n),quote=FALSE)
		if(length(ll$original)==0)
			return(master)
		PackageDependencies(names(ll$original),master,n+1,verbose)
	}
	
#' retrieveLeafNodes
#'
#' @description compute a list of the packages in the correct order for processing by
#' checkBuildInstallSourcePackage()
#'
#' @param master return value of sortedInputForCheckBuildInstallSourcePackageDriver()
#'
#' @examples
#' \dontrun{
#' dir<-"~/inference_packages/"
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' master<-PackageDependencies(packs,vector("list",length(packs)),1,TRUE)
#' retrieve<-retrieveLeafNodes(master)
#' }
#'
#' @details the master list may contain some packages that do not need to be processed by
#'	checkBuildInstallSourcePackage(). These are weeded out by sortedInputForcheckBuildInstallSourcePackage()
#'
#' @return returns a list of the packages in the correct order for processing by
#' checkBuildInstallSourcePackage()
#'
#' @export
retrieveLeafNodes<-
	function(master) {
		# returns a list of the packages in the correct order for processing by
		# checkBuildInstallSourcePackage()
		l<-vector("list",length(master))
		for(level in 1:length(master)) {
			l[[level]]<-list()
			l[[level]]<-master[[level]][["ll"]][["zeros"]]
			}
		return(names(unlist(l)))
	}
	
#' bottomUpRecursive
#'
#' @description given a list of packages, determine which packages recursively import
#'	the packages in the list
#'
#' @param l return value of recursivePackageDependenciesWrapper()
#' @param p0 list of those packages whose R code has been modified by the developer
#'
#' @examples
#' \dontrun{
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' l<-recursivePackageDependenciesWrapper(packs)
#' bur<-bottomUpRecursive(l,c("iterationDriver"))
#' }
#'
#' @return returns a list of the original query packages plus the packages that directly import them
#'
#' @export
bottomUpRecursive<-
	# given a list of packages, which packages recursively import the packages in the list?
	# p0 should be a list of those packages that have been modified
	# then all of p0 plus those found by BottomUpRecursive() need to be
	# parameters passed to checkBuildInstallSourcePackage()
	function(l,p0) {
		ll<-list()
		for(p in names(l)) {
			w<-which(l[[p]] %in% p0) # w is the index/position of p0 within the list l[[p]]
			if(length(w)>0) {
				ll[[p]]<-p
				#print(c(p,w,l[[p]][w]))
				}
			}
		# return a list of the original query packages plus the packages that directly import them
		return(union(ll,p0))
	}

#' bottomUpRecursiveDriver
#'
#' @description compute a list of all the packages that either directly or indirectly import the original query packages
#'
#' @param l return value of recursivePackageDependenciesWrapper()
#' @param p0 list of those packages whose R code has been modified by the developer
#' @param verbose if TRUE print line indicating the recursion level
#'
#' @examples
#' \dontrun{
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' l<-recursivePackageDependenciesWrapper(packs)
#' burd<-bottomUpRecursiveDriver(l,c("iterationDriver"),TRUE)
#' }
#'
#' @return returns a list of all the packages that either directly or indirectly imports the original query packages
#'
#' @export
bottomUpRecursiveDriver<-
	# returns a list of all the packages that either directly or indirectly imports the original query packages
	# this list will be used to select the packages that need to be updated from the return value of
	# retrieve<-retrieveLeafNodes(master), while keeping the order in return value retrieve
	function(l,p0,verbose) {
		for(i in 1:20) { # set finite limits to avoid infinite loop in case of soe error condition
			if(verbose)
				print(c("bottomUpRecursiveDriver RECURSION LEVEL",i),quote=FALSE)
			if(i>1)
				if(length(oldp0)==length(p0))
					return(p0)
			oldp0<-p0
			p0<-bottomUpRecursive(l,p0)
		}	
	}

#' sortedInputForCheckBuildInstallSourcePackage
#'
#' @description compute a list of packages in the correct order to input to
#'	checkBuildInstallSourcePackage()
#'
#' @param retrieve return value of retrieveLeafNodes()
#' @param burd return value of bottomUpRecursiveDriver()
#'
#' @examples
#' \dontrun{
#' dir<-"~/inference_packages/"
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' master<-PackageDependencies(packs,vector("list",length(packs)),1,TRUE)
#' l<-recursivePackageDependenciesWrapper(packs)
#' burd<-bottomUpRecursiveDriver(l,c("iterationDriver"),TRUE)
#' retrieve<-retrieveLeafNodes(master)
#' s<-sortedInputForCheckBuildInstallSourcePackage(retrieve,burd)
#' }
#'
#' @return returns a list of packages in the correct order to input to
#'	checkBuildInstallSourcePackage()
#'
#' @export
sortedInputForCheckBuildInstallSourcePackage<-
	function(retrieve,burd) {
		return(retrieve[which(retrieve %in% burd)])
	}

#' sortedInputForCheckBuildInstallSourcePackageDriver
#'
#' @description driver to invoke sequence of functions to retrieve the correctly
#'	ordered list of packages as input and to invoke checkBuildInstallSourcePackage()
#'
#' @param dir character string containing the path name of the directory holding the package folders
#' @param packs list of package names
#' @param p0 list of those packages whose R code has been modified by the developer
#' @param verbose if TRUE print line indicating the recursion level
#'
#' @details This driver is the single master function to run in order to invoke
#'  all of the other functions in the packages *NoviceDeveloperResources* and
#'   *NoviceDeveloperResources2*.
#'   
#'   In the examples, I show the actual call using
#'    packages that are currently under development, so they are not yet available 
#'    (I expect them to be available in mid-2024).
#'
#' @examples
#' \dontrun{
#' dir<-"~/inference_packages/"
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' l<-sortedInputForCheckBuildInstallSourcePackageDriver(dir,packs,"iterationDriver",TRUE)
#' }
#'
#' @returns a list whose components are the return values of checkBuildInstallSourcePackage()
#'	and conflictOfInterestRestricted()
#'
#' @export
sortedInputForCheckBuildInstallSourcePackageDriver<-
  function(dir,packs,p0,verbose) {
    l<-list()
    master<-PackageDependencies(packs,vector("list",length(packs)),1,TRUE)
    retrieve<-retrieveLeafNodes(master)
    burd<-bottomUpRecursiveDriver(master[[1]][["l"]],p0,TRUE)
    s<-sortedInputForCheckBuildInstallSourcePackage(retrieve,burd)
    l$attach<-checkBuildInstallSourcePackage(dir,s,s[1],FALSE)
    l$conflict<-conflictOfInterestRestricted(packs)
    
    # this script can take a little while, notify user when it is finished
    alarm()  
    
    return(l)		
  }

# non-exported packrat functions
# see https://stackoverflow.com/questions/32535773/using-un-exported-function-from-another-r-package
recursivePackageDependencies<-utils::getFromNamespace("recursivePackageDependencies", "packrat")
availablePackages<-utils::getFromNamespace("availablePackages", "packrat")
isPackratModeOn<-utils::getFromNamespace("isPackratModeOn", "packrat")
readLockFilePackages<-utils::getFromNamespace("readLockFilePackages", "packrat")
lockFilePath<-utils::getFromNamespace("lockFilePath", "packrat")
combineDcfFields<-utils::getFromNamespace("combineDcfFields", "packrat")
readDcf<-utils::getFromNamespace("readDcf", "packrat")
sort_c<-utils::getFromNamespace("sort_c", "packrat")

