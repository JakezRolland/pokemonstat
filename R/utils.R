
#' onError
#'
#' @param err error message in the try catch
#' @param functionName name of function the try catch is located
#' @param step name of the step in the function
#' @export

onError<-function(err,functionName,   step){
  message_traceback<-err$message;
  errormessage = paste(paste("at ",functionName," :: ",step,sep=""),message_traceback,sep='\n')
  stop(errormessage,call. = FALSE)
}

#' getCurrentRVersion
#'
#' @export
getCurrentRVersion<-function(){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    Rinfos = R.Version();
    version = paste(Rinfos$major,Rinfos$minor,sep=".")
    return(version)
  }, error = function(err) onError(err,functionName,step ))
}


#' getLibPath
#' @export
getLibPath<-function(){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    return(paste(R.home(),"/library",sep=""));
  }, error = function(err) onError(err,functionName,step ))
}



#' installPackages
#'
#' @param packages_to_install  list of R packages to install
#'
#'
#' @export
#'
installRPackages<-function(packages_to_install){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    to_install<-packages_to_install %in% rownames(installed.packages(getLibPath()))
    print(packages_to_install[!to_install])
    if (FALSE %in% to_install){
      install.packages(packages_to_install[!to_install],dependencies=TRUE,repos='http://cran.us.r-project.org',lib=getLibPath())
    }
  }, error = function(err) onError(err,functionName,step ))
}
#' libraryPackages
#'
#' @param packages_to_install  list of R packages to install
#'
#'
#' @export
#'
libraryPackages<-function(packages_to_install){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({

    lapply(packages_to_install, require, character.only = TRUE, quietly=TRUE,lib.loc=getLibPath());

  }, error = function(err) onError(err,functionName,step ))
}



#' installandLoadGitHubPackage
#'
#' @param user_repo_pat user/repo/pat
#'
#' @importFrom withr with_libpaths
#' @import devtools
#' @export
installGitHubPackage <- function(user_repo_pat){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
     split = strsplit(user_repo_pat,split = "/")[[1]]
    packageName = split[2]
    pat =split[3]
    user_repo = paste(split[1],split[2],sep="/")
    if(is.na(pat)){
      with_libpaths(new = getLibPath(), install_github(user_repo))
    }
    else {
      with_libpaths(new = getLibPath(), install_github(user_repo,auth_token = pat))
    }
    #lapply(packageName, require, character.only = TRUE, quietly=TRUE,lib.loc=getLibPath());
  }, error = function(err) onError(err,functionName,step ))
}


#' libraryGitHubPackage
#'
#' @param user_repo_pat user/repo/pat if private
#' @export
libraryGitHubPackage <- function(user_repo_pat){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    split = strsplit(user_repo_pat,split = "/")[[1]]
    packageName = split[2]

    lapply(packageName, require, character.only = TRUE, quietly=TRUE,lib.loc=getLibPath());
  }, error = function(err) onError(err,functionName,step ))
}

