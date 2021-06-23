## A loadedObjectEnv is simply an environment that holds
## an external pointer to a C++ object and/or a serialization
## of that object.  This facilitates saving and loading via
## R's serialization mechanisms.  One will first need to 
## explicitly serialize the C++ objects via a simple
## interface.  Then they can be saved as regular R
## objects.  Since R's saving and loading serialization
## manages environments correctly (i.e. without redundantly 
## saving multiple copies), the same will be true for 
## the serialized contents of nClass objects.

## The system could be organized using R6 classes or the like,
## but we do it simply with environments to keep it as light
## and low-level as possible for speed and memory efficiency.

new.loadedObjectEnv <- function(extptr = NULL) {
  ans <- new.env()
  ans$extptr <- extptr
  class(ans) <- "loadedObjectEnv"
  ans
}

is.loadedObjectEnv <- function(env) {
  ## The checks here may be over-kill.
  ## We may be able to rely solely on the class label.
  if(!is.environment(env)) return(FALSE)
  if(!exists("extptr", where = env)) return(FALSE)
  if(class(env) != "loadedObjectEnv") return(FALSE)
  TRUE
}

getExtptr <- function(env) {
    ## If env$extptr ever changes, the C++ code for as< std::shared_ptr< T > > should also be changed.
    ## This is written as a custom Exporter added to namespace Rcpp::traits
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$extptr
}

setExtptr <- function(env, xptr) {
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$extptr <- xptr
  env
}

make_DLLenv <- function(dllFuns) {
  dllEnv <- new.env(parent = getNamespace("nCompiler"))
  for (dllFun in dllFuns) {
    dllEnv[[name(dllFun)]] <- dllFun
  }
  
  class(dllEnv) <- "nC_DLL_env"
  newDLLEnv <- dllEnv
  dllEnv
}

get_DLLenv <- function(obj) {
  parent.env(obj)
}


# Returns a list of reserved auxiliary function names. For now limited to serialization utilities.
getAuxFunNames <- function() {
    getSerialFunNames()
}


## Identifies indices of DLL helper functions.
## Returns vector with slot i ==  1/0 if ans[i] is/isn't a helper function.
findDllIdx <- function(ans) {
  keep <- rep(1, if (is.list(ans)) length(ans) else 1)
  for(DLLname in getAuxFunNames()) {
    found <- grepl(DLLname, names(ans))
    if(any(found)) {
      i <- which(found)
      if(length(i) != 1)
        stop(paste("Duplicates of function name ", DLLname, " found"));
      keep[i] <- 0
    }
  }
  keep
}


## Wraps a generator inside an invoking function which also records the DLL
## environment.
wrapNCgenerator_for_DLLenv <- function(newObjFun, newDLLenv) {
  force(newDLLenv)
  force(newObjFun)
  if(!is.function(newObjFun))
      stop(paste0("newObjFun has non-function class ",
                  paste0(class(newObjFun), collapse = " ")))

  # Return value is a wrapper that invokes the generator and assigns a
  # parent environment to the generated value.  
  wrappedNewObjFun <- function() {
    ans <- newObjFun()
    parent.env(ans) <- newDLLenv
    ans
  }
  wrappedNewObjFun
}


## Next two will be deprecated

loadedObjectEnv_serialized <- function(env) {
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$serialized
}

`loadedObjectEnv_serialized<-` <- function(env, value) {
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$serialized <- value
  env
}
  
