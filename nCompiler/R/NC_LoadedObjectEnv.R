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


#' Embeds a compiled class generator within a method returning the DLLenv manager.
#' @return embedded generator method.
dllEmbedGenerator <- function(generatorFun, mgr) {
  force(generatorFun)
  if (!is.function(generatorFun))
      stop(paste0("generator function has non-function class ",
                  paste0(class(generatorFun), collapse = " ")))

  # Return value is a method which both invokes the generator and assigns the
  # DLL environment to the newly generated object.
  embeddedGeneratorFun <- function() {
    newObj <- generatorFun()
    parent.env(newObj) <- mgr()
    newObj
  }
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
  
