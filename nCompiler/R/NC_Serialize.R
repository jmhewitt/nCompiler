# Code for serializing and de-serializing loaded nComp objects

# Internal function used by save_nClass() which calls the provided serialization
# function, then instantiates and returns a new loadedObjectEnv with the
# contents

loadDLLenv <- function(newLOE, loadands) {
  for (DLLname in getSerialFunNames()) {
    found <- grepl(DLLname, names(loadands))
    if (any(found)) {
      i <- which(found)
      if (length(i) != 1)
        stop(paste("Loading multiple instances of ", DLLname));
      parent.env(newLOE)[[DLLname]] <- loadands[[i]]
    }
  }
}


#' @name save_nClass
#' @title Save an instance of an nClass object across sessions
#' @description Saves an nClass, including its compiled components, across
#'   sessions using the `cereal` serialization library for C++.
#' @param ncObj A compiled instance of an nClass object. Right now this only
#'   works for
#' @param ncDef The uncompiled nClass generator used to define ncObj's nClass.
#'   Leave NULL and use the argument \code{packageWithDefn} if the nClass is
#'   defined in an existing package.
#' @param file The filename to which the serialized object will be written as
#'   RDS, probably with extension ".rds"
#' @param packageWithDefn If one exists, the name of the previously defined
#'   package that defines the nClass (character string). Leave NULL if no such
#'   package exists in which case a small package will be created. If NULL, an
#'   ncDef object must be provided.
#' @param classname If no ncDef is provided, a character string giving the class
#'   name of the ncObj. If ncDef is provided, can be left NULL.
#' @param dir The directory in which the package directory will be created and
#'   source code will be written. Ignored if packageWithDefn is not NULL.
#' @param lib The lib folder where the package will be quietly installed.
#'   Ignored if packageWithDefn is not NULL.
#' @export
#'
#' @details
#'
#' There are two parameterizations of this function. If you've defined an nClass
#' and want to save it as an instance, use:
#'
#' \code{ save_nClass(ncObj, file, ncDef) }
#'
#' In this case, a package storing the nClass definition will be created. This
#' allows the nClass info to be restored with the specific instantiated object
#' upon reading.
#'
#' If the nClass is defined in a package, and so will be found even in a new
#' (fresh) session, use:
#'
#' \code{ save_nClass(ncObj, file, packageWithDefn, classname) }
#'
#' @examples
#' set_nOption("serialize", TRUE)
#'
#' # Create a new nClass
#' nc1 <- nClass(
#'   classname = "nc1",
#'   Cpublic = list(
#'     Cv = 'numericScalar',
#'     Cx = 'integerScalar',
#'     Cfoo = nFunction(
#'       fun = function(x) {
#'         return(x+1)
#'       },
#'       argTypes = list(x = 'numericScalar'),
#'       returnType = 'numericScalar')
#'   )
#' )
#' 
#' # Compile the nClass
#' Cnc1 <- nCompile_nClass(nc1, interface = "full")
#' 
#' # Instantiate an object of the nClass
#' my_nc1_instance <- Cnc1$new()
#' my_nc1_instance$Cv <- 10
#' 
#' # Save the instance (along with its class definition)
#' save_nClass(my_nc1_instance, file = "example_save.Rds", ncDef = nc1)
#' 
save_nClass <- function(ncObj,
                        file,
                        ncDef = NULL,
                        packageWithDefn = NULL,
                        classname = NULL,
                        dir = tempdir(),
                        lib = .libPaths()[1]) {
  if (isFALSE(get_nOption("serialize"))) {
    set_nOption("serialize", TRUE)
    on.exit(set_nOption("serialize", FALSE))
  }
  if (missing(lib)) lib <- .libPaths()[1]
  if (is.null(packageWithDefn)) {
    if (is.null(ncDef)) {
      stop("In save_nClass, must provide either 'ncDef' or 'packageWithDefn'.")
    }
    package.name <- paste0("savePackageClass", 
                           gsub(pattern = "_", "", 
                                Rname2CppName(ncDef$classname)))
    createPackage <- TRUE
  } else if (is.character(packageWithDefn)) {
    package.name <- packageWithDefn
    createPackage <- FALSE
  } else {
    stop("In save_nClass, packageWithDefn must be a character string")
  }
  
  if (is.null(classname)) {
    if (is.null(ncDef))
    stop(paste0("In save_nClass, if no 'ncDef' is provided, the class name",
                " must be provided", "\n  as a character string"))
    else classname <- ncDef$classname
  }
  
  ## if (createPackage) {
  ##   serialize_fn <- get_serialize_fun(ncObj)
  ## } else {
  ##   serialize_fn <- utils::getFromNamespace(
  ##     "nComp_serialize_" 
  ##     package.name
  ##   )
  ## }
  
  if (is.loadedObjectEnv(ncObj)) {
    soe <- serialize_nComp_object(ncObj)
    listToSerialize <- list(CppObj = soe$serial,
                            constructedPackage = TRUE,
                            classname = classname,
                            package.name = package.name)
  } else if (isNC(ncObj)) { # Callee expects LOE so will throw error.
    soe <- serialize_nComp_object(ncObj$private$CppObj, serialize_fn)
    listToSerialize <- list(full = ncObj, 
                            CppObj = soe$serial,
                            constructedPackage = TRUE,
                            classname = classname,
                            package.name = package.name)
    
  } else {
    stop("Object to save, 'ncObj', must be an instance of an nClass.")
  }
  saveRDS(listToSerialize, file)
  
  if (createPackage) {
    nWritePackage(ncDef,
                 package.name = package.name,
                 dir = dir,
                 control = list(export = FALSE), 
                 modify = FALSE,
                 memberData = list(classname = classname))
    nBuildPackage(package.name = package.name, 
                 dir = dir, lib = lib, load = FALSE)
  }
  invisible(NULL)
}


#' @name read_nClass
#' @title Read an nClass object saved by \code{save_nClass}
#' @description Read an instance of an nClass saved by \code{save_nClass}.
#'   The class definition and deserialization tools (using the \code{cereal}
#'   library for C++) are loaded from the relevant package, whether it was
#'   written or generated automatically from \code{save_nClass}.
#' @param file The (probably .rds) file to which the nClass object was written
#' @param lib The lib folder where the package defining the nClass is found
#' @export
#' 
#' @examples
#' set_nOption("serialize", TRUE)
#'
#' # Create a new nClass
#' nc1 <- nClass(
#'   classname = "nc1",
#'   Cpublic = list(
#'     Cv = 'numericScalar',
#'     Cx = 'integerScalar',
#'     Cfoo = nFunction(
#'       fun = function(x) {
#'         return(x+1)
#'       },
#'       argTypes = list(x = 'numericScalar'),
#'       returnType = 'numericScalar')
#'   )
#' )
#' 
#' # Compile the nClass
#' Cnc1 <- nCompile_nClass(nc1, interface = "full")
#' 
#' # Instantiate an object of the nClass
#' my_nc1_instance <- Cnc1$new()
#' my_nc1_instance$Cv <- 10
#' 
#' # Save the instance (along with its class definition)
#' save_nClass(my_nc1_instance, file = "example_save.Rds", ncDef = nc1)
#' 
#' ### A new session can be started here
#' my_nc1_read <- read_nClass("example_save.Rds")
#' my_nc1_read$Cv
#' my_nc1_read$Cfoo(10)
#' 
read_nClass <- function(file, lib = .libPaths()[1]) {
  
  savedObj <- readRDS(file)
  if (!is.list(savedObj) || is.null(savedObj$CppObj)) {
    stop("Object in specified RDS file is not a saved nClass.")
  }

  deserialized <- deserialize_nComp_object(
    new.loadedObjectEnv(savedObj$CppObj),
    getDeserializerName(savedObj))

  # library(savedObj$package.name, character.only = TRUE, lib = lib)
  # loadEnv <- new.env()
  # data(list = "classname", package = savedObj$package.name, envir = loadEnv)
  if (!is.null(savedObj$full)) {
    rtnObjFull <- savedObj$full
    rtnObjFull$private$CppObj <- deserialized
    return(rtnObjFull)
  } else {
    return(deserialized)
  }
}


# Internal function used by read_nClass() which calls the provided
# deserialization function and applies it to a loadedObjectEnv
deserialize_nComp_object <- function(loe, deserializer) {
  if(!is.serialObjectEnv(loe))
    stop("obj must be a serialObjectEnv.")
  if (class(loe$serial) != "raw")
    stop("serialized content must have class 'raw'")
  if (missing(deserializer)) {
    deserializer <- get_deserialize_fun(loe)
  }
  newXptr <- deserializer(loe$serial)
  newLOE <- new.loadedObjectEnv(newXptr)
  parent.env(newLOE) <- parent.env(loe)
  newLOE
}


get_deserialize_fun <- function(loe) {
  deserializer <- getElement(parent.env(loe), getDeserializerFunName())
  if(!is.function(deserializer))
      stop(paste0("Deserialization function ", getDeserializerFunName(), " not found."))
  deserializer
}


getDeserializerFunName <- function() {
  "nComp_deserialize_"
}


getSerializerFunName <- function() {
  "nComp_serialize_"
}


getSerializationManagerName <- function() {
  "new_serialization_mgr"
}


getSerializationMgr <- function(LOE) {
  if (class(LOE) != "loadedObjectEnv") {
    stop(paste("Serialization manager not present in class ", class(LOE)))
  }
  mgr <- getElement(parent.env(LOE), getSerializationManagerName())
  if (is.null(mgr)) {
    stop(paste0("Manager ", getSerializationManagerName(), " not found in parent environment."))
  }
  mgr
}


# Returns the names of the internally-generated serialization helper functions.
getSerialFunNames <- function() {
  c(getSerializerFunName(), getDeserializerFunName(), getSerializationManagerName())
}

#' Constructs the name of the deserializer from the package and class name.
getDeserializerName <- function(loadedObj) {
  utils::getFromNamespace(paste0(getDeserializerFunName(), Rname2CppName(loadedObj$classname)),
                          loadedObj$package.name)
}


# Returns the serialization function associated with loe's parent environment.
get_serialize_fun <- function(loe) {
  serializer <- getElement(parent.env(loe), getSerializerFunName())
  if(!is.function(serializer))
    stop(paste0("Serializer ", getSerializerFunName(), " not found not found in parent environment."))
  serializer
}


serialize_nComp_object <- function(loe, serializer) {
  if(!is.loadedObjectEnv(loe))
    stop("obj must be a loadedObjectEnv.")

  nCompObj <- getExtptr(loe)
  if (is.null(nCompObj)) {
    warning("No nCompile object to serialize")
    return(loe)
  }

  if (missing(serializer)) {
    serializer <-get_serialize_fun(loe)
  }

  new.serialObjectEnv(serializer(nCompObj), get_DLLenv(loe))
}


new.serialObjectEnv <- function(serial_data = NULL, dll_env) {
  ans <- new.env()
  if(!missing(dll_env)) parent.env(ans) <- dll_env
  ans$serial <- serial_data
  class(ans) <- "serialObjectEnv"
  ans
}


is.serialObjectEnv <- function(env) {
  ## The checks here may be over-kill.
  ## We may be able to rely solely on the class label.
  if(!is.environment(env)) return(FALSE)
  if(!exists("serial", where = env)) return(FALSE)
  if(class(env) != "serialObjectEnv") return(FALSE)
  TRUE
}

getSerial <- function(env) {
  ## If env$extptr ever changes, the C++ code for as< std::shared_ptr< T > > should also be changed.
  ## This is written as a custom Exporter added to namespace Rcpp::traits
  if(!is.serialObjectEnv(env))
    stop("env should be a serialObjectEnv")
  env$serial
}

setSerial <- function(env, serial_data) {
  if(!is.serialObjectEnv(env))
    stop("env should be a serialObjectEnv")
  env$serial <- serial_data
  env
}
