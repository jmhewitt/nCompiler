cppFileLabelFunction <- labelFunctionCreator('nCompiler_units')

#' @export
nCompile <- function(...,
                     dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                     cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                     env = parent.frame(),
                     control = list(),
                     interfaces = list(),
                     roxygen = list(),
                     returnList = FALSE) { ## return a list even if there is only one unit being compiled.
  dotsDeparses <- unlist(lapply( substitute(list(...))[-1], deparse ))
  origList <- list(...)
  if(!is.list(interfaces))
    interfaces <- as.list(interfaces)
  if(is.null(names(origList))) 
    names(origList) <- rep('', length(origList))
  boolNoName <- names(origList)==''
  origIsList <- unlist(lapply(origList, is.list))
  for(i in which(origIsList)) {
    if(is.null(names(origList[[i]])) || any(names(origList[[i]])==""))
      stop("If you provide a list of compilation units, it must be named.")
  }
  dotsDeparses[origIsList] <- ''
  names(origList)[boolNoName] <- dotsDeparses[boolNoName]
  units <- do.call('c', origList)
  if (is.null(names(units)))
    names(units) <- rep('', length(units))
  RcppPacket_list <- compileLoop(units,
                                 names(units),
                                 env = env,
                                 control = control,
                                 combined=TRUE)

  if(isTRUE(get_nOption('serialize'))) {
    RcppPacket_list[[ length(RcppPacket_list) + 1]] <- cppDefs_2_RcppPacket(make_serialization_cppDef(), "serialization_")
  }

  if(!isTRUE(get_nOption('use_nCompLocal'))) {
    RcppPacket_list[[ length(RcppPacket_list) + 1]] <- cppDefs_2_RcppPacket(make_loadedObjectEnv_cppDef(), "loadedObjectEnv_")
  }

  

  ## Write the results jointly, with one .cpp file and multiple .h files.
  ## This fits Rcpp::sourceCpp's requirements.
  cppFile = paste0(cppFileLabelFunction(),".cpp")  ## "nCompiler_multiple_units.cpp"
  writeCpp_nCompiler_combine(RcppPacket_list,
                             cppfile=cppFile)

  if(isTRUE(get_nOption('pause_after_writing_files')))
    browser()

  resultEnv <- new.env()
  ans <- compileCpp_nCompiler(cppFile,
                              dir = dir,
                              cacheDir = cacheDir,
                              env = resultEnv,
                              returnList = returnList)
  ## 'ans' consists of all compiled function names and the corresponding environments.
  newDLLenv <- make_DLLenv()
  compiledFn <- setup_DLLenv(ans, newDLLenv, returnList)
  ## 'compiledFn' is 'ans' gleaned of any DLL-specific function/environment.
  
  ## Next we re-order results using input names,
  ## in case the ordering in the C++ code or in Rcpp's handling
  ## does not match order of units.
  ## cpp_names should be 1-to-1 with names(ans)
  ## We want to return with names(ans) changed to
  ## names(units) corresponding to cpp_names.
  cpp_names <- sapply(units, function(unit) {
    if (isNF(unit)) NFinternals(unit)$cpp_code_name
    else unit$classname
  })

  if(is.list(compiledFn)) {
    newNames <- names(compiledFn)
    SEXPgen_names <- paste0("new_", cpp_names)
    for(i in seq_along(units)) {
      iRes <- which(SEXPgen_names[i] == names(compiledFn))
      if(length(iRes) != 1) {
        warning("Name matching of results had a problem.  Returning list of compiled results with internal C++ names.")
        return(compiledFn)
      }
      newNames[iRes] <- names(units)[i]

      if (isNCgenerator(units[[i]])) {
        nClass_name <- cpp_names[i]
        interfaceType <- if (is.null(interfaces[[nClass_name]])) "generic" else interfaces[[nClass_name]]
        compiledFn[[iRes]] <- setup_nClass_interface(interfaceType,
                                              units[[i]],
                                              compiledFn[[iRes]],
                                              newDLLenv,
                                              env = resultEnv)        
      }
    }
    names(compiledFn) <- newNames
  } else {
    if (isNCgenerator(units[[1]])) {
      interfaceType <- if (length(interfaces) == 0 || is.null(interfaces[[1]])) "full" else interfaces[[1]]
      compiledFn <- setup_nClass_interface(interfaceType,
                                           units[[1]],
                                           compiledFn,
                                           newDLLenv,
                                           env = resultEnv)
    }
  }
  compiledFn
}


setup_nClass_interface <- function(interfaceType,
                                   NC,
                                   compiledFn,
                                   newDLLenv,
                                   env,
                                   tryError = TRUE) {
  wrappedFn <- wrapNCgenerator_for_DLLenv(compiledFn, newDLLenv)
  if (interfaceType == "generic")
    return(wrappedFn)

  ## To Do: Only "generic" works when more than one function will be returned from sourceCpp in cpp_nCompiler.  That occurs with serialization turned on.

  fullInterface <- if (tryError) try(build_compiled_nClass(NC, wrappedFn, env = env))
  else build_compiled_nClass(NC, wrappedFn, env=env)

  if (inherits(fullInterface, "try-error")) {
      warning("There was a problem building a full nClass interface.\n",
              "Attempting to return a generic interface.\n")
      return(wrappedFn)
  }
  else if(interfaceType == "full")
    return(fullInterface)
  else if(interfaceType == "both")
    return(list(full = fullInterface, generic = wrappedFn))
  else {
    warning(paste0("Invalid interface type ", interfaceType, " requested.\n",
                   "Returning a full interface.\n"))
    return(fullInterface)
  }
}
  
