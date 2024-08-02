#' Create a nimbleFunction that wraps a call to external compiled code
#'
#' Given C header information, a function that takes scalars or pointers can be called from a compiled nimbleFunction.  If non-scalar return values are needed, an argument can be selected to behave as the return value in nimble.
#'
#' @param prototype Argument type information.  This can be provided as an R function using \code{nimbleFunction} type declarations or as a list of \code{nimbleType} objects.
#' @param returnType Return object type information.  This can be provided similarly to \code{prototype} as either a \code{nimbleFunction} type declaration or as a \code{nimbleType} object.  In the latter case, the name will be ignored. If there is no return value, this should be \code{void()}.
#' @param Cfun Name of the external function (character).
#' @param headerFile Name (possibly including file path) of the header file where Cfun is declared.
#' @param oFile Name (possibly including path) of the .o file where Cfun has been compiled.  Spaces in the path may cause problems.
#' @param where An optional \code{where} argument passed to \code{setRefClass} for where the reference class definition generated for this nimbleFunction will be stored.  This is needed due to R package namespace issues but should never need to be provided by a user.
#'
#' @author Perry de Valpine
#' @export
#' @details The only argument types allowed in Cfun are \code{double}, \code{int}, and \code{bool}, corresponding to \code{nimbleFunction} types \code{double}, \code{integer}, and \code{logical}, respectively.
#'
#' If the dimensionality is greater than zero, the arguments in \code{Cfun} should be pointers.  This means it will typically be necessary to pass additional integer arguments telling \code{Cfun} the size(s) of non-scalar arguments.
#'
#' The return argument can only be a scalar or void.  Since non-scalar arguments are passed by pointer, you can use an argument to return results from \code{Cfun}.  If you wish to have a \code{nimbleFunction} that uses one argument of \code{Cfun} as a return object, you can wrap the result of \code{nimbleExternalCall} in another \code{nimbleFunction} that allocates the return object.  This is useful for using \code{Cfun} in a \code{nimbleModel}.  See example below.
#'
#' Note that a \code{nimbleExternalCall} can only be executed in a compiled \code{nimbleFunction}, not an uncompiled one.
#' 
#' If you have problems with spaces in file paths (e.g. for \code{oFile}), try compiling everything locally by including \code{dirName = "."} as an argument to \code{compileNimble}.
#' 
#' @return A \code{nimbleFunction} that takes the indicated input arguments, calls \code{Cfun}, and returns the result.
#'
#' @seealso \code{\link{nimbleRcall}} for calling arbitrary R code from compiled \code{nimbleFunction}s.
#' 
#' @examples
#' \dontrun{
#' sink('add1.h')
#' cat('
#'  extern "C" {
#'  void my_internal_function(double *p, double*ans, int n);
#'  }
#' ')
#' sink()
#' sink('add1.cpp') 
#' cat('
#'  #include <cstdio>
#'  #include "add1.h"
#'  void my_internal_function(double *p, double *ans, int n) {
#'    printf("In my_internal_function\\n");
#'      /* cat reduces the double slash to single slash */ 
#'    for(int i = 0; i < n; i++) 
#'      ans[i] = p[i] + 1.0;
#'  }
#' ')
#' sink()
#' system('g++ add1.cpp -c -o add1.o')
#' Radd1 <- nimbleExternalCall(function(x = double(1), ans = double(1),
#' n = integer()){}, Cfun =  'my_internal_function',
#' headerFile = file.path(getwd(), 'add1.h'), returnType = void(),
#' oFile = file.path(getwd(), 'add1.o'))
#' ## If you need to use a function with non-scalar return object in model code,
#' ## you can wrap it  in another nimbleFunction like this:
#' model_add1 <- nimbleFunction(
#'      run = function(x = double(1)) {
#'          ans <- numeric(length(x))
#'          Radd1(x, ans, length(x))
#'          return(ans)
#'          returnType(double(1))
#'      })
#' demoCode <- nimbleCode({
#'      for(i in 1:4) {x[i] ~ dnorm(0,1)} ## just to get a vector
#'      y[1:4] <- model_add1(x[1:4])
#' })
#' demoModel <- nimbleModel(demoCode, inits = list(x = rnorm(4)),
#' check = FALSE, calculate = FALSE)
#' CdemoModel <- compileNimble(demoModel, showCompilerOutput = TRUE)
#' }
nExternalCall <- function(
    prototype, refArgs = list(), returnType, Cfun, headerFile, cppFile = NULL, 
    where = parent.frame()
) {
    ## construct an nFunction to wrap a call to Cfun
    force(where)
    returnTypeExpr <- substitute(returnType)
    if(!is.function(prototype)) stop("Invalid prototype argument")
    fun <- prototype
    args <- formals(fun)
    argsSymTab <- argTypeList2symbolTable(args)
    argNames <- names(args)
    replacedArgNames <- argNames
    ## Populate a set of lines to convert from NimArr<>s to C pointers...
    convertLines <- list()
    ## ... and back again
    unconvertLines <- list()
    # for(i in seq_along(args)) {
    #     thisSymbol <- argsSymTab$getSymbolObject( argNames[i] )
    #     thisType <- thisSymbol$type
    #     thisNdim <- thisSymbol$nDim
    #     if(thisNdim > 0) {
    #         ## for argument "x", make an x_internalPOINTER_
    #         ptrName <- paste0(argNames[i], '_internalPOINTER_')
    #         replacedArgNames[i] <- ptrName
    #         ## Make line "x_internalPOINTER_ <- nimbleConvert(x)"
    #         newConvertLine <- substitute( A <- nimbleConvert(B), list(A = as.name(ptrName), B = as.name(argNames[i])))
    #         convertLines[[ length(convertLines) + 1]] <- newConvertLine
    #         ## Make line "nimbleUnconvert(x_internalPOINTER_, x)"
    #         newUnconvertLine <- substitute( nimbleUnconvert(A, B), list(A = as.name(ptrName), B = as.name(argNames[i])))
    #         unconvertLines[[ length(unconvertLines) + 1]] <- newUnconvertLine
    #     }
    # }
    # if(inherits(try(returnType, silent = TRUE), 'nimbleType'))
    #     returnType <- nimbleType2argType(returnType)[[1]]
    # else
    returnType <- returnTypeExpr
    returnSymbol <- argType2symbol(returnType)
    ## Make the call to Cfun, without yet a return value
    externalCallExpr <- as.call(c(list(as.name(Cfun)), lapply(replacedArgNames, as.name)))
    externalCallLine <- substitute(asReturnSymbol(ECE, type = RStype, nDim = RSnDim), list(ECE = externalCallExpr, RStype = returnSymbol$type, RSnDim = returnSymbol$nDim))
    returnLines <- list()
    # TODO: implement non-void return
    # if(returnSymbol$type != 'void') {
    #     ## Insert the return value as a variable "RETURNVALUE"
    #     externalCallLine <- substitute(RETURNVALUE <- EXTERNALCALL, list(EXTERNALCALL = externalCallLine))
    #     ## Insert "return(RETURNVALUE)" and "returnType(<correct type>)"
    #     returnLines <- list(
    #         quote(return(RETURNVALUE)),
    #         substitute(returnType(RT), list(RT = returnType))
    #     )
    # }
    ## put all the lines together
    allLines <- c(list(as.name("{")), convertLines, list(externalCallLine), unconvertLines, returnLines)
    body(fun) <- as.call(allLines)
    ans <- nFunction(fun = fun, refArgs = refArgs, check = FALSE, where = where)
    ## Stick header information into the nfMethodRC
    ans@internals$externalHincludes <- paste0('\"',headerFile,'\"')
    # allow use of header-only external c++ code
    if(!is.null(cppFile)) {
      if(grepl(" ", cppFile)) warning("The space in the cppFile name may cause a problem.")
      ans@internals$externalCPPSourceFiles <- cppFile
    }
    return(ans)
}
