
# Tests serialization of generic objects in the presence of aliasing.

library(nCompiler)

set_nOption("serialize", TRUE)

# Serializes and deserializes a single object.
# Returns the deserialized LOE.
# Makes a serialization manager from whatever DLL-charged LOE is handy.
# Anything constructed using a generic interface should suffice.
serdes <- function(obj) {
  mgr <- nCompiler:::getSerializationMgr(obj)()
  serial_index <- method(mgr, 'add_extptr')(nCompiler:::getExtptr(obj))
  soeMgr <- serialize_nComp_object(mgr, serializer = nCompiler:::get_serialize_fun(obj))
  desoeMgr <- deserialize_nComp_object(soeMgr, nCompiler:::get_deserialize_fun(obj))
  new.loadedObjectEnv(method(desoeMgr, "get_extptr")(serial_index))
}

#
# Cycle test.
#
# Rnc1 -> ncTwoObj -> {ncOneObjA, ncOneObjB} -> ncTerm.
#
#  serialize:  Rnc1
#
Cnc <- nClass( # Terminal class.
  classname = "Cnc",
  Cpublic = list(
    Cnum = 'numericScalar',
    Cint = 'integerScalar'
  )
)
CintInit = -455
CnumInit = 3.1416

Rnc <- nClass(
  classname = "Rnc",
  Rpublic = list(),
  Cpublic = list(
    cnc = 'Cnc',
    check = 'integerScalar'
  )
)
checkInit1 = 1483
checkInit2 = 1713

comp <- nCompile(Cnc, Rnc, interfaces = list(CncTerm="generic", CncOneObj = "generic"))

nc1 <- comp[[1]]()
Rnc1 <- comp[[2]]()
Rnc2 <- comp[[2]]()
nc2 <- nc1

value(nc1, 'Cint') <- CintInit
value(nc1, 'Cnum') <- CnumInit
nc1 <- nc2

value(Rnc1, 'check') <- checkInit1
value(Rnc2, 'check') <- checkInit2

value(Rnc2, 'cnc') <- nc2
value(Rnc1, 'cnc') <- nc1

# Are the initialized values accessible?
paste0(CnumInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(Rnc2, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc2, 'cnc'), 'Cint'))

# Is the class initialization accsssible via the aggregate?    
cnc1 <- value(Rnc1, 'cnc')
cnc2 <- value(Rnc2, 'cnc')
paste0(CnumInit, " =?= ", value(cnc1, 'Cnum'))
paste0(CintInit, " =?= ", value(cnc1, 'Cint'))
paste0(CnumInit, " =?= ", value(cnc2, 'Cnum'))
paste0(CintInit, " =?= ", value(cnc2, 'Cint'))

LOE1 <- serdes(Rnc1)
LOE2 <- serdes(Rnc2)

# Have the scalar initializations survived serdes?
paste0(checkInit1, " =?= ", value(LOE1, 'check'))
paste0(checkInit2, " =?= ", value(LOE2, 'check'))

# Are the initialized values accessible?
paste0(CnumInit, " =?= ", value(value(LOE1, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE1, 'cnc'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(LOE2, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE2, 'cnc'), 'Cint'))

# Is the class initialization accsssible via the aggregate?    
cnc1 <- value(LOE1, 'cnc')
cnc2 <- value(LOE2, 'cnc')
paste0(CnumInit, " =?= ", value(cnc1, 'Cnum'))
paste0(CintInit, " =?= ", value(cnc1, 'Cint'))
paste0(CnumInit, " =?= ", value(cnc2, 'Cnum'))
paste0(CintInit, " =?= ", value(cnc2, 'Cint'))
