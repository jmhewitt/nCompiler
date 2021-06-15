
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
CncTerm <- nClass( # Terminal class.
  classname = "Cnc",
  Cpublic = list(
    Cnum = 'numericScalar',
    Cint = 'integerScalar'
  )
)
CintInit = -455
CnumInit = 3.1416

CncOneObj <- nClass( # Single terminal-object member.
  classname = "CncOneObj",
  Cpublic = list(
    cncTerm = 'CncTerm'
  )
)

CncTwoObj <- nClass( # Two single-object members.
  classname = "CncTwoObj",
  Cpublic = list(
    cncOneObjA = 'CncOneObj',
    cncOneObjB = 'CncOneObj'
  )
)


Rnc <- nClass(
  classname = "Rnc",
  Rpublic = list(),
  Cpublic = list(
    cncTwoObj = 'CncTwoObj',
    check = 'integerScalar'
  )
)
checkInit = 1483

comp <- nCompile(CncTerm, CncOneObj, CncTwoObj, Rnc, interfaces = list(CncTerm="generic", CncOneObj = "generic", CncTwoObj = "generic", Rnc = "generic"))

ncTerm <- comp[[1]]()
ncOneObjA <- comp[[2]]()
ncOneObjB <- comp[[2]]()
ncTwoObj <- comp[[3]]()
Rnc1 <- comp[[4]]()

value(ncOneObjA, 'cncTerm') <- ncTerm
value(ncOneObjB, 'cncTerm') <- ncTerm
value(ncTwoObj, 'cncOneObjA') <- ncOneObjA
value(ncTwoObj, 'cncOneObjB') <- ncOneObjB
value(Rnc1, 'cncTwoObj') <- ncTwoObj
value(Rnc1, 'check') <- checkInit

value(ncTerm, 'Cint') <- CintInit
value(ncTerm, 'Cnum') <- CnumInit

# Are the initialized values accessible?
paste0(CnumInit, " =?= ", value(value(value(value(Rnc1, 'cncTwoObj'), 'cncOneObjA'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(value(Rnc1, 'cncTwoObj'), 'cncOneObjA'), 'cncTerm'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(value(value(Rnc1, 'cncTwoObj'), 'cncOneObjB'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(value(Rnc1, 'cncTwoObj'), 'cncOneObjB'), 'cncTerm'), 'Cint'))


# Is the class initialization accsssible via the top-most aggregate?    
cncTwoObj <- value(Rnc1, 'cncTwoObj')
paste0(CnumInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjA'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjA'), 'cncTerm'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjB'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjB'), 'cncTerm'), 'Cint'))

LOE <- serdes(Rnc1)

# Does the scalar initializations survive serdes?
paste0(checkInit, " =?= ", value(LOE, 'check'))

# Have the initialized values survived.
paste0(CnumInit, " =?= ", value(value(value(value(LOE, 'cncTwoObj'), 'cncOneObjA'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(value(LOE, 'cncTwoObj'), 'cncOneObjA'), 'cncTerm'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(value(value(LOE, 'cncTwoObj'), 'cncOneObjB'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(value(LOE, 'cncTwoObj'), 'cncOneObjB'), 'cncTerm'), 'Cint'))


# Has the top-most aggregate survived?
cncTwoObj <- value(LOE, 'cncTwoObj')
paste0(CnumInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjA'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjA'), 'cncTerm'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjB'), 'cncTerm'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(cncTwoObj, 'cncOneObjB'), 'cncTerm'), 'Cint'))
