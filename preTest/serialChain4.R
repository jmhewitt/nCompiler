
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
# Chain test 4.
#
# Rnc1 -> nc1 -> nc2 -> nc3; Rnc2 -> nc2 and Rnc3 -> nc3.
#  nc1 and nc2 are distinct classes.
#
#  serialize:  Rnc1, Rnc2, Rnc3.
#
Cnc <- nClass(
  classname = "Cnc",
  Cpublic = list(
    Cnum = 'numericScalar',
    Cint = 'integerScalar'
  )
)
CintInit = 273
CnumInit = 1.2

Rnc1 <- nClass(
  classname = "Rnc1",
  Rpublic = list(),
  Cpublic = list(
    cnc = 'Cnc',
    check = 'integerScalar'
  )
)
checkInit = 7154

Cnc2 <- nClass(
  classname = "Cnc2",
  Cpublic = list(
    cnc = 'Cnc',
    Clog = 'logicalScalar'
  )
)
ClogInit <- TRUE

Rnc2 <- nClass(
  classname = "Rnc2",
  Rpublic = list(),
  Cpublic = list(
    cnc2 = 'Cnc2',
    check = 'integerScalar'
  )
)

comp <- nCompile(Cnc, Cnc2, Rnc1, Rnc2, interfaces = list(Cnc="generic", Cnc2 = "generic", Rnc1 = "generic", Rnc2 = "generic"))

nc1 <- comp[[1]]()
nc2 <- comp[[2]]()

Rnc1 <- comp[[3]]()
Rnc2 <- comp[[4]]()
Rnc3 <- comp[[3]]()

nc3 <- nc1

value(Rnc1, 'check') <- checkInit
value(Rnc2, 'check') <- checkInit
value(Rnc3, 'check') <- checkInit

value(Rnc3, 'cnc') <- nc3
value(Rnc2, 'cnc2') <- nc2
value(Rnc1, 'cnc') <- nc1

value(nc1, 'Cint') <- CintInit
value(nc1, 'Cnum') <- CnumInit

value(nc2, 'Clog') <- ClogInit
value(nc2, 'cnc') <- comp[[1]]() # Allocates aggregate member.
value(value(nc2, 'cnc'), 'Cint') <- CintInit
value(value(nc2, 'cnc'), 'Cnum') <- CnumInit

# Are the initialized values accessible?
paste0(CnumInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cint'))

# Are the initialized values accessible?
paste0(ClogInit, " =?= ", value(value(Rnc2, 'cnc2'), 'Clog'))
paste0(CnumInit, " =?= ", value(value(value(Rnc2, 'cnc2'), 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(Rnc2, 'cnc2'), 'cnc'), 'Cint'))

paste0(CnumInit, " =?= ", value(value(Rnc3, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc3, 'cnc'), 'Cint'))

# Is the class initialization accsssible as an aggregate?    
cncPreserial1 <- value(Rnc1, 'cnc')

# Does the aggregate reference preserve initialands?
paste0(CnumInit, " =?= ", value(cncPreserial1, 'Cnum'))
paste0(CintInit, " =?= ", value(cncPreserial1, 'Cint'))

# Is the aggregate accessible?
cncPreserial2 <- value(Rnc2, 'cnc2')

# Do the aggregate members preserve initial values?
paste0(ClogInit, " =?= ", value(cncPreserial2, 'Clog'))
paste0(CnumInit, " =?= ", value(value(cncPreserial2, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(cncPreserial2, 'cnc'), 'Cint'))

cncPreserial3 <- value(Rnc3, 'cnc')
paste0(CnumInit, " =?= ", value(cncPreserial3, 'Cnum'))
paste0(CintInit, " =?= ", value(cncPreserial3, 'Cint'))

LOE1 <- serdes(Rnc1)
LOE2 <- serdes(Rnc2)
LOE3 <- serdes(Rnc3)

# Do the scalar initializations survive serdes?
paste0(checkInit, " =?= ", value(LOE1, 'check'))
paste0(checkInit, " =?= ", value(LOE2, 'check'))
paste0(checkInit, " =?= ", value(LOE3, 'check'))

# Have the members survived as values?
paste0(CnumInit, " =?= ", value(value(LOE1, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE1, 'cnc'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(value(LOE2, 'cnc2'), 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(LOE2, 'cnc2'), 'cnc'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(LOE3, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE3, 'cnc'), 'Cint'))

# Have members surived as aggregates?
cncDeserial1 <- value(LOE1, 'cnc')
paste0(CnumInit, " =?= ", value(cncDeserial1, 'Cnum'))
paste0(CintInit, " =?= ", value(cncDeserial1, 'Cint'))

cncDeserial2 <- value(LOE2, 'cnc2')
cnc2Deserial <- value(cncDeserial2, 'cnc')
paste0(CnumInit, " =?= ", value(cnc2Deserial, 'Cnum'))
paste0(CintInit, " =?= ", value(cnc2Deserial, 'Cint'))

cncDeserial3 <- value(LOE3, 'cnc')
paste0(CnumInit, " =?= ", value(cncDeserial3, 'Cnum'))
paste0(CintInit, " =?= ", value(cncDeserial3, 'Cint'))
