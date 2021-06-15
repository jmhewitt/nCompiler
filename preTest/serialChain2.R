
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
# Chain test 2.
#
#  As with simple chain test, but with differing class types.

Cnc <- nClass(
  classname = "Cnc",
  Cpublic = list(
    Cnum = 'numericScalar',
    Cint = 'integerScalar'
  )
)
CintInit = 273
CnumInit = 1.2

Rnc <- nClass(
  classname = "Rnc",
  Rpublic = list(),
  Cpublic = list(                # available in R or compiled version
    # field (aka member data) -- must have a type declaration
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

comp2 <- nCompile(Cnc, Cnc2, Rnc2, interfaces = list(Cnc="generic", Cnc2 = "generic", Rnc2 = "generic"))

nc1 <- comp2[[1]]()
nc2 <- comp2[[2]]()
Rnc2 <- comp2[[3]]()

value(Rnc2, 'check') <- checkInit
nc3 <- nc2
value(Rnc2, 'cnc2') <- nc3
value(nc2, 'cnc') <- nc1

value(nc1, 'Cint') <- CintInit
value(nc1, 'Cnum') <- CnumInit
value(nc2, 'Clog') <- ClogInit

# Are the initialized values accessible?
paste0(ClogInit, " =?= ", value(value(Rnc2, 'cnc2'), 'Clog'))
paste0(CnumInit, " =?= ", value(value(value(Rnc2, 'cnc2'), 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(Rnc2, 'cnc2'), 'cnc'), 'Cint'))

# Is the class initialization accsssible as an aggregate?       
cncPreserial2 <- value(Rnc2, 'cnc2')
cncPreserial2 # Should not be NULL.

# Does the aggregate reference look right?
paste0(ClogInit, " =?= ", value(cncPreserial2, 'Clog'))
paste0(CnumInit, " =?= ", value(value(cncPreserial2, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(cncPreserial2, 'cnc'), 'Cint'))

mgr <- nCompiler:::getSerializationMgr(Rnc2)()
serial_index <- method(mgr, 'add_extptr')(nCompiler:::getExtptr(Rnc2))
serial_index

LOE <- serdes(Rnc2)

# Does the scalar initialization survive serdes?
paste0(checkInit, " =?= ", value(LOE, 'check'))

# Have the members survived as values?
paste0(ClogInit, " =?= ", value(value(LOE, 'cnc2'), 'Clog'))
paste0(CnumInit, " =?= ", value(value(value(LOE, 'cnc2'), 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(value(LOE, 'cnc2'), 'cnc'), 'Cint'))

# Do members surive as aggregates?
cnc2Deserial <- value(LOE, 'cnc2')
paste0(ClogInit, " =?= ", value(cnc2Deserial, 'Clog'))
cncDeserial <- value(cnc2Deserial, 'cnc')
paste0(CnumInit, " =?= ", value(cncDeserial, 'Cnum'))
paste0(CintInit, " =?= ", value(cncDeserial, 'Cint'))

