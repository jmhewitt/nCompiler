
# Tests serialization of generic objects in the presence of aliasing.

library(nCompiler)

set_nOption("serialize", TRUE)

#
# Chain test 3.
#
# Rnc1 -> nc1 -> nc2 -> nc3.  Also Rnc2 ->nc2 and/or Rnc3 -> nc3:
#                                        #
#  serialize:  Rnc1 and/or Rnc2 and/or Rnc3
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

Rnc <- nClass(
  classname = "Rnc",
  Rpublic = list(),
  Cpublic = list(
    cnc = 'Cnc',
    check = 'integerScalar'
  )
)
checkInit = 7154

comp <- nCompile(Cnc, Rnc, interfaces = list(Cnc = "generic", Rnc = "generic"))

nc3 <- comp[[1]]()

Rnc1 <- comp[[2]]()
Rnc2 <- comp[[2]]()
Rnc3 <- comp[[2]]()

nc2 <- nc3
nc1 <- nc2

value(Rnc1, 'check') <- checkInit
value(Rnc2, 'check') <- checkInit
value(Rnc3, 'check') <- checkInit

value(Rnc3, 'cnc') <- nc3
value(Rnc2, 'cnc') <- nc2
value(Rnc1, 'cnc') <- nc1

value(nc3, 'Cint') <- CintInit
value(nc3, 'Cnum') <- CnumInit

# Are the initialized values accessible?
paste0(CnumInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cint'))

paste0(CnumInit, " =?= ", value(value(Rnc2, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc2, 'cnc'), 'Cint'))

paste0(CnumInit, " =?= ", value(value(Rnc3, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc3, 'cnc'), 'Cint'))

# Is the class initialization accsssible as an aggregate?    
cncPreserial1 <- value(Rnc1, 'cnc')

# Does the aggregate reference preserve initialands?
paste0(CnumInit, " =?= ", value(cncPreserial1, 'Cnum'))
paste0(CintInit, " =?= ", value(cncPreserial1, 'Cint'))

cncPreserial2 <- value(Rnc2, 'cnc')
paste0(CnumInit, " =?= ", value(cncPreserial2, 'Cnum'))
paste0(CintInit, " =?= ", value(cncPreserial2, 'Cint'))

cncPreserial3 <- value(Rnc3, 'cnc')
paste0(CnumInit, " =?= ", value(cncPreserial3, 'Cnum'))
paste0(CintInit, " =?= ", value(cncPreserial3, 'Cint'))


mgr <- nCompiler:::getSerializationMgr(Rnc1)()
serial_index <- method(mgr, 'add_extptr')(nCompiler:::getExtptr(Rnc1))
soeMgr <- serialize_nComp_object(mgr, serializer = nCompiler:::get_serialize_fun(Rnc1))
desoeMgr <- deserialize_nComp_object(soeMgr, nCompiler:::get_deserialize_fun(Rnc2) )
xptr <- method(desoeMgr, "get_extptr")(serial_index)
LOE1 <- new.loadedObjectEnv(xptr)


mgr <- nCompiler:::getSerializationMgr(Rnc2)()
serial_index <- method(mgr, 'add_extptr')(nCompiler:::getExtptr(Rnc2))
soeMgr <- serialize_nComp_object(mgr, serializer = nCompiler:::get_serialize_fun(Rnc3))
desoeMgr <- deserialize_nComp_object(soeMgr, nCompiler:::get_deserialize_fun(Rnc2) )
xptr <- method(desoeMgr, "get_extptr")(serial_index)
LOE2 <- new.loadedObjectEnv(xptr)

mgr <- nCompiler:::getSerializationMgr(Rnc3)()
serial_index <- method(mgr, 'add_extptr')(nCompiler:::getExtptr(Rnc3))
soeMgr <- serialize_nComp_object(mgr, serializer = nCompiler:::get_serialize_fun(Rnc3))
desoeMgr <- deserialize_nComp_object(soeMgr, nCompiler:::get_deserialize_fun(Rnc3) )
xptr <- method(desoeMgr, "get_extptr")(serial_index)
LOE3 <- new.loadedObjectEnv(xptr)


# Does the scalar initialization survive serdes?
paste0(checkInit, " =?= ", value(LOE1, 'check'))
paste0(checkInit, " =?= ", value(LOE2, 'check'))
paste0(checkInit, " =?= ", value(LOE3, 'check'))

# Have the members survived as values?
paste0(CnumInit, " =?= ", value(value(LOE1, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE1, 'cnc'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(LOE2, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE2, 'cnc'), 'Cint'))
paste0(CnumInit, " =?= ", value(value(LOE3, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE3, 'cnc'), 'Cint'))

# Do members surive as aggregates?
cncDeserial1 <- value(LOE1, 'cnc')
paste0(CnumInit, " =?= ", value(cncDeserial1, 'Cnum'))
paste0(CintInit, " =?= ", value(cncDeserial1, 'Cint'))

cncDeserial2 <- value(LOE2, 'cnc')
paste0(CnumInit, " =?= ", value(cncDeserial2, 'Cnum'))
paste0(CintInit, " =?= ", value(cncDeserial2, 'Cint'))

cncDeserial3 <- value(LOE3, 'cnc')
paste0(CnumInit, " =?= ", value(cncDeserial3, 'Cnum'))
paste0(CintInit, " =?= ", value(cncDeserial3, 'Cint'))
