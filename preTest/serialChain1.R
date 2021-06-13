
# Tests serialization of generic objects in the presence of aliasing.

library(nCompiler)

set_nOption("serialize", TRUE)

# Chains:
#  Rnc1 -> nc1 -> nc2 -> nc3.  Also Rnc2 ->nc2 and/or Rnc3 -> nc3:
#    nSerialize(Rnc1 and/or Rnc2 and/or Rnc3)
#
# As above, but with permutations of which classes are identical/different.

# Cyclic cases:
#          Rnc1 -> nc2 -> {nc2, nc3} -> nc4:  nSerialize(Rnc1)
# {} denotes that all elments pointed to by arrow.

# Biderectional cases:

#  Rnc1 -> nc1 ->nc2 -> nc1 and Rnc2 -> nc2 (-> nc1 -> nc2)
#  Permutations of this.

#Creative extensions of these.
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

comp <- nCompile(Cnc, Rnc, interfaces = list(Cnc="generic", Rnc="generic"))

# Chain test 1:
#
#  Rnc1 -> nc3 -> nc2 -> nc1 :  nSerialize(Rnc1)
#
nc1 <- comp[[1]]()
nc2 <- nc1
nc3 <- nc2

Rnc1 <- comp[[2]]()
value(Rnc1, 'check') <- checkInit
value(Rnc1, 'cnc') <- nc3

value(nc1, 'Cint') <-  CintInit
value(nc1, 'Cnum') <- CnumInit

# Are the initializer values recovered?
paste0(CnumInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(Rnc1, 'cnc'), 'Cint'))

cncPreserial <- value(Rnc1, 'cnc') # Does the class initialization survive?
cncPreserial # Should not be NULL.

# Does the aggregate reference look right?
paste0(CnumInit, " =?= ", value(cncPreserial, 'Cnum'))
paste0(CintInit, " =?= ", value(cncPreserial, 'Cint'))

# Makes a serialization manager from whatever DLL-charged LOE is handy.
# Anything constructed using a generic interface should suffice.
serialization_mgr <- nCompiler:::getSerializationMgr(Rnc1)()

serial_index <- method(serialization_mgr, 'add_extptr')(nCompiler:::getExtptr(Rnc1))
serial_index

# Serializes the R container.
soeMgr <- serialize_nComp_object(serialization_mgr, serializer = nCompiler:::get_serialize_fun(Rnc1))

desoeMgr <- deserialize_nComp_object(soeMgr, nCompiler:::get_deserialize_fun(Rnc1) )
xptr <- method(desoeMgr, "get_extptr")(serial_index)
LOE <- new.loadedObjectEnv(xptr)

# Does the scalar initialization survive serdes?
paste0(checkInit, " =?= ", value(LOE, 'check'))

# Have the reference members survived?
paste0(CnumInit, " =?= ", value(value(LOE, 'cnc'), 'Cnum'))
paste0(CintInit, " =?= ", value(value(LOE, 'cnc'), 'Cint'))

# Does the class member surive as an aggregate?
cncDeserial <- value(LOE, 'cnc')
paste0(CnumInit, " =?= ", value(cncDeserial, 'Cnum'))
paste0(CintInit, " =?= ", value(cncDeserial, 'Cint'))

#########
##
