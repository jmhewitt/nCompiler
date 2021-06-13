# Tests serialization of aliased objects.

context("Test serialization under aliasing")

# Saves state.
old_serialize_option <- get_nOption("serialize")
set_nOption("serialize", TRUE)

test_that("Basic serialization works",
{
  nc1 <- nClass(
    classname = "nc1",
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cx = 'integerScalar',
      Cfoo = nFunction(
        fun = function(x) {
          return(x+1)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar'),
      Cbar = nFunction(
        fun = function(x, y) {
          return(x + y)
        },
        argTypes = list(x = 'numericMatrix',
                        y = 'numericMatrix'),
        returnType = 'numericMatrix')
    }
  )
  # Various tests here:
  
# nc1, nc2, etc., are C++ objects.
# '->' dentoes contains / points to, for nc objects.
# Rnc1 is the R object containing nc1:  Rnc1 -> nc1.

# Chains:
#  Rnc1 -> nc1 -> nc2 -> nc3 :  nSerialize(Rnc1)
# As above, but with nc2 and/or nc3 with same or different class as nc1

# Rnc1 -> nc1 -> nc2 -> nc3.  Also Rnc2 ->nc2 and/or Rnc3 -> nc3:
#  nSerialize(Rnc1 and/or Rnc2 and/or Rnc3)

# As above, but with permutations of which classes are identical/different.

# Dimaond cases:
#          Rnc1 -> nc2 -> {nc2, nc3} -> nc4:  nSerialize(Rnc1)
# {} denotes that all elments pointed to by arrow.

#Biderectional cases:

#  Rnc1 -> nc1 ->nc2 -> nc1 and Rnc2 -> nc2 (-> nc1 -> nc2)
#  Permutations of this.

#Creative extensions of these.
}
)

# Restores state.
set_nOption("serialize", old_serialize_option)

