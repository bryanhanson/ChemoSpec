
### Unit tests for evalClusters in ChemoSpec
# This example is wrapped in dontrun in the Rd for evalClusters
# as it takes a bit of time, so run only at home

# If running this test interactively:
#   From R, run Sys.setenv("ESTOY_EN_CASA" = "TRUE") to permit additional tests to run
#   From R, run Sys.setenv("ESTOY_EN_CASA" = "") to run exactly as at CRAN

# If running this test via a shell script like a makefile, set the variable in the shell first:
#   setenv ESTOY_EN_CASA TRUE # csh
#   unsetenv ESTOY_EN_CASA # csh
#   export ESTOY_EN_CASA=TRUE # bash
#   export -n ESTOY_EN_CASA # bash
#   or set in the makefile

if (identical(Sys.getenv("ESTOY_EN_CASA"), "TRUE")) {
	
  data(metMUD2)

  # Using NbClust
  expect_silent(evalClusters(metMUD2, min.nc = 2, max.nc = 5, method = "average", index = "kl"))
}

