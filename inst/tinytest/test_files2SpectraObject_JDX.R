
### Unit tests for files2SpectraObject JDX option in ChemoSpec
# This example is wrapped in dontrun in the Rd for files2SpectraObject
# as it takes a bit of time, so run only at home

# If running this test interactively:
#   From R, run Sys.setenv("ESTOY_EN_CASA" = "TRUE") to permit additional tests to run
#   From R, run Sys.setenv("ESTOY_EN_CASA" = "") to run exactly as at CRAN

# If running this test via a shell script like a makefile, set the variable in the shell first:
#   setenv ESTOY_EN_CASA TRUE # csh
#   unsetenv ESTOY_EN_CASA # csh
#   export ESTOY_EN_CASA=TRUE # bash
#   export -n ESTOY_EN_CASA # bash
#   or set in the makefile (my current use)

if (identical(Sys.getenv("ESTOY_EN_CASA"), "TRUE")) {

  expect_silent( {
    options(ChemoSpecGraphics = "base")
    ed <- system.file("extdata", package = "ChemoSpec")
    tf <- "PCRF.jdx"
    chk <- file.copy(from = file.path(ed, tf), to = file.path(getwd(), tf),
      overwrite = TRUE)
    spec <- files2SpectraObject(gr.crit = "PCRF", freq.unit = "ppm", int.unit = "intensity",
      descrip = "test import", fileExt = "\\.jdx")
    sumSpectra(spec)
    plotSpectra(spec, lab.pos = 3.5, main = "Reduced Fat Potato Chip")
  })

}

