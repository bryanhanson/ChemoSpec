### Unit tests for files2SpectraObject JDX option in ChemoSpec
# This example is wrapped in dontrun in the Rd for files2SpectraObject
# as it takes a bit of time, so run only at home
# This test requires packages that are listed in Depends, so
# we DONT run it when checking with noSuggests at CRAN

if (home) {
  chk <- Sys.getenv("_R_CHECK_DEPENDS_ONLY_", unset = "use_suggests")
  if (chk == "use_suggests") {
    expect_silent(source("JDX_Import_Test_Script.R"))
  }
}