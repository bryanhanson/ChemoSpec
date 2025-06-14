
### Unit tests for evalClusters in ChemoSpec
# This example is wrapped in dontrun in the Rd for evalClusters
# as it takes a bit of time, so run only at home
# This test requires packages that are listed in Depends, so
# we DONT run it when checking with noSuggests at CRAN

if (home) {
  chk <- Sys.getenv("_R_CHECK_DEPENDS_ONLY_", unset = "use_suggests")
  if (chk == "use_suggests") {
    data(metMUD2)
    expect_silent(evalClusters(metMUD2, min.nc = 2, max.nc = 5,
                method = "average", index = "kl"))
  }
}

