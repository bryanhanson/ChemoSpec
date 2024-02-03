
### Unit tests for evalClusters in ChemoSpec
# This example is wrapped in dontrun in the Rd for evalClusters
# as it takes a bit of time, so run only at home

if (home) {
  data(metMUD2)
  expect_silent(evalClusters(metMUD2, min.nc = 2, max.nc = 5,
                method = "average", index = "kl"))
}

