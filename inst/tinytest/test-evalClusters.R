
### Unit tests for evalClusters in ChemoSpec
# This example is wrapped in dontrun in the Rd for evalClusters
# as it takes a bit of time, so run only at home
	
data(metMUD2)

# Using NbClust
expect_silent(evalClusters(metMUD2, min.nc = 2, max.nc = 5, method = "average", index = "kl"))


