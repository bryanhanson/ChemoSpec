if (require(tinytest, quietly = TRUE)) {
  home <- Sys.info()["nodename"] == "BryanHs-MacBook-Pro.local"
  tinytest::test_package("ChemoSpec", verbose = TRUE, at_home = home)
}

