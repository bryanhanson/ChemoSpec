if (require(tinytest, quietly = TRUE)) {
  home <- identical(Sys.info()["nodename"], "Abbott-2.local")
  tinytest::test_package("ChemoSpec", verbose = TRUE, at_home = home)
}

