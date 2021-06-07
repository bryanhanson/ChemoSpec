
### Unit tests for chkGraphicsOpt in ChemoSpec

# check for 'base'
options(ChemoSpecGraphics = "base")
expect_equal(chkGraphicsOpt(), "base")

# check for 'ggplot2'
options(ChemoSpecGraphics = "ggplot2")
expect_equal(chkGraphicsOpt(), "ggplot2")


# Check for empty values
options(ChemoSpecGraphics = "")
expect_equal(chkGraphicsOpt(), "base")

# check for invalid value
options(ChemoSpecGraphics = "xyz")
expect_equal(chkGraphicsOpt(), "base")