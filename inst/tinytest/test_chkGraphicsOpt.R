
### Unit tests for chkGrafOpt in ChemoSpec

options(ChemoSpecGraphics = 'base')
expect_equal(chkGraphicsOpt() ,"base")

options(ChemoSpecGraphics = 'ggplot2')
expect_equal(chkGraphicsOpt() ,"ggplot2")

