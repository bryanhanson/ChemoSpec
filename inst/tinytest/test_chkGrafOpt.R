
### Unit tests for chkGrafOpt in ChemoSpec

options(ChemoSpecGraphics = 'base')
expect_equal(chkGrafOpt() ,"base")

options(ChemoSpecGraphics = 'ggplot2')
expect_equal(chkGrafOpt() ,"ggplot2")

