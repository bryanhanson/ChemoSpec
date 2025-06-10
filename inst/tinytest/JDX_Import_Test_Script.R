### This file is used by test-files2SpectraObject_JDX.R

options(ChemoSpecGraphics = "base")
ed <- system.file("extdata", package = "ChemoSpec")
tf <- "PCRF.jdx"
chk <- file.copy(from = file.path(ed, tf),
                 to = file.path(getwd(), tf),
                 overwrite = TRUE)
tst <- files2SpectraObject(gr.crit = "PCRF",
                           freq.unit = "ppm",
                           int.unit = "intensity",
                           descrip = "test import",
                           fileExt = "\\.jdx")
sumSpectra(tst)
plotSpectra(tst, lab.pos = 3.5, main = "Reduced Fat Potato Chip")
