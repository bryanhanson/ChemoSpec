
### Unit tests for files2SpectraObject JDX option in ChemoSpec
# This example is wrapped in dontrun in the Rd for files2SpectraObject
# as it takes a bit of time, so run only at home

home <- identical(Sys.info()["nodename"], "Abbott-2.local")
if (home) {
  expect_silent( {
    options(ChemoSpecGraphics = "base")
    ed <- system.file("extdata", package = "ChemoSpec")
    tf <- "PCRF.jdx"
    chk <- file.copy(from = file.path(ed, tf), to = file.path(getwd(), tf),
      overwrite = TRUE)
    spec <- files2SpectraObject(gr.crit = "PCRF", freq.unit = "ppm", int.unit = "intensity",
      descrip = "test import", fileExt = "\\.jdx")
    sumSpectra(spec)
    plotSpectra(spec, lab.pos = 3.5, main = "Reduced Fat Potato Chip")
  })
}
