#'
#' Create a Spectra Object from a Matrix
#'
#' @describeIn files2SpectraObject Import a matrix of data
#'
matrix2SpectraObject <-
  function(gr.crit = NULL, gr.cols = c("auto"),
           freq.unit = "no frequency unit provided",
           int.unit = "no intensity unit provided",
           descrip = "no description provided",
           in.file = NULL, out.file = "mydata",
           chk = TRUE, ...) {
    # Function to Read & Prep Spectroscopic Data
    # from a raw data file into an Spectra object
    # This function acts on a matrix-like file
    # containing frequencies in the first column
    # and samples in additional columns.

    # Part of the ChemoSpec package
    # Bryan Hanson, DePauw University, April 2016

    if (.chkReqPkgs("R.utils")) {
      if (is.null(gr.crit)) stop("No group criteria provided to encode data")
      if (is.null(in.file)) stop("You need to specify an input file")

      out <- tryCatch(
        {
          # Get the data matrix/matrices
          no.files <- length(in.file)

          message("\nReading ", no.files, " file(s)...\n")

          specs <- read.table(in.file[1], header = TRUE, ...)
          sampleNames <- colnames(specs)[-1]
          freq <- specs[, 1]
          no.freq <- length(freq)
          specs <- as.matrix(t(specs[, -1]))
          dimnames(specs) <- NULL

          # since we don't know how many samples are in each file ahead of time, we have to
          # grow the memory in an inefficient manner...

          if (length(in.file) > 1) {
            # Code for progress bar contributed by Reinhard Kerschner
            env <- environment() # NEW set environment for progress bar
            counter <- 1
            pb <- txtProgressBar(min = 0, max = no.files, style = 3)

            for (i in 2:no.files) {
              curVal <- get("counter", envir = env)
              assign("counter", curVal + 1, envir = env)
              setTxtProgressBar(get("pb", envir = env), curVal + 1)

              tmp <- read.table(in.file[i], header = TRUE, ...)
              if (isFALSE(all.equal(no.freq, nrow(tmp)))) {
                probFile <- in.file[i]
                msg <- paste("Encountered a file (", probFile, ") with the wrong number of frequencies")
                stop(msg)
              }
              sampleNames <- c(sampleNames, colnames(tmp)[-1])
              tmp <- as.matrix(t(tmp[, -1])) # seems to remove name attributes
              specs <- rbind(specs, tmp)
            }

            close(pb)
          }

          message("\nAssigning ", length(sampleNames), " spectra to ", length(gr.crit), " group(s)...\n")

          # Set up the Spectra object
          spectra <- list()
          spectra$freq <- as.numeric(freq)
          spectra$data <- specs
          dimnames(spectra$data) <- NULL
          spectra$names <- sampleNames
          class(spectra) <- "Spectra"

          # Go get group assignments & colors, to complete assembly of spectra

          spectra <- .groupNcolor(spectra, gr.crit, gr.cols, mode = "1D")
          spectra$unit[1] <- freq.unit
          spectra$unit[2] <- int.unit
          spectra$desc <- descrip

          if (chk) chkSpectra(spectra)

          datafile <- paste(out.file, ".RData", sep = "")
          R.utils::saveObject(spectra, file = datafile)
          return(spectra)
        },
        error = function(cond) {
          errmess <- "There was a problem processing your matrix!\n\nDid you get a message such as 'subscript out of bounds'? You probably need to specify sep and possibly dec values. Please read ?files2Spectra2Object for details.\n\nIf that doesn't fix things, set chk = FALSE and inspect the resulting object.\n"
          message("\nError message from R: ", cond$message, "\n")
          message(errmess)
          return(NA)
        }
      ) # end of tryCatch

      return(out)
    }
  }
