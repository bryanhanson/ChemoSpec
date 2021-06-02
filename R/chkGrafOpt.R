chkGrafOpt <- function(silent = TRUE) {
  go <- getOption("ChemoSpecGraphics")
  valid <- c("base", "ggplot2", "plotly", "shiny")

  flag <- 0
  if (!(go %in% valid)) {
    go <- "base"
    flag <- 1
    options(ChemoSpecGraphics = "base")
    message("An invalid option is found! \nThe ChemoSpec graphics option has been set to 'base' ")
  }

  if (!silent) {
    if (go == "base" && flag == 0) {
      message("\nThe ChemoSpec graphics option is set to 'base'")
    }

    if (go == "ggplot2") {
      message("\nThe ChemoSpec graphics option is set to 'ggplot2'")
    }

    if (go == "plotly") {
      message("\nThe ChemoSpec graphics option is set to 'plotly'")
    }

    if (go == "shiny") {
      message("\nThe ChemoSpec graphics option is set to 'shiny'")
    }
  }
  return(go)
}
