chkGrafOpt <- function(silent = TRUE){
  if(!silent)
  {
    if(getOption('ChemoSpecGraphics')=="base") 
      message("\nThe ChemoSpec graphics option is set to 'base'")
    
    if(getOption('ChemoSpecGraphics')=="ggplot2") 
      message("\nThe ChemoSpec graphics option is set to 'ggplot2'")
    
    if(getOption('ChemoSpecGraphics')=="plotly") 
      message("\nThe ChemoSpec graphics option is set to 'plotly'")
    
    if(getOption('ChemoSpecGraphics')=="shiny") 
      message("\nThe ChemoSpec graphics option is set to 'shiny'")
  }
  return(getOption("ChemoSpecGraphics"))
}