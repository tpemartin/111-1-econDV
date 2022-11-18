library(targets)
source("R/week_9.R")
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "econDV2"))

list(
  plotWithStackReverse %t=% getPlotWithStackReverse(), 
  plotWithStackNotReverse %t=% 
    turnPlotStackNotReverse(plotWithStackReverse), 
  plotWithCustomizedFill %t=% 
    customFillMapping(plotWithStackNotReverse), 
  plotWithStackReordered %t=%  
    reorderStackViaLimits(plotWithStackNotReverse), 
  plotWith4RemovedFromLimits %t=% 
    remove4FromLimits(plotWithCustomizedFill), 
  plotWith4RemovedAndFillTransparent %t=% 
    remove4andSetupNaValue(plotWithCustomizedFill),
  plotWithLegendYAxisFixed %t=%  
    setupFillLegendYAxis(plotWithStackReordered), 
  plotFinal %t=%  plotFinalMakeup(plotWithLegendYAxisFixed)
)
