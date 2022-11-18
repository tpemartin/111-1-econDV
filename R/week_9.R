
getPlotWithStackReverse = function(){
  plt = Plot()
  plt$ggplot = ggplot(mpg, aes(y = class))
  plt$geom =  geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) 
  plt
}
turnPlotStackNotReverse = function(plt){
  plt$geom =geom_bar(aes(fill = drv), position = position_stack(reverse=FALSE))
  plt
}

#  """## Map limits to values"""
customFillMapping = function(plt){
  plt$scale = scale_fill_manual(
     limits = c("f", "4", "r"),
     values = c("#ED90A4", "#7EBA68", "#6FB1E7")
  )
  plt
}
#  """## limits also determine order"""
reorderStackViaLimits = function(plt){
  plt$scale = scale_fill_manual(
   limits = c("r", "f", "4"), # 最下，... ，最上
   values = c("#ED90A4", "#7EBA68", "#6FB1E7")
)
  plt
}

#  """## Outside limits is considered NA"""
remove4FromLimits = function(plt){
  plt$scale = scale_fill_manual(
     limits = c("r", "f"), 
     values = c("#ED90A4", "#7EBA68")
  )
  plt
}

remove4andSetupNaValue = function(plt){
  plt$scale = scale_fill_manual(
    limits = c("r", "f"), 
    values = c("#ED90A4", "#7EBA68"),
    na.value = "transparent" # decides na.values
  )
  plt
}



#  """##  Lengend: name, breaks, labels
#  
#  #  """
setupFillLegendYAxis = function(plt){
  plt$scale = list(
    scale_fill_manual(
      limits=c("r", "f", "4"),values=c("#ED90A4", "#7EBA68",  "#6FB1E7"),
      labels=c("休旅車","四輪驅動", "???"), name="車種"),
    scale_y_discrete(
      limits=c("subcompact", "suv"), 
      labels=c("中型", "SUV"), name="車型")
  )
  
  
  plt
}




#  """# Final makeup
#  
#  
#  #  """
plotFinalMakeup = function(plt){
  plt$others = list(
    scale_x_continuous(sec.axis = dup_axis(),
                       expand=c(0,0)
    )
  )
  
  mk = econDV2::Makeup()
  plt$theme = mk$theme()
  
  plt$theme = list(
    mk$theme(),
    theme(
      panel.grid.major.y= element_blank(),
      panel.grid.major.x=element_line(color="grey"),
      legend.position = "top",
      axis.title.y=element_text(angle = 0)
    )
  )
  
  plt$explain = list(
    labs(
      title="不同車型/車種數量",
      subtitle="去除部份車型/車種",
      caption="資料來源：",
      tag="圖1"
    ),
    xlab(NULL)
  )
  plt
}


# 
# plt$make()+
#   theme(
#   plot.title=element_text(size = 10 #input$title
#    ),
#   plot.subtitle = element_text(size= 10 #input$subtitle
#    ),
#   plot.caption = element_text(size=10 #input$caption
#    )
#   )
# 
# econDV2::ggdash()
