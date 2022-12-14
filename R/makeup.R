Plot = function(){
  gg=new.env()
  gg$ggplot = NULL
  gg$geom = list()
  gg$others = list()
  gg$make = function(){
    names(gg) |> setdiff(c("ggplot","geom", "make"))-> components
    gglist = as.list(gg)
    gglist[c("ggplot", "geom", components)] |> purrr::reduce(`+`)
  }
  return(gg)
}
Makeup = function(){
  makeup = new.env()
  makeup$geom_col = function(...){
    list(
      description = "Add transparency to filled colors and have 0 in y-axis touch down",
      geom = function(...) ggplot2::geom_col(..., alpha=0.5), 
      scale_y_continuous =function(...) ggplot2::scale_y_continuous(..., expand=c(0,0))
    )
  }
  makeup$theme = function(){
    theme_classic()
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_line(color="#c4d1d7")
    )
  }
  return(makeup)
}
