plot_company = function(){
  plt2 = Plot()
  plt2$ggplot = ggplot(data=eachTypeCompanyCapital_long)
  plt2$geom = 
    geom_col(
      mapping=aes(
        x=`縣 市 別`,
        y=CompanyTypeCapital, # 佔比的來源
        fill=CompanyType
      ),
      position="fill" # 
    )
  plt2$theme = theme(
    axis.text.x=element_text(angle = 45, hjust =1)
  )
  return(plt2)
}
