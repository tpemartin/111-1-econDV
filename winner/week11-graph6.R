# source: https://github.com/linttttt/111-1-econDV/blob/22940a91e1d0978bc4045c816949e7ec9e4d3d97/week9_COVID.R

COVIDNationalSum <- readRDS(file = "./winner/COVIDNationalSum.Rds")

pltCOVID=Plot()
pltCOVID$ggplot = ggplot(data = COVIDNationalSum)
pltCOVID$geoms = list(
  geom_tile(
    aes(
      x = yearPlusMonth,
      y = `區域_性別`,
      fill = numberColor
    ),
    width = 1,
    height = 1
  ),
  geom_vline(
    aes(xintercept = x),
    color = "white",
    size = 0.1
  ),
  geom_hline(
    aes(yintercept = y),
    color = "white",
    size = 0.1
  )
)

maxMale <- max(COVIDNationalSum$numberColor)
maxFemale <- min(COVIDNationalSum$numberColor)

pltCOVID$scale = scale_fill_gradient2(
  name = "男女人數",
  low = "#00bbcc",
  mid = "#d6dff5",#ebeffa
  high = "#6304C8",
  na.value = "grey",
  breaks = c(maxMale, maxMale, 0, maxFemale, maxFemale),
  labels = c(paste("最大值:", maxMale), "\n男(M)",
             "0", paste("最大值:", maxFemale*(-1)), "\n女(F)"
  )
)

pltCOVID$others = list(
  scale_y_discrete(
    expand = c(0, 0)
  ),
  scale_x_discrete(
    expand = c(0, 0),
    breaks = c(
      202001, 202007,
      202101, 202107,
      202201, 202207),
    labels = c(
      "2020/01", "2020/07",
      "2021/01", "2021/07",
      "2022/01", '2022/07'),
    name = "時間(年/月)"
  )
)

pltCOVID$theme = list(
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(angle = 0)),
  guides(
    fill = guide_colorbar(
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 10)
  )
)

pltCOVID$explain = list(
  labs(
    title = "台灣境內各地區COVID傳染病人數",
    subtitle = "註1:去除性別未定(U﹑X)\n註2:空白區域為無確診資料",
    caption = "資料來源：政府資料開放平臺\n https://data.gov.tw/dataset/118039"
  )
)

pltCOVID$make()+
  theme(
    plot.title = element_text(size = 20 #input$title
    ),
    plot.subtitle = element_text(size= 10 #input$subtitle
    ),
    plot.caption = element_text(size=10 #input$caption
    )
  )

pltCOVID$make()
rstudioapi::savePlotAsImage(
  "./winner/week11-graph6.png", width=1500, height=800, format = "png")

#econDV2::ggdash()
