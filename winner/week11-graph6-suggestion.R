COVIDNationalSum <- readRDS(file = "./winner/COVIDNationalSum.Rds")
library(dplyr)
# Benchmark -----
{
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
    title = "台灣境內各地區COVID-19傳染病人數",
    subtitle = "註1:去除性別未定(U﹑X)\n註2:空白區域為無確診資料",
    caption = "資料來源：政府資料開放平臺\n https://data.gov.tw/dataset/118039"
  )
)}
pltCOVID$make()

# Morphosis 1: NA  ---------
{
  COVIDNationalSum$region = COVIDNationalSum$區域_性別 |> stringr::str_extract('[^_]+(?=_)') |>
    factor(levels=c("北部","中部","南部","東部","離島"))
  
  COVIDNationalSum$yearPlusMonth |> unique() -> yearPlusMonth
  COVIDNationalSum$region |> unique() -> region
  COVIDNationalSum$gender |> unique() -> gender
  genderRegion <- expand.grid(region, gender)
  genderRegion <- paste(genderRegion[[1]], genderRegion[[2]], sep = "_")
  dfFull <- expand.grid(yearPlusMonth, genderRegion)
  dfFull |>
    rename(
      "yearPlusMonth"="Var1", "區域_性別"="Var2"
    ) |> 
    mutate(
      region = 區域_性別 |> stringr::str_extract('[^_]+(?=_)') |>
        factor(levels=c("北部","中部","南部","東部","離島"))
    ) |>
    left_join(COVIDNationalSum, by=c("yearPlusMonth","區域_性別","region")) -> COVIDNationalSum
   
  pltCOVID$ggplot = ggplot(data=COVIDNationalSum)
  
  pltCOVID$explain = list(
    labs(
      title = "台灣境內各地區COVID-19傳染病人數",
      subtitle = "註1:去除性別未定(U﹑X)\n註2:灰色區域為無確診資料",
      caption = "資料來源：政府資料開放平臺\n https://data.gov.tw/dataset/118039"
    )
  )
}
pltCOVID$make()


# Morph 2: facet -----


{
  COVIDNationalSum$region = COVIDNationalSum$區域_性別 |> stringr::str_extract('[^_]+(?=_)') |>
    factor(levels=c("北部","中部","南部","東部","離島"))
  # COVIDNationalSum$yearPlusMonth |> as.character() |> as.numeric() -> COVIDNationalSum$yearPlusMonth

  pltCOVID$ggplot = ggplot(data = COVIDNationalSum)
  pltCOVID$others[[1]] = 
    scale_y_discrete(
      expand = c(0,0),
      labels = function(x){
        dplyr::if_else(
          stringr::str_detect(x,"M"),
          stringr::str_replace(x, "_M"," 男性"),
          "     女性"
        )
      }
    )
  pltCOVID$others[[2]] = scale_x_discrete(
    expand = c(0, 0),
    breaks = c(
      202001, 202007,
      202101, 202107,
      202201, 202207,
      202209 # add ending 
      ),
    labels = c(
      "2020/01", "/07",
      "2021/01", "/07",
      "2022/01", '/07',
      "2022/09" # add ending
      ),
    name = "時間(年/月)"
  )
  pltCOVID$theme2 <- theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank()
  )
  
  pltCOVID$facet = facet_grid(rows = vars(region), scales = "free_y")
}
pltCOVID$make()

# Morph 3: gender -----
{

  pltCOVID$scale <- scale_fill_gradient2(
    name = "男女人數",
    low = "#f7037b",
    mid = "white",
    high = "#0079fa",
    na.value = "grey",
    breaks = c(maxMale, maxMale, 0, maxFemale, maxFemale),
    labels = c(
      paste("最大值:", maxMale), "
      男(M)",
      "0", paste("最大值:", maxFemale * (-1)), "
      女(F)"
    )
  )
  
  pltCOVID$guid = guides(color="none")
}
pltCOVID$make()

# Morph 4: Highlight ------
{  
  COVIDNationalSum |>
    arrange(region, yearPlusMonth, desc(gender)) |>
    group_by(region, yearPlusMonth) |>
    mutate(
      maleShare = (function(x) {
        if (length(x) == 2) {
          return(abs(x[[1]] / x[[2]]))
        } else {
          return(NaN)
        }
      })(numberColor),
      maleShareHigher = maleShare > 1,
      cases = dplyr::case_when(
        gender == "M" & maleShare > 1 ~ "male win",
        gender == "F" & maleShare < 1 ~ "female win",
        TRUE ~ "others"
      )
    ) -> COVIDNationalSum
  
  pltCOVID$ggplot <- ggplot(data = COVIDNationalSum)
  pltCOVID$geoms[[4]] <- geom_tile(
    aes(
      x = yearPlusMonth,
      y = 區域_性別,
      color = cases
    ),
    fill="transparent",
    linewidth = 1, #input$linewidth
    width = 1, #input$width
    height = 1 #input$height
  )
  pltCOVID$scale2 = list(
    scale_color_manual(
      limits = c("male win", "female win", "others"),
      values = c(
        "#003c7b",
        "#79013d",
        "transparent"
      )),
      guides(color = "none")
    )
  pltCOVID$explain = list(
    labs(
      title = "台灣境內各地區COVID-19傳染病人數",
      subtitle = "有框表示該性別人數多於另一性別\n註1:去除性別未定(U﹑X)\n註2:灰色區域為無確診資料",
      caption = "資料來源：政府資料開放平臺\n https://data.gov.tw/dataset/118039"
    ),
    xlab(NULL),ylab(NULL))
  pltCOVID$theme3 = theme(
    panel.spacing.y = unit(
      20, #input$spacing
      "points"
    ))
}

pltCOVID$make()
rstudioapi::savePlotAsImage(
  "./winner/week11-graph6-revised.png", width=1500, height=800, format = "png")

