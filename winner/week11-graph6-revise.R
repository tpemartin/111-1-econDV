COVIDNationalSum <- readRDS(file = "./winner/COVIDNationalSum.Rds")
source("./winner/week11-graph6-revisionSupport.R")

# Benchmark -----
pltBenchmark = plotBenchmark(COVIDNationalSum)

pltBenchmark$make()

# 1. Balance the data in order to show NAs
dfBalanced = augmentData(COVIDNationalSum)
pltBenchmark2 = plotBenchmark(dfBalanced)
pltBenchmark2$make()

# 2. Facet regions
pltM2 = plotMorph2(dfBalanced)
pltM2$make()

# 3. Use common gender color
pltM3 = plotMorph3(dfBalanced)
pltM3$make()

# 4. Highlight winning gender
pltM4 = plotMorph4(dfBalanced)
pltM4$make()

rstudioapi::savePlotAsImage(
  file="week11-graph6-revised.png",
  format="png",
  width=1500, height=700
)
