correctClass = function(marriageTaoYuan){
  
  marriageTaoYuan |>
    mutate(
      marry_pair = as.integer(marry_pair)
    ) -> marriageTaoYuan
  
  marriageTaoYuan$female_age_or_spouse1 |>
    factor(
      levels=c("未滿15歲", "15歲", "16歲", 
        "17歲", "18歲", "19歲", "20歲", "21歲", "22歲", "23歲", 
        "24歲", "25歲", "26歲", "27歲", "28歲", "29歲", "30歲", 
        "31歲", "32歲", "33歲", "34歲", "35～39歲", "40～44歲", 
        "45～54歲","55～64歲", "65歲以上")
    ) -> marriageTaoYuan$female_age_or_spouse1
  
  return(marriageTaoYuan) # this is added as the last line to represents the desired output
}
# together with
getMarriageTauYuan = function(marriage){
  marriage |>
    dplyr::filter(
      # 只看桃園市
      stringr::str_detect(site_id,"桃園市")
    ) -> marriageTaoYuan
  marriageTaoYuan |>
    # 創造行政區欄位
    mutate(
      site_id2 = stringr::str_sub(site_id,4,6)
    ) 
}
prepareProportionFemaleLateMarriage = function(marriageTaoYuan){
  # 計算每個（group_by)行政區登記對數(mutate(total_marry_pair = ...))
  marriageTaoYuan |>
    group_by(site_id2) |>
    summarise(
      total_marry_pair = sum(marry_pair)
    ) -> marriageTaoYuanTotal
  
  # 計算每個（group_by)行政區女性晚婚(condition)登記對數(mutate(late_marry_pair = ...))
  marriageTaoYuan |>
    dplyr::filter(
      # 女性晚婚條件
      female_age_or_spouse1 %in% c("30歲", "31歲", "32歲", "33歲", "34歲", "35～39歲", 
        "40～44歲", "45～54歲", "55～64歲", "65歲以上")
    ) |>
    group_by(site_id2) |>
    summarise(
      late_marry_pair = sum(marry_pair)
    ) -> marriageTaoYuanLate
  
  # 每個行政區女性晚婚的登記比率
  marriageTaoYuanLate |> 
    left_join(marriageTaoYuanTotal,
      by=c("site_id2")) -> 
    marriageTaoYuanLate
  marriageTaoYuanLate |>
    mutate(
      prop_marry_pair_late = 
        late_marry_pair/total_marry_pair
    ) -> marriageTaoYuanLate
  marriageTaoYuanLate
}
