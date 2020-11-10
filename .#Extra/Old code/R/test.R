# test

library(SWBM)
DataClim <- read.csv("./data/DataClim.csv", sep = ";", dec = ".", header = T, stringsAsFactors = F)

test <- SWB_model(DataClim, model = "c_code")
display_SWM(test)
# Not working because model generated interceptions > precipitations... Like, a lot of them.
test$daily_aggregated_outputs -> test2
test2 %>%
  mutate(year = lubridate::year(date)) %>%
  ggplot(aes(x = date, y = REW_global))+
  geom_line()+
  facet_wrap(~year, scales = "free_x")
