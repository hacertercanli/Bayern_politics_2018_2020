
#Bayern Politics 2018 - 2020 WC > the keywords handpicked from the corpus analysis

Keywords = c("usa","trump","polizei","kommunalwahl","spd","csu","europäische union","auto","landtagswahl","afd","merkel","cdu","finanzpolitik","migrationspolitik","nationalmannschaft","großbritannien","europawahl","türkei","russland","syrien","frankreich","china","brexit","demokratie","gesundheitspolitik","grüne","österreich","italien","iran","frauenrechte")


data = read.delim("Ordered_raw_HT.csv", sep = ";")
data = data[data$Date !="",]
data$Date = as.Date(data$Date, format = "%d/%m/%Y")
data$date = format(data$Date, "%m-%Y")
theset = data.frame("keyword"=rep(Keywords, time=31), date = rep(seq(from = as.Date('2018-01-01'), to = as.Date('2020-07-31'), by='months'), each = 30), size = 0)



for(i in 1:length(theset$keyword)) {
  train = data[data$date==format(as.Date(theset$date[i], format="%Y/%m/%d"),"%m-%Y"),]
  k = 0
  for (j in 1:length(train)){
    if (str_contains(train$Keywords[j],theset$keyword[i],ignore.case = TRUE)){
      k = k+1
    }
  }
  theset$size[i] = k
  
}


gg <- theset %>%
  ggplot(aes(label = keyword, size=size)) +
  geom_text_wordcloud() +
  theme_classic()
gg2 <- gg + transition_time(date) +
  ease_aes('linear')+
  labs(title = 'date: {frame_time}')

animate(gg2, duration = 30, fps = 1, width = 400, height = 400, renderer = gifski_renderer())
anim_save("gg_anim_wc.gif", animation = last_animation())