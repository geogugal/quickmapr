library(cranlogs)
library(dplyr)
library(lubridate)
library(ggplot2)
qm_downld <- cran_downloads("quickmapr",from = "2015-11-03", to = "last-day")
qm_monthly <- qm_downld %>%
  mutate(month = month(date),
         year = year(date))%>%
  group_by(year,month) %>%
  summarize(monthly_download = sum(count))
ggplot(qm_downld,aes(x=date,y=count))+
  geom_line()+geom_smooth()  
