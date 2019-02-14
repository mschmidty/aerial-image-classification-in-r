library(tidyverse)

data<-read_csv("extra_shape/acres_check_for_class.txt")

data%>%
  group_by(Class)%>%
  summarize(acres=sum(Acres))%>%
  ungroup()%>%
  mutate(percent=acres/sum(acres)*100)
