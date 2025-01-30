#flor 

#test

library(tidyverse)
library(forcats)
library(readxl)
library(lubridate)

flor<-read_csv('flor101022.csv')%>%
  mutate(date1=ymd(date))

ggplot(flor, aes(date1, flora, group=transect)) + geom_point() + 
  geom_line() + facet_wrap(~field) + xlab("") + ylab("floral abundance")+theme_classic()


ggplot(flor, aes(date1, flora, group=transect)) + geom_point() + 
  geom_line() + facet_wrap(~group) + xlab("") + ylab("floral abundance")+theme_classic()

meanflor<-flor%>%
  group_by(group,  field)%>%
  mutate(meanflora=mean(flora))%>%
  ungroup()%>%
  mutate(name=fct_reorder(field, desc(meanflora)))



ggplot(meanflor, aes(name, meanflora)) + geom_point() + 
  geom_line()+ xlab("") + ylab("mean floral abundance (july-oct)")+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
