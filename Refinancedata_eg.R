library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Read in data 

 refinancedata <- read_excel("refinancedata.xlsx", 
                                   col_types = c("date", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                         "numeric"))
  

skimr::skim(refinancedata)

startdt <-'2019-10-01'
enddt<- '2020-02-29'

refinancedata$week<-lubridate::week(ymd(refinancedata$date))
refinancedata$month<-lubridate::month(ymd(refinancedata$date))

refin <-refinancedata %>% filter(CM > 0)%>%filter(date <= enddt & date >= startdt)%>%
arrange(Cost)%>%select(Cost,CM,week,month)

#Plot Cost against CM : all data ; daily

ggplot(refin, aes(x= Cost,y=CM)) + geom_line() + geom_point()  +geom_smooth(method="lm", formula = y~log(x))

refinAGG <-refinancedata %>%filter(date< enddt & date > startdt)%>%
arrange(Cost)%>%select(Cost,CM,Clicks,Leads,Revenue,week,month)%>%group_by(week)%>%summarize(sumcost=sum(Cost),sumcm=sum(CM),sumclick=sum(Clicks),
                                                                       
                                                                        sumRevenue=sum(Revenue),
                                                                        sumconv=sum(Leads),
                                                                        avgconv=mean(Leads),
                                                                        avgclicks=mean(Clicks),
                                                                        
                                                                        avgRev=mean(Revenue))%>% mutate(convrate=(sumconv/sumclick),revperconv=sumRevenue/sumconv)


#Plot Cost against CM : all data ; aggregated by week

ggplot(refinAGG, aes(x= sumcost,y=sumcm)) + geom_line() + geom_point() +geom_smooth(method="glm", formula = y~log(x))





