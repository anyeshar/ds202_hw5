#setwd("~/ds202_hw5")

sales=readr::read_csv("Ames_Liquor_Sales.csv")
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(stringr)
#data cleaning


####part 2###

#extract lat/long
sales$`Store Location`=str_replace(sales$`Store Location`, "\\)","")
sales_clean = sales %>% separate(col=`Store Location`,into=c("point","coord"),sep="\\(") %>% separate(col=coord,into=c("Lat","Long"),sep=" ")
sales_clean = subset(sales_clean, select = -c(point))



#converting to correct type
str(sales_clean)
sales_clean$Lat=as.numeric(sales_clean$Lat)
sales_clean$Long=as.numeric(sales_clean$Long)
sales_clean$`Vendor Number` = as.numeric(sales_clean$`Vendor Number`)
sales_clean$`Category Name` = factor(sales_clean$`Category Name`)
sales_clean$`Store Name` = factor(sales_clean$`Store Name`)



#date
sales_clean = sales_clean %>% separate(Date, into = c('month', 'day', 'year'), sep = "/")
sales_clean$month=as.numeric(sales_clean$month)
sales_clean$day=as.numeric(sales_clean$day)
sales_clean$year=as.numeric(sales_clean$year)




###Part 3###

#PLot lat and long



sales_clean %>% ggplot(aes(y=Long,x=Lat))+geom_point()+labs(y="Latitude",x="Longitude",title="Scatterplot of Latitude and Longitude")
 

#need to fix
sales_clean %>% ggplot(aes(x=`Category Name`, count=`Volume Sold (Liters)`)) + 
  geom_bar(position=position_dodge(width = .5)) + ylab('Volume Sold') + 
  ggtitle("Voume of Alcohol Sold By Category") + theme(axis.text.y = element_text(size=5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#4

summary(sales_clean$`Bottles Sold`)
summary(sales_clean$`Volume Sold (Gallons)`)
summary(sales_clean$`Sale (Dollars)`)

#5

sales_clean %>% ggplot(aes(x=day, y=`Volume Sold (Liters)`)) + geom_point() + facet_wrap(~month) + geom_smooth() + labs(title=" Liquor Sold by Day by Month")


#6

sales_clean %>% ggplot(aes(x=day, y=`Volume Sold (Liters)`)) + geom_smooth() +
  facet_wrap(~month) + ggtitle("Volume of Alcohol Sold in Ames in 2019 by Month")

#Games: 8/31/19, 9/14/19, 9/21/19, 10/5/19, 10/26/19, 11/16/19, 11/23/19




























