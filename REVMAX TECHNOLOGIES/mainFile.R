#Important libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotrix)
library(plotly)

#Reading the csv file
sales_data<-read.csv('C:\\Users\\SOMESH\\Desktop\\REVMAX\\SalesData.csv')

#Describing the dataset 
summary(sales_data)
str(sales_data)

#Checking for Missing Values
sum(is.na(sales_data)) #<-0

#First 6 rows of dataset
head(sales_data)

#Q1] Compare Sales by region for 2016 with 2015 using bar chart.
que1 <- sales_data %>% group_by(Region) %>% summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
que1 <- melt(que1,id.vars= "Region",variable.name="Year",value.name="sales")
plot1 <- ggplot(que1)+aes(x=Region,y=sales,fill=Year)+geom_bar(stat = "identity",position = "dodge")
plot1


#Q2] Pie charts for sales for each region in 2016.

que2 <- sales_data %>% group_by(Region) %>% dplyr::summarise(Total_Sales2016=sum(Sales2016))
pct <- round(que2$Total_Sales2016/sum(que2$Total_Sales2016)*100)
labels <- paste(pct,"%",":",c("Central","East","West"))

par(mfrow = c(1,1))
plot2.1<-pie(que2$Total_Sales2016,labels=labels,main="Pie chart of Sales in 2016")
plot2.2<-pie3D(que2$Total_Sales2016,labels=labels,explode=0.1,main="Pie chart of Sales in 2016")


#Q3 Compare sales of 2015 and 2016 with Region and Tiers

que3 <- sales_data %>% group_by(Region,Tier) %>% summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
que3 <- melt(que3,id.vars= c("Region","Tier"),variable.name="Year",value.name="sales")
plot3 <- ggplot(que3)+
  aes(x=Tier,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")+
  facet_grid(.~Region)
plot3

#Q4	In East region, which state registered a decline in 2016 as compared to 2015?

que4 <- sales_data %>% group_by(Region,State) %>% summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
que4 <- que4[que4$Region=="East",]
que4 <- melt(que4,id.vars= c("Region","State"),variable.name="Year",value.name="sales")
plot4 <- ggplot(que4)+
  aes(x=State,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")

plot4

#Q5 In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015 ?

que5 <- sales_data %>% group_by(Tier,Division) %>% summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
que5 <- que5[que5$Tier=="High",]
que5 <- melt(que5,id.vars= c("Tier","Division"),variable.name="Year",value.name="sales")
plot5 <- ggplot(que5)+
  aes(x=Division,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")
plot5 + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Q6 Create a new column Qtr -
#.	Jan - Mar : Q1
#.	Apr - Jun : Q2
#.	Jul - Sep : Q3
#.	Oct - Dec : Q4

sales_data$Qtr <- ifelse(sales_data$Month%in% c("Jan","Feb","Mar"),"Q1",
                    ifelse(sales_data$Month%in% c("Apr","May","Jun"),"Q2",
                           ifelse(sales_data$Month%in% c("Jul","Aug","Sep"),"Q3","Q4")))

#Q7 Compare Qtr wise sales in 2015 and 2016 in a bar plot

que7 <- sales_data %>% group_by(Qtr) %>%summarise(TotalSales2015=sum(Sales2015),TotalSales2016=sum(Sales2016))
que7 <- melt(que7,id.vars= "Qtr",variable.name="Year",value.name="sales")
plot7 <- ggplot(que7)+
  aes(x=Qtr,y=sales,fill=Year)+
  geom_bar(stat = "identity",position = "dodge")

plot7

#Q8 Determine the composition of Qtr wise sales in and 2016 with regards to all the Tiers in a pie chart.
# (Draw 4 pie charts representing a Quarter for each Tier)

que8 <- sales_data %>% group_by(Qtr,Tier) %>% summarise(TotalSales2016=sum(Sales2016))
que8<- melt(que8,id.vars= c("Qtr","Tier"),variable.name="Year",value.name="sales")

ggplot(que8, aes(x = factor(1), y = sales, fill = factor(Tier))) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  facet_wrap(~Qtr)





















