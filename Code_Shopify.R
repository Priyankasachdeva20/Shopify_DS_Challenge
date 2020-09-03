#setwd("P:/Courses/CAC/Internships/Shopify")

library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
options(scipen=999)
#Read in data

data1 <- read.csv("2019 Winter Data Science Intern Challenge Data Set.csv")
str(data1)
#convert date into correct format
data1$created_at <- as_datetime(data1$created_at)

#Exploratory analysis

#================================================================================================================
#--------------------Check order amount distribution by user_id

fig1 <- plotly::plot_ly(data1,  y = ~order_amount, x= ~user_id, type="scatter")  %>% 
        layout(title = "Order amounts by user ids",
         xaxis = list(title = "User_id"),
         yaxis = list (title = "Order_amount"))

fig1

#Confirm
data1 %>% filter(order_amount>600000)

#--------RESULT:
#user id 607 same amount spend at same time by credit card, hence likely fraud
#Hence, remove user_id=607 from data before further calculation

data1 <- data1 %>% filter(user_id!=607)


#--------------------Check order amount distribution by shop_id

fig2 <- plotly::plot_ly(data1,  y = ~order_amount, x= ~shop_id, type="scatter", 
                        marker = list(size = 10,
                                      color = 'rgba(255, 182, 193, .9)',
                                      line = list(color = 'rgba(152, 0, 0, .8)',
                                                  width = 2)))  %>% 
        layout(title = "Order amounts by shop ids",
         xaxis = list(title = "Shop_id"),
         yaxis = list (title = "Order_amount"))

fig2

#Confirm
data1 %>% filter(order_amount>20000)

#--------RESULT:
#user id 78 has abruptly high order amounts, could be wrong data entry in either amount or order items entered
#Hence, remove user_id=78 from data before further calculation

data1 <- data1 %>% filter(shop_id!=78)

#================================================================================================================

#Calculate bill amount per item as many users have purchased more than 1 items
data1$order_amt_per_item <- data1$order_amount/data1$total_items

#Find out mean order amount at every shop_id
t <- data1 %>% group_by(shop_id) %>% summarize(mean=mean(order_amt_per_item))
t
#visualize and check for outlier 
fig3 <- plotly::plot_ly(t,  y = ~mean, x= ~shop_id,  
                        marker = list(size = 10,
                                      color = 'rgba(25, 182, 193, .9)',
                                      line = list(color = 'rgba(152, 0, 0, .8)',
                                                  width = 2)))  %>% 
  layout(title = "Mean order amounts per item by shop ids",
         xaxis = list(title = "Shop_id"),
         yaxis = list (title = "Mean Order_amount_per_item"))

fig3


#RESULT: shop_id = 42 has higher average of order amount,
#could be selling more expensive collection

#Find out mean order amount at every shop_id for every user_id
t1 <- data1 %>% group_by(shop_id,user_id) %>% summarize(mean=mean(order_amt_per_item))
t1

#visualize and check for outlier 
fig4 <- plotly::plot_ly(t1,  y = ~mean, x= ~user_id,  
                        marker = list(size = 10,
                                      color = 'rgba(25, 222, 153, .9)',
                                      line = list(color = 'rgba(12, 110, 0, .8)',
                                                  width = 2)))  %>% 
  layout(title = "Mean order amounts per item by user ids",
         xaxis = list(title = "User_id"),
         yaxis = list (title = "Mean Order_amount_per_item"))

fig4

#There are users with average spending of $352 i.e. it could be at shop 42, many users assure legit transcations
#Hence, we can assume shop id 42 sells an expensive range of shoes
#==========================================================================================================
x <- data1 %>% summarize(mean=mean(order_amount), stddev=sd(order_amount))
y <- data1 %>% summarize(mean=mean(order_amt_per_item), stddev=sd(order_amt_per_item))

x
y

fig5 <- plotly::plot_ly(data1,   x= ~order_amt_per_item,  
                        marker = list(size = 10,
                                      color = 'rgba(25, 22, 213, .9)',
                                      line = list(color = 'rgba(12, 76, 160, .7)',
                                                  width = 2)))  %>% 
  layout(title = "Order amounts per item ",
         xaxis = list(title = "Order_amount_per_item"),
         yaxis = list (title = "Frequency"))

fig5

#Y is a better metric to consider based off of the mean and std dev values which affirm the normal distribution
#==========================================================================================================