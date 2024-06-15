graphics.off();remove(list=ls());cat("\14")
getwd()


library(openxlsx) 
library(data.table)
library(dplyr)

sales<- read.xlsx("C:/Users/thinl/Documents/Classification/Red30 Tech Sales.xlsx", 1, detectDates=TRUE)

summary(sales)

summary(sales[,c('Quantity', 'Price', 'Discount', 'Order.Total')])

table(sales$Payment.Status)

prop.table(table(sales$Payment.Status))

#get payment plan table(sales$Payment.Plan)

prop.table(table(sales$Payment.Plan))

#null sum(is.null(sales))

#sort the data by total sales[order(sales\$Order.Total, decreasing =TRUE),]

#top qty

sales[order(sales$Quantity, decreasing =TRUE),]

#get top N values by customer

data_mod<- sales[order(sales$Order.Total, decreasing =TRUE),]
data_mod<- data.table(data_mod, key="CustState") 
data_mod<- data_mod[ , head(.SD, 1), by= CustState]

#top customer by total number or Orders

sales %>% count(CustName,Sort=TRUE)

#get customer type by total number of orders

table(sales$CustomerType)

prop.table(table(sales$CustomerType))

#average sales and quantity by customer sales%\>%group_by(CustomerType)%\>% summarise(mean_sales =mean(Order.Total), mean_quantity =mean(Quantity))

#average sales and quantity by customer sales%\>%group_by(CustomerType)%\>% summarise(total_sales =sum(Order.Total), total_quantity =sum(Quantity))

#top customer states by total number of orders sales%\>% count(CustState, sort=TRUE)

#REVIEW productcategories sold by customer type table(sales$CustomerType,sales$ProdCategory )

table(sales$CustState,sales$ProdCategory )

#employee analysis

sales[order(sales$Order.Total, decreasing =TRUE),]

#orderdata by order qty

sales[order(sales$Quantity, decreasing=TRUE),]

#top employeesby total number of orders

sales %>% count(Employee.Name, sort=TRUE )

#top employees JOB POSITION by total number of orders sales %\>% count(Employee.Job.Title, sort=TRUE )

#review employee job titles prop.table(table(sales\$Employee.Job.Title))

#review employee job titles prop.table(table(sales\$Sales.Region))

#avg sales and qty sold

sales %>% group_by(Employee.Job.Title) %>% summarise(mean_sales =mean(Order.Total), mean_qty= mean(Quantity))

#total sales and total qty sold

sales %>% group_by(Employee.Job.Title) %>% summarise(total_sales =sum(Order.Total), total_qty= sum(Quantity))

#product catgoy by each employee table(sales$Employee.Job.Title, sales$ProdCategory)

#product catgoy by each employee table(sales$Employee.Job.Title, sales$Sales.Region)

#sort data by order sales[order(sales\$Order.Total, decreasing=TRUE),]

#sort data by quantity sales[order(sales\$Quantity, decreasing=TRUE),]

#longvity of customer sales%\>% count(DateCustAdded, sort=TRUE) table(sales$CustState, sales$DateCustAdded)

sales[order(sales$DateCustAdded, decreasing=TRUE),]

sales[order(sales$DateCustAdded, decreasing=FALSE),]


#classification analysis


#decision tree#
#get order month


sales$OrderMonth <- format(sales$OrderDate, "%B")
sales$OrderYear <- format(sales$OrderDate, "%Y")


#FACTOR PAYMENT STATUS
sales$Payment.Status <-factor(sales$Payment.Status)


#we use subset of sales data- check  feature eng
sales_subset<- subset(sales, select=c(Employee.Job.Title, Sales.Region, OrderMonth,OrderYear, OrderType, CustomerType, ProdCategory, Quantity, Price, Discount, Order.Total, Payment.Plan, Payment.Status))

#split the data#75% training and 25% testing
set.seed(42)
bound <- floor((nrow(sales_subset) / 4) * 3)

df <- sales_subset[sample(nrow(sales_subset)), ]
train <- df[1:bound, ]
test <- df[(bound + 1):nrow(df), ]

summary(train)
summary(test)

#install package
install.packages("tidymodels")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("parsnip")
install.packages("recipes")

library(tidymodels)
library(rpart)
library(rpart.plot)
library(parsnip)
library(recipes)
library(workflows)

tree <- decision_tree() %>% 
  set_engine("rpart") %>%
  set_mode("classification")

#create receipe

df_receipe<- recipe(Payment.Status~ ., data=df) %>% 
  step_normalize(all_numeric())

#create decsion tree flow
tree_wf <- workflow()%>% add_recipe(df_receipe)%>% add_model(tree)%>% fit(train)

summary(tree_wf)

#predict
predResult<- data.frame(predict(tree_wf, test))
colnames(predResult) <-c("test_pred_tress")
test<-cbind(test, predResult)

#plot decsion tree
fit<- rpart(Payment.Status ~., data=train, method="class")
rpart.plot(fit, tweak =1.5)







