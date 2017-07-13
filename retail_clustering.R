##Online Retail Dataset - K Means Clustering based on Amount Spent grouped by Customer ID
#install.packages("xlsx", dependencies = TRUE)
#install.packages("sqldf")
#install.packages("dplyr")
#install.packages("data.table")
library(dplyr)
library(readxl)
library(sqldf)
library(xlsx)
library(data.table)
library(ggplot2)
getwd()

#importing data from excel

retail_data <- read_excel("Online Retail.xlsx")
View(retail_data)
sum(is.na(retail_data))

#na = "NA"
#EDA-------------
# 1. Dropping the Cancelled Invoices
# 2. Creating a new column  "BILLAMOUNT" from Quantity and Unit Price
# 3. Treatment of NAs, Missing values and outliers
# 4. Dropping the non-relevant columns 
##1. Dropping the Cancelled Invoices
retail_data1=as.data.frame(retail_data)

#abc=dt[dt$InvoiceNo %like% 'C'] #--extracts records having C
#below command is in sqldf package -similar to sql commands

retail_data1=sqldf("SELECT * FROM retail_data1 WHERE InvoiceNo NOT LIKE '%C%'")
tail(retail_data1$InvoiceNo,50)
str(retail_data1)
#c=sqldf("SELECT * FROM retail_data1 WHERE InvoiceNo LIKE '%C%'")
#dim(c)
#rm(c)
#2. Creating a new column  "BILLAMOUNT" from Quantity and Unit Price
#this is using dplyr package
#retail_data2 <- as.table(retail_data1)
retail_data1 <- mutate(retail_data1, BILLAMOUNT = Quantity * UnitPrice)
#######3. Treatment of NAs, Missing values and outliers
#sum(is.na(retail_data1))
#sum(is.na(retail_data1$Quantity))
# 4. Dropping the non-relevant columns 
retail_data1=as.data.frame(retail_data1[,c(1,4,6,9)])
head(retail_data1)
#sum(is.na(retail_data1$CustomerID))
#num1=as.integer(retail_data1$CustomerID)
dim(retail_data1)
sum(is.na(retail_data1))
summary(retail_data1$BILLAMOUNT)
View(retail_data1)
#4. Treatment of NAs, Missing values and outliers
#sum(ifelse(retail_data1$Quantity <= 0, 1,0)) # 1336
#sum(ifelse(retail_data1$Quantity > 0 | retail_data1$UnitPrice > 0, 1,0)) # 531285

#up_neg=subset(retail_data1, retail_data1$UnitPrice <= 0) ## subset func is not recommended.
#str(up_neg)
#Removing the observations with Unit price <=0 and also quantity <=0
retail_data2=retail_data1[retail_data1$UnitPrice > 0 ,]

#str(retail_data2)
summary(retail_data2$UnitPrice)

boxplot(retail_data2$UnitPrice)
boxplot.stats(retail_data2$BILLAMOUNT)
hist(retail_data2$BILLAMOUNT)

#Considering only  the observations with Unit price < 10000 and also quantity  < 5000 as 
#there are only 2 observations above these values so they are outliers
retail_data2=retail_data2[retail_data2$UnitPrice < 10000,]
retail_data2=retail_data2[retail_data2$Quantity < 5000,]
dim(retail_data2)
retail_data2=na.omit(retail_data2)
retail_data2[(retail_data2$UnitPrice > 3500 & retail_data2$UnitPrice < 10000  ),]
dim(retail_data2)
summary(retail_data2)

####Cluster fitting
retail_data3=retail_data2[,c(2,4)]
str(retail_data3)






## fitting k means

set.seed(30)
wcss<-vector()
for(i in 1:25)wcss[i]<-sum(kmeans(retail_data3,i)$withinss)
plot(1:25,wcss,type ="b",main = "Order/Qty. clusters",xlab = "no of.clusters",ylab = "wcss")

#fitting the kmeans to the dataset
set.seed(7)
kmeans=kmeans(retail_data2[,c(2,3)],14,iter.max=10)
#visualising the clusters for this 2 dimensional variables

clusplot(retail_data2[,c(2,3)],kmeans$cluster,lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = TRUE,
         plotchar = FALSE,
         # labels=2,
         span = TRUE,
         main="clusters of order/qty.",
         xlab = "quantity",
         ylab = "Amount")







