customer_churn <- read.csv("churn_data.csv")
customer_churn
str(customer_churn)
summary(customer_churn)
#####Correlation plot of all features #####
cor(customer_churn)
head(customer_churn)
tail(customer_churn)
is.na(customer_churn)
na.omit(customer_churn)

##### produce one correlation plot #####
Project_cor <- cor(customer_churn,method="pearson")
corrplot(Project_cor,method="circle",mar=c(1,1,0,1))

#### produce one density and histogram plots together####
usage_den <- density(customer_churn$DataUsage) 
plot(usage_den,col=4,main="Plot for density",sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")))


Daycals_den <- density(customer_churn$DayCalls)
plot(usage_den,col=4,main="Plot for density",sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")))
Daycals_den


hist(customer_churn$DataUsage,main="Data Usage",prob=TRUE,
xlab="Usage in GB",ylab="Density",
sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")),col=4)

lines(usage_den,col=5)

#### produce pie plot/bar plot and a histogram based on the features of your choice in your dataset ###
T=table(customer_churn$DataPlan)
T
barplot(T,main="Bar Plot of Telecom customer churn",xlab="Active Data Plan",ylab="Frequency",sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")),col=4)

#### pie chart ###
T=table(customer_churn$DataPlan)
T
colors<-c("red","blue")
pie(T,main="Pie Chart of Telecom customer churn",xlab="Active Data Plan",ylab="Frequency",sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")),col=colors)

#### Histogram Chart ####

hist(customer_churn$DataUsage,main="Histogram of Data Usage",xlab="Data Usage in GB",ylab="Frequency",sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")),col=4)

#### Count no of customers that use >= 1GB ####
High_Data_Users <- sqldf("select count(DataUsage) from customer_churn where DataUsage >=1" )
High_Data_Users
Low_Data_Users <- sqldf("select count(DataUsage) from customer_churn where DataUsage <1" )
Low_Data_Users
Rev <- sqldf("select sum(MonthlyCharge + OverageFee) As Rev,DataUsagecat from customer_churn group by DataUsagecat" )
Rev1 <- sqldf("select sum(MonthlyCharge + OverageFee) As Rev from customer_churn" )
Rev1

#### Create Sub-categories of Data Users ####
customer_churn$DataUsagecat[customer_churn$DataUsage <1] <- "Low_Users"
customer_churn$DataUsagecat[customer_churn$DataUsage >=1 & customer_churn$DataUsage <=2.7] <-"Avg_Users"
customer_churn$DataUsagecat[customer_churn$DataUsage >2.7] <-"High_Users"

is.na(customer_churn)
#### Create a Distribution for DataUsers ####
A=table(customer_churn$DataUsagecat)
A
Usage_plot <- barplot(A,main="Categorization of Data Users",sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")),col=4)
data_plot <- barplot(place,main="Categorization of Data Users",sub=paste("R",format(Sys.time(),"%Y-%b-%d%H:%S")),col=4)
B=table(Rev)
B
place<-c(34325,44635,142206)

plot(customer_churn$DataUsage,customer_churn$DataPlan,lty=2,pch=10,xlab="Width",ylab="Lenght",col="red")


#### Check the number of colmns and rows ########
names(customer_churn)
str(customer_churn)
dim(customer_churn)

#### Use Mice Package to report the pattern including the related plot ###
md.pattern(customer_churn)

##### Check the existence of NA and exclude them ######
is.na(customer_churn)

