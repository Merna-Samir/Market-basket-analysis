
DataSetPath<-readline("Enter dataset path: ")
#The path in which that data is located, which will be taken from the user


numberOfClusters<-as.integer(readline("Enter the numbers of clusters between 2 and 4:"))
#The number of clusters that user will enter and will be used within the k 
#as.integer to convert the number which the user will enter from character to integer

minSupport<-as.numeric(readline("Enter the Minimum Apriori Support between 0.001 and 1: "))
#The user enters the min support in order to be used within the apriori
#as.numeric to convert the number which the user will enter from character to numeric

minConfidence<-as.numeric(readline("Enter the Minimum Apriori confidence between 0.001 and 1: "))
#The user enters the minConfidence  in order to be used within the apriori
#as.numeric to convert the number which the user will enter from character to numeric


library("csv")
dataset<-read.csv(DataSetPath)


#clean of dataset
library(tidyr)
library(janitor)
library(tidyverse)
library(dplyr)
mydata1<-clean_names(dataset)
mydata2<-remove_empty(mydata1,which = c("rows","cols"),quiet = FALSE)
cleaningdata<-distinct(mydata2)
clean<-drop(cleaningdata)
str(clean)


######################################################################

library("dplyr")

pie(
  x=table(dataset$paymentType),
  #we use contingency table to display the (multivariate)frequancy distriputionof the variabls
  main="compare paymentType by count"
  #to display total spending paymentType in cash&credit.
)

########################################################

TotalsPerAge <-group_by(dataset,age) 
#to group the dataframe and to create a table display each age and its dataset.
TotalsPerAge <-summarise(TotalsPerAge,Total=sum(total))
#The output will have one row for each group.
TotalsPerAge <-arrange(TotalsPerAge,desc(Total))
#function arrange() can be used to reorder (or sort) rows by one or more variables.
TotalsPerAge

barplot(
  height =TotalsPerAge$Total,
  name = TotalsPerAge$age,
  col  ="purple",
  main ="compare age and sum total",
  xlab ="age",
  ylab ="Total",
  horiz=TRUE,
  las=1 )

############################################################

TotalsPerCity <-group_by(dataset,city)
#to group the dataframe and to create a table display each city and its dataset.
TotalsPerCity <-summarise(TotalsPerCity,Total=sum(total))
#The output will have one row for each group.
TotalsPerCity <-arrange( TotalsPerCity,desc(Total))
#function arrange() can be used to reorder (or sort) rows by one or more variables.
TotalsPerCity                      

barplot(
  height =TotalsPerCity$Total,
  name   =TotalsPerCity$city,
  col  ="pink",
  main ="compare city and sum total",
  xlab ="Total",
  ylab ="city",
  horiz=TRUE,
  las=1 
)

#######################################################################


par(mfrow=c(1,3))
#we use it as a matrix [1,3]to display the three charts in one chart 
pie(
  x=table(dataset$paymentType),
  main="compare paymentType by count",
)
barplot(
  height =TotalsPerAge$Total,
  name = TotalsPerAge$age,
  col  ="purple",
  main ="compare age and sum total",
  xlab ="age",
  ylab ="Total",
  horiz=TRUE,
  las=1 
)
TotalsPerCity <-group_by(dataset,city)
TotalsPerCity <-summarise(TotalsPerCity,Total=sum(total))
TotalsPerCity <-arrange( TotalsPerCity,desc(Total))

barplot(
  height =TotalsPerCity$Total,
  name   =TotalsPerCity$city,
  col  ="pink",
  main ="compare city and sum total",
  xlab ="Total",
  ylab ="city",
  horiz=TRUE,
  las=1
)

#####################################################


boxplot(
  #A graphical representation of a quantitative values such as minimum, maximum, median.and It can identify outliers
  x=dataset$total,
  main= "Distribution of total spending",
  xlab= "total" #to display total spending
)


#####################################################

par(mfrow=c(2,2))
#we use it as a matrix [2,2]to display the four charts in one chart 
pie(
  x=table(dataset$paymentType),
  main="compare paymentType by count",
)

barplot(
  height =TotalsPerAge$Total,
  name   = TotalsPerAge$age,
  col  ="purple",
  main ="compare age and sum total",
  xlab ="age",
  ylab ="Total",
  horiz=TRUE,# to dispaly bars horizontaly
  las=1 #to display age horizontaly
)

TotalsPerCity <-group_by(dataset,city)
TotalsPerCity <-summarise(TotalsPerCity,Total=sum(total))
TotalsPerCity <-arrange(TotalsPerCity,desc(Total))
barplot(
  height =TotalsPerCity$Total,
  name   =TotalsPerCity$city,
  col  ="pink",
  main ="compare city and sum total",
  xlab ="total",
  ylab ="city",
  horiz=TRUE,# to dispaly bars horizontaly
  las=1 #to display city  horizontaly
)

boxplot(
  #A graphical representation of a quantitative values such as minimum, maximum, median.and It can identify outliers
  x=dataset$total,
  main= " Distribution of total spending", 
  xlab= "total"
)
###########

if(numberOfClusters>=2&numberOfClusters<=4){
  library("dplyr")
  library("gtools")
  
  selectOfdataset<-select(dataset,customer,age,total)
  selectOfdataset<-group_by(selectOfdataset,customer,age)
  selectOfdataset<-summarise(selectOfdataset,Total=sum(total))
  
  #we select from the main data customer,age,total
  #we use group_by and summarise to calculate the sum total for every customer without repeat 
  newTable<-data.frame(selectOfdataset)
  
  #we will put the sum total for every customer without repeat in data.frame
  DataKmean<-select(newTable,age,)
  
  #we select from the main data the age
  RESULT<-kmeans(DataKmean,centers=numberOfClusters )
  RESULT
  
  #we take from the result the numberOfClusters
  #if the numberOfClusters=n the data will divided by number n
  numberOfClusters<-RESULT$cluster
  reaultFinally<-mutate(newTable,numberOfClusters )
  
  # we added a new column contain  numberOfClusters and newTable
  reaultFinally
  
}else{
  print("the number in not between 2 and 4")
}
#the message that will appear to the user if he enter a wrong number


######################################################


rowDatat<-read.csv(DataSetPath , stringsAsFactors = FALSE)
# we use stringsAsFactors=FALSE to covert all data from factors to vectors 
transactions= strsplit(as.vector(rowDatat$items), ',')
#we select items from all data and convert every element in items by as.vector we he find','to vector t 

if(minSupport>=0.001&minSupport<=1&minConfidence>=0.001&minConfidence<=1){
  library("dplyr")
  library("arules")
  library("gtools")

  
  apriori_rules<-apriori(transactions,
                         parameter=list(supp=minSupport, conf=minConfidence,minlen=2))
  inspect(apriori_rules)
  #we use the function apriori_rules to dotransactions on items
}else{
  print("the number is not between 0.001 and 1")
}

print("if(minSupport & minConfidence) = 0.01 ")

apriori_rules1<-apriori(transactions,
                        parameter=list(supp=0.01, conf=0.01,minlen=2))
inspect(apriori_rules1)


