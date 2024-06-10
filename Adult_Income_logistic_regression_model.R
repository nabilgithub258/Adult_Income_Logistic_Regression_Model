library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)
library(ggplot2)
library(dplyr)

## Getting the data

adult <- read.csv('adult_sal.csv')

View(adult)

## Getting rid of extra column X

adult <- select(adult,- X)

View(adult)

summary(adult)

str(adult)

## Data cleaning

table(adult$type_employer)

table(adult$marital)

adult$type_employer <- gsub('Never-worked','Unemployed',adult$type_employer)
adult$type_employer <- gsub('Without-pay','Unemployed',adult$type_employer)

adult$type_employer <- gsub('Self-emp-inc','Self-emp',adult$type_employer)
adult$type_employer <- gsub('Self-emp-not-inc','Self-emp',adult$type_employer)
View(adult$type_employer)
View(adult)
table(adult$type_employer)

## Getting rid of the ? and replacing it with NA

adult[adult=='?'] <- NA

## Cleaning column type employer

adult$type_employer <- gsub(' Local-gov','SL-gov',adult$type_employer)
adult$type_employer <- gsub('State-gov','SL-gov',adult$type_employer)
table(adult$type_employer)

adult$type_employer <- gsub('Local-gov','SL-gov',adult$type_employer)
table(adult$type_employer)

## Cleaning the column education now

table(adult$education)

edu <- function(ed){
  ed <- as.character(ed)
  if (ed =='10th' | ed =='11th' |ed=='12th' | ed=='1st-4th' | ed=='5th-6th' | ed=='7th-8th' | ed=='9th' | ed=='Preschool'){
    return('School')
  }else if(ed == 'Assoc-acdm' | ed=='Assoc-voc'){
    return('Associate')
  }else{
    return(ed)
  }
}

adult$education <- sapply(adult$education,edu)

## Cleaning the column marital now

marital <- function(mar){
  mar <- as.character(mar)
  if (mar=='Seperated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
  }else if(mar =='Never-married'){
    return(mar)
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,marital)

table(adult$marital)

## Cleaning relationship

relation <- function(relate){
  relate <- as.character(relate)
  if(relate == 'Not-in-family' | relate=='Other-relative' | relate=='Own-child' | relate=='Unmarried'){
    return('Complicated')
  }else{
    return(relate)
  }
}

adult$relationship <- sapply(adult$relationship,relation)

table(adult$relationship)

## Cleaning country

table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

## Factoring

str(adult)

adult$education <- factor(adult$education)
adult$country <- factor(adult$country)
adult$marital <- factor(adult$marital)
adult$type_employer <- factor(adult$type_employer)
adult$relationship <- factor(adult$relationship)
adult$sex <- factor(adult$sex)
adult$income <- factor(adult$income)
str(adult)

any(is.na(adult))

## we need to repeat the factor function so we don't see that ? in the str

str(adult)

table(adult$type_employer)

## Using Amelia

missmap(adult,legend = TRUE,col = c('yellow','black'))

## Removing the NA value from the data set

adult <- na.omit(adult)

missmap(adult,legend = TRUE,col = c('yellow','black'))

## EDA time

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='pink',binwidth = 1) + theme_bw()

ggplot(adult,aes(hr_per_week)) + geom_histogram(aes(fill=income),color='pink',binwidth = 1) + theme_bw()

# rename the column name of country to region because it now does not make sense

adult <- rename(adult,region=country)

table(adult$region)

ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='pink',binwidth = 1) + theme_bw()

## Modeling

sample <- sample.split(adult$income,SplitRatio = 0.7)
train <- subset(adult,sample == TRUE)
test <- subset(adult,sample == FALSE)

str(train)
str(test)

model <- glm(income ~ . ,family = binomial(link='logit'),train)

View(train)
View(test)
View(adult)

summary(model)

## Using the predict 

test$predict.income <- predict(model,test,type='response')

table(test$income,test$predict.income >0.5)

## calculating how accurate how model is

acc <- (6409+1366)/(6409+511+929+1366)

## Our accuracy is 0.84

print(acc)

## Recall is 0.92

recall <- 6409/(6409+511)

print(recall)

## Precision is 0.87

precision <- 6409/(6409+929)

print(precision)