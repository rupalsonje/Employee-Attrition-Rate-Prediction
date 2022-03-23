library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)

attrition.df <- read.csv("C:\\Users\\nikhi\\Downloads\\kaggle\\HR-Employee-Attrition.csv", stringsAsFactors = TRUE)
str(attrition.df)

head(attrition.df)

attrition.df %>% 
  select(Attrition) %>%
  group_by(Attrition) %>% 
  summarize(N = n()) %>% 
  mutate(percent = round(prop.table(N), 2))

summary(attrition.df$EducationField)

library(repr)
options(repr.plot.width=10, repr.plot.height = 10) 
ggplot(data=attrition.df)+
  geom_bar(aes(x=EducationField,color=Attrition,fill=Attrition))+
  ggtitle("Field of Education vs Attrition ") + 
  labs(x = "EducationField", y = "Count")

attrition.df %>% 
  select(Attrition,Gender,EducationField) %>%
  filter (Attrition=="Yes") %>%
  group_by(EducationField,Gender) %>% 
  summarise(Count=n()) 

attr_new <- attrition.df %>% 
  select(Attrition,Gender,EducationField) %>%
  filter (Attrition=="Yes") %>%
  group_by(EducationField,Gender) %>% 
  summarise(Count=n()) 

ggplot(data=attr_new,aes(x=EducationField,y = Count,color=Gender,fill=Gender))+
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Field of Education vs Gender ")+
  labs(x = "EducationField", y = "Count")

attrition.df %>% 
  summarise(Median = median(MonthlyIncome), 
            Mean = mean(MonthlyIncome),
            Max = max(MonthlyIncome), 
            Min = min(MonthlyIncome))

ggplot(attrition.df, aes(x=Attrition, y=MonthlyIncome, color=Gender, fill=Gender)) +
  geom_boxplot()

ggplot(attrition.df, aes(x=MonthlyIncome,color=Attrition,fill=Attrition)) +
  geom_histogram(position="identity", alpha=0.7)+
  ggtitle("Distribution of Monthly Income ")+
  labs(x = "Monthly Income", y = "Count")

ggplot(attrition.df, aes(x=OverTime, y=MonthlyIncome, color=Gender, fill=Gender)) +
  geom_boxplot()


### model 1 logistic regression
##Step 1: Read data
attrition.df <- read.csv("C:\\Users\\nikhi\\Downloads\\kaggle\\HR-Employee-Attrition.csv", stringsAsFactors = TRUE)
#str(attrition.df)

#preprocessing the data
#all relevant variables are categorical
#base level for attrition is "No" . changing it to "Yes"
attrition.df$Attrition<- as.numeric(attrition.df$Attrition == "Yes")
attrition.df$Attrition <- factor(attrition.df$Attrition)

#Set base level for categorical variables 
#attrition.df$Attrition<- relevel(attrition.df$Attrition, ref = "1")
#(attrition.df$Attrition)

### Step 3: Partition data
set.seed(132)
# select variables
#selected.var <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
selected.var <- c(29,19,16,23,1,4,15,6,32,13,2)
selected.df <- attrition.df[, selected.var]
str(selected.df)
#selected.df <- attrition.df[, 1:ncol(attrition.df)]
# partition the data
train.index <- sample(1:nrow(attrition.df), nrow(attrition.df)*0.7)  
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

### Step 4: Fit a logistic regression model
logit.reg <- glm(Attrition ~ ., data = train.df, family = "binomial")
options(scipen = 999) 
summary(logit.reg)

### Step 5: Generate outcome by comparing predicted probability with the cutoff probability
logit.reg.pred <- predict(logit.reg, valid.df,  type = "response")
pred <- ifelse(logit.reg.pred > 0.3, 1, 0)
library(caret)
confusionMatrix(factor(pred), factor(valid.df$Attrition), positive = "1")
#5 manual calculation using logit

###Step 6: Generate ROC curve
library(pROC)
logit.reg.pred <- predict(logit.reg, valid.df,  type = "response")
pred <- ifelse(logit.reg.pred > 0.3, 1, 0)

pred <- factor(ifelse(logit.reg.pred > 0.1,1, 0))
test_actual_Attr <- factor(ifelse(valid.df$Attrition==1,"Yes","No"))
table(pred,test_actual_Attr)


r <- roc(valid.df$Attrition, logit.reg.pred)
plot.roc(r)

# find the best threshold, with a default best method "youden"
coords(r, x = "best")

coords(r, x = c(0.1, 0.2, 0.5))

### model 2 logistic regression
##Step 1: Read data
attrition.df <- read.csv("C:\\Users\\nikhi\\Downloads\\kaggle\\HR-Employee-Attrition.csv", stringsAsFactors = TRUE)
#str(attrition.df)

##Step 2: Preprocess the data
#all relevant variables are categorical
#base level for attrition is "No" . changing it to "Yes"
attrition.df$Attrition<- as.numeric(attrition.df$Attrition == "Yes")
attrition.df$Attrition <- factor(attrition.df$Attrition)

#Set base level for categorical variables 
#attrition.df$Attrition<- relevel(attrition.df$Attrition, ref = "1")
#levels(attrition.df$Attrition)

### Step 3: Partition data
set.seed(132)
# select variables
#selected.var <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
#
selected.var <- c(3,28,5,11,8,20,35,34,17,10,2)
selected.var
#selected.var <- c()
selected.df <- attrition.df[, selected.var]
#selected.df <- attrition.df[, 1:ncol(attrition.df)]
# partition the data
train.index <- sample(1:nrow(attrition.df), nrow(attrition.df)*0.7)  
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

### Step 4: Fit a logistic regression model
logit.reg <- glm(Attrition ~ ., data = train.df, family = "binomial")
options(scipen = 999) 
summary(logit.reg)

### Step 5: Generate outcome by comparing predicted probability with the cutoff probability
logit.reg.pred <- predict(logit.reg, valid.df,  type = "response")
pred <- ifelse(logit.reg.pred > 0.3, 1, 0)
library(caret)
confusionMatrix(factor(pred), factor(valid.df$Attrition), positive = "1")
#5 manual calculation using logit

###Step 6: Generate ROC curve
library(pROC)
logit.reg.pred <- predict(logit.reg, valid.df,  type = "response")
pred <- ifelse(logit.reg.pred > 0.3, 1, 0)
#
pred <- factor(ifelse(logit.reg.pred > 0.2,1, 0))
test_actual_Attr <- factor(ifelse(valid.df$Attrition==1,"Yes","No"))
table(pred,test_actual_Attr)
#
r <- roc(valid.df$Attrition, logit.reg.pred)
plot.roc(r)

# find the best threshold, with a default best method "youden"
coords(r, x = "best")

coords(r, x = c(0.1, 0.2, 0.5))
