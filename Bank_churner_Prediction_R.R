# load required libraries
library(tidyverse)
library(sf)
library(plotly)
library(caret)
library(likert)
library(grid)
library(gridExtra)
library(ggpubr)
library(tmap)
library(sp)
library(datasets)
library(GGally)
library(RColorBrewer)
library(ggspatial)
library(data.table)
library(ggplot2) 
library(corrplot)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(randomForest)
library(reprtree)
library(rpart)
library(visNetwork)
library(rgl)
library(kernlab)
library(forcats)
library(gbm)
library(gstat)
library(visNetwork)
library(rgl)
library(cluster)
library(sparkline)

#setting the working directory
setwd("C:\\Users\\W0475475\\OneDrive - Nova Scotia Community College\\R activities")
# loading the dataset
Bankchurner <- read.csv("C:\\Users\\W0475475\\OneDrive - Nova Scotia Community College\\R activities\\Bankchurners.csv")
#summarizing the dataset
summary(Bankchurner)
knitr::kable(head(Bankchurner))
# getting the column names
colnames(Bankchurner)
str(Bankchurner)
# Dimension of the dataset
dim(Bankchurner)

# Types of Attributes
sapply(Bankchurner, class)

# Removing the Unknown character in Income Category
Bankchurners<- Bankchurner %>%
  filter(!grepl('Unknown', Income_Category))
view(Bankchurners)
# removing the unwanted variable
df= subset(Bankchurners, select = -c(CLIENTNUM, 
  Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2,
  Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1))
df
#Column Description
#CLIENTNUM: Client number. Unique identifier for the customer holding the account
#Attrition_Flag: Internal event (customer activity) variable - if the account is closed then 1 else 0
#Customer_Age: Demographic variable - Customer's Age in Years
#Gender: Demographic variable - M=Male, F=Female
#Dependent_count: Demographic variable - Number of dependents
#Education_Level: Demographic variable - Educational Qualification of the account holder (example: high school, college graduate, etc.)
#Marital_Status: Demographic variable - Married, Single, Divorced, Unknown
#Income_Category: Demographic variable - Annual Income Category of the account holder (< 40????, 40K - 60K, 60??????? 80K, 80??????? 120K, >
#Card_Category: Product Variable - Type of Card (Blue, Silver, Gold, Platinum)
#Monthsonbook: Period of relationship with bank
#TotalRelationshipcount: Total no. of products held by the customer
#MonthsInactive12_mon: No. of months inactive in the last 12 months
#ContactsCount12_mon: No. of Contacts in the last 12 months
#Credit_Limit: Credit Limit on the Credit Card
#TotalRevolvingBal: Total Revolving Balance on the Credit Card
#AvgOpenTo_Buy: Open to Buy Credit Line (Average of last 12 months)
#TotalAmtChngQ4Q1: Change in Transaction Amount (Q4 over Q1)
#TotalTransAmt: Total Transaction Amount (Last 12 months)
#TotalTransCt: Total Transaction Count (Last 12 months)
#TotalCtChngQ4Q1: Change in Transaction Count (Q4 over Q1)
#AvgUtilizationRatio: Average Card Utilization Ratio
#NaiveBayesClassifier_attribution Naive BayesNaiveBayesClassifier_attribution Naive Bayes



# Data Preparation
qw <- as.data.frame(df) %>% 
  mutate(Income_Category = factor(Income_Category),Gender = factor(Gender), Attrition_Flag = factor(Attrition_Flag),
         Months_Inactive_12_mon = factor(Months_Inactive_12_mon),Marital_Status = factor(Marital_Status),
         Education_Level = factor(Education_Level), Card_Category = factor(Card_Category))
view(qw)

# data subsetting
#subsetting  the dataset into 3 variables; categorical, numerical and id.


# subsetting the data
#categorical data

Categorical_variable <- qw %>% 
  select("Total_Relationship_Count", "Card_Category","Gender","Marital_Status","Education_Level","Attrition_Flag", "Months_Inactive_12_mon" , "Income_Category")

# knowing the class the variable belongs to
class(Categorical_variable$Gender)


is.factor(Categorical_variable$Gender)

# Levels of the class
levels(Categorical_variable$Attrition_Flag)
levels(Categorical_variable$Months_Inactive_12_mon)
levels(Categorical_variable$Income_Category)

# class distribution
percentage <- prop.table(table(Categorical_variable$Income_Category))
percentage

# numerical variables
numerical_variable <- df %>% 
  select('Customer_Age','Dependent_count',
         'Contacts_Count_12_mon', 'Credit_Limit', 'Total_Revolving_Bal',
         'Avg_Open_To_Buy', 'Total_Amt_Chng_Q4_Q1', 'Total_Trans_Amt',
         'Total_Trans_Ct', 'Total_Ct_Chng_Q4_Q1', 'Avg_Utilization_Ratio')

# Exploratory Data Analysis

# scatter plot of the numerical variable
ggplot(df, aes(x = Avg_Open_To_Buy, y = Total_Trans_Amt)) +
  geom_point() +
  stat_smooth(method = lm)

ggplot(df, aes(x = Credit_Limit, y = Total_Trans_Amt)) +
  geom_point(alpha= 10, color= "Black") +
  stat_smooth(method = lm)

ggplot(df, aes(x = Credit_Limit, y = Card_Category)) +
  geom_point(alpha= 10, color= "Black") +
  stat_smooth(method = lm)


# Barchart for the categorical
positions <- c("Attrited Customer", "Existing Customer")
a1 <- ggplot(Categorical_variable, aes(x = Attrition_Flag)) + 
      geom_bar(stat = "count", width=0.7)+
      scale_x_discrete(limits = positions)
ggplotly(a1)


positions2 <- c(0,1,2,3,4,5,6,7,8,9,10,11,12)
a2 <- ggplot(Categorical_variable, aes(x=Months_Inactive_12_mon)) + 
      geom_bar(stat = "count", width=0.7,)+
      scale_x_discrete(limits = factor(positions2))
     
ggplotly(a2)


positions3 <- c("Divorced", "Married", "Single", "Unknown")
a3 <- ggplot(Categorical_variable, aes(x=Marital_Status)) + 
      geom_bar(stat = "count", width=0.8)+
      scale_x_discrete(limits = positions3)
ggplotly(a3)

positions4 <- c(1,2,3,4,5,6)
a4 <- ggplot(Categorical_variable, aes(x=Total_Relationship_Count)) + 
  geom_bar(stat = "count", width=0.7,)+
  scale_x_discrete(limits = factor(positions4))

ggplotly(a4)

# Density plot of the numerical variables
reg <- numerical_variable %>%
  filter(Customer_Age > 0) %>% 
  ggplot(aes(x = Customer_Age))+
  geom_density(alpha=0.5, fill="grey25")
ggplotly(reg)


eg <-numerical_variable %>%
  filter(Contacts_Count_12_mon > 0) %>% 
  ggplot(aes(x = Contacts_Count_12_mon))+
  geom_density(alpha=0.5, fill="grey25")
ggplotly(eg)


g <- numerical_variable %>%
  filter(Total_Trans_Ct > 0) %>% 
  ggplot(aes(x = Total_Trans_Ct))+
  geom_density(alpha=0.5, fill="grey25")
ggplotly(g)


# Histogram for Numerical variable

c1 <- numerical_variable %>% 
  filter(Total_Trans_Amt > 500) %>% 
  ggplot(aes(x=Total_Trans_Amt)) + ggtitle("Total_Trans_Amt") +
  geom_histogram( binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 10") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
ggplotly(c1)


c2 <- numerical_variable %>% 
  filter(Dependent_count > 0) %>% 
  ggplot(aes(x=Dependent_count)) + ggtitle("Dependent_count") +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Dependent Count") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
ggplotly(c2)

c3 <-numerical_variable %>% 
  filter(Avg_Utilization_Ratio > 0) %>% 
  ggplot(aes(x=Avg_Utilization_Ratio)) + ggtitle("Average card utilization ratio") +
  geom_histogram( binwidth=0.1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Dependent Count") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
ggplotly(c3)


# violin plot showing the relationship between a numeric variable and categorical variable
ab <- qw %>%
  ggplot( aes(x=Attrition_Flag, y=Credit_Limit, fill=Attrition_Flag)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")
ggplotly(ab)

# heatmaps of categorical status  
heatmap <- Categorical_variable %>%
  select(Attrition_Flag, Card_Category) %>%
  xtabs(~., data=.) %>%
  data.frame() %>%
  ggplot(aes(Attrition_Flag,Card_Category, fill=Freq))+
  geom_tile(col="white")+
  xlab("Attrition_Flag")+ylab("Card_Category")+
  scale_fill_gradient(low="green", high="darkgreen")
heatmap  


heatmap <- Categorical_variable %>%
  select(Months_Inactive_12_mon, Card_Category) %>%
  xtabs(~., data=.) %>%
  data.frame() %>%
  ggplot(aes(Months_Inactive_12_mon,Card_Category, fill=Freq))+
  geom_tile(col="white")+
  xlab("Months_Inactive_12_mon")+ylab("Card_Category")+
  scale_fill_gradient(low="green", high="darkgreen")
heatmap

#build a regression tree using rpart()
set.seed(2)
sample(1:nrow(qw))
tree_model <- rpart(qw$Card_Category ~., data = qw %>%
                      mutate_if(is.factor, as.character))

#visualize a regression tree using output using visNetwork package
visTree(tree_model, legend = F, collapse = T, direction = "LR")


# training data
a <- qw$Credit_Limit
# creating a training and validation data
set.seed(123)
inTraining <- createDataPartition(qw$Card_Category, p=0.80, list=FALSE)
training <- qw[inTraining,]
validation <- qw[-inTraining,]

# get counts for Province distribution in validation dataset
tapply(validation$Card_Category, validation$Income_Category, length)

# run algorithms (10-fold cross validation)
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# linear algorithm
set.seed(123)
fit.lda <- train(Card_Category~., data=qw, method="lda", metric=metric, trControl=control)
predictions <- predict(fit.lda, validation)
predictions


cm <- confusionMatrix(predictions, as.factor(validation$Card_Category))
cm

cm_d <- as.data.frame(cm$table)
cm_d$diag <- cm_d$Prediction == cm_d$Reference # Get the Diagonal
cm_d$ndiag <- cm_d$Prediction != cm_d$Reference # Off Diagonal     
cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
cm_d$Reference <-  reverse.levels(cm_d$Reference) # diagonal starts at top left
cm_d$ref_freq <- cm_d$Freq * ifelse(is.na(cm_d$diag),-1,1)

plt1 <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
  scale_x_discrete(position = "top") +
  geom_tile( data = cm_d,aes(fill = ref_freq)) +
  scale_fill_gradient2(guide = FALSE ,low="red3",high="orchid4", midpoint = 0,na.value = 'white') +
  geom_text(aes(label = Freq), color = 'black', size = 3)+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
  )
plt1


# C&RT algorithm
fit.cart <- train(Card_Category~., data=qw, method="rpart", metric=metric, trControl=control)
predictions <- predict(fit.cart, validation)
predictions

cm1 <- confusionMatrix(predictions, as.factor(validation$Card_Category))
cm1

# knn algorithm
set.seed(123)
fit.knn <- train(Card_Category~., data=qw, method="knn", metric=metric, trControl=control)
predictions <- predict(fit.knn, validation)
predictions

cm2 <- confusionMatrix(predictions, as.factor(validation$Card_Category))
cm2

# SVMs
set.seed(123)
fit.svm <- train(Card_Category~., data=qw, method="svmRadial", metric=metric, trControl=control)
predictions <- predict(fit.svm, validation)
predictions

cm3 <- confusionMatrix(predictions, as.factor(validation$Card_Category))
cm3

# Random Forest
set.seed(123)
fit.rf <- train(Card_Category~., data=df, method="rf", metric=metric, trControl=control)
predictions <- predict(fit.rf, validation)
predictions

# summary of results
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

results_df <- as.data.frame(results)

results_tidy <- results_df %>% 
  pivot_longer(names_to = "Model", values_to = "Accuracy", -Resample) %>% 
  group_by(Model) %>% 
  summarise(Mean_Accuracy = mean(Accuracy))

mean_acc <- results_tidy %>% 
  ggplot(aes(x=fct_reorder(Model, Mean_Accuracy), y=Mean_Accuracy))+
  geom_bar(stat = "identity")+
  coord_flip()+
  xlab("Mean Accuracy")+
  ylab("Model")+
  theme(text = element_text(size = 20))

mean_acc

# determining variable importance

importance1 <- varImp(fit.lda)
importance2 <- varImp(fit.cart)
importance3 <- varImp(fit.knn)
importance4 <- varImp(fit.svm)
importance5 <- varImp(fit.rf)

imp1 <- importance1$importance 
imp2 <- importance2$importance
imp3 <- importance3$importance
imp4 <- importance4$importance
imp5 <- importance5$importance


p1 <- imp1 %>% 
  mutate(Predictor = rownames(imp1)) %>% 
  pivot_longer(names_to = "Province", values_to = "Importance", -Predictor) %>%
  ggplot(aes(x=Predictor, y=Importance))+
  geom_segment(aes(x=Predictor, xend=Predictor, y=0, yend=Importance), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  ylab("Linear Regression")+
  xlab("")

p2 <- imp2 %>% 
  mutate(Predictor = rownames(imp2)) %>% 
  pivot_longer(names_to = "Province", values_to = "Importance", -Predictor) %>%
  ggplot(aes(x=Predictor, y=Importance))+
  geom_segment(aes(x=Predictor, xend=Predictor, y=0, yend=Importance), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  ylab("Classification & Regression Tree")+
  xlab("")

p3 <- imp3 %>% 
  mutate(Predictor = rownames(imp3)) %>% 
  pivot_longer(names_to = "Province", values_to = "Importance", -Predictor) %>%
  ggplot(aes(x=Predictor, y=Importance))+
  geom_segment(aes(x=Predictor, xend=Predictor, y=0, yend=Importance), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  ylab("k-Nearest Neighbors")

p4 <- imp4 %>% 
  mutate(Predictor = rownames(imp4)) %>% 
  pivot_longer(names_to = "Province", values_to = "Importance", -Predictor) %>%
  ggplot(aes(x=Predictor, y=Importance))+
  geom_segment(aes(x=Predictor, xend=Predictor, y=0, yend=Importance), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  ylab("Support Vector Machine")+
  xlab("")

p5 <- imp5 %>% 
  mutate(Predictor = rownames(imp5)) %>% 
  pivot_longer(names_to = "Province", values_to = "Importance", -Predictor) %>%
  ggplot(aes(x=Predictor, y=Importance))+
  geom_segment(aes(x=Predictor, xend=Predictor, y=0, yend=Importance), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  ylab("Random Forest")+
  xlab("")

plot_importance <- ggarrange(p1, p2, p3, p4, p5, ncol=1)
plot_importance

