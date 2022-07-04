library(dplyr)
library(ggplot2)
# Importing the data set
df<- read.csv("D:/1. Big Data Analytics (Sem 2)/5. Credit Card Fraud Detection/CCFD.csv", header=TRUE)
View(df)
credit_card<-(df%>% select(-23,-24,-26))
# to view the data set (structure)
str(credit_card)
View(credit_card)

# class is number, but convert it to categorical
# v1- reduced versions of actual values
credit_card$Class=factor(credit_card$Class, levels=c(0,1))

# Summary of the data set
summary(credit_card)

# Missing values
sum(is.na(credit_card))

#No missing values found

# No of legit and fraud transactions
table(credit_card$Class)

# percentage of fraud and legit cases
prop.table(table(credit_card$Class))


# Pie chart of credit card transactions
labels<- c("legit","fraud")
labels<- paste(labels, round(100*prop.table(table(credit_card$Class)),2))
labels<- paste0(labels,"%")
pie(table(credit_card$Class),labels, col=c("orange","red"),main="Pie chart of credit card transactions")

#---------------------------------------------------------------------------------------------------------------------
#BASIC EDA
# Variable Format Changes
# Make the variable of interest (Fraud) a factor and re-level the variable for later interpretation in the models, this makes that the fraud level can be interpreted as Y=1.'''

names(credit_card)[names(credit_card)=="Class"] <- "Fraud"
credit_card$Fraud <- factor(credit_card$Fraud, labels = c("Normal", "Fraud"))
credit_card$Fraud <- relevel(credit_card$Fraud, "Fraud")


credit_card%>%
  group_by(Fraud)%>%
  summarise(n = n())%>%
  mutate(percentage = n/sum(n)*100)%>%
  ggplot(aes(Fraud, n, fill=Fraud))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=n), vjust=-0.3, hjust = 1, size=4.5)+
  geom_text(aes(label=paste0("( ",round(percentage,2),"% )")), vjust=-0.3, hjust = -0.1, size=4.5)+
  ylab("transactions")+
  xlab("")

#See the relation between the amount of the transaction and if it is fraudulent or not.
credit_card%>%
  group_by(Fraud)%>%
  ggplot()+
  geom_density(aes(Amount, fill = Fraud), alpha = .5)+
  scale_x_log10()


credit_card%>%
  group_by(Fraud)%>%
  ggplot()+
  geom_density(aes(V3, fill = Fraud), alpha = .5)

credit_card%>%
  group_by(Fraud)%>%
  ggplot()+
  geom_density(aes(V4, fill = Fraud), alpha = .5)



credit_card%>%
  group_by(Fraud)%>%
  ggplot()+
  geom_density(aes(V2, fill = Fraud), alpha = .5)

credit_card%>%
  group_by(Fraud)%>%
  ggplot()+
  geom_density(aes(V25, fill = Fraud), alpha = .5)

boxplot(credit_card$Amount, horizontal=T)

boxplot(credit_card$Amount, data = , xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")
#-------------------------------------------------------
# waste No model predictions
predictions=rep.int(0,nrow(credit_card))
predictions=factor(predictions, levels=c(0,1))
predictions
#-------------------------------------------------------
# 0 - legit , 1 - fraud
library(caret)
confusionMatrix(data=predictions, reference=credit_card$Class)

#----------------------------------------------------------------------------------------------------------------------

# to take a subset of the dataset (10%)
library(dplyr)
set.seed(1)  # to get same sample again
credit_card<- credit_card %>%sample_frac(0.1)
table(credit_card$Class)

library(ggplot2)
#dev.off()

ggplot(data=credit_card, aes(x=V1, y=V2, col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values=c('blue','red'))
#--------------------------------------------------------------------------------------------------------------

# Creating training and testing dataset
 

library('caTools')
set.seed(123)
data_sample= sample.split(credit_card$Class, SplitRatio=0.80)
train_data= subset(credit_card, data_sample==TRUE)
test_data = subset(credit_card, data_sample==FALSE)

dim(train_data)
dim(test_data)

#---------------------------------------------------------------------------


#Random over sampling to balance the data
# increase fraud


table(train_data$Class)
n_legit<-22750
new_frac_legit<-0.5  #(no of fraud)
new_n_total<- n_legit/new_frac_legit # no of rows


library("ROSE")
oversampling_result <- ovun.sample(Class~.,
                                   data=train_data,
                                   method="over",
                                   N=new_n_total,
                                   seed=2019)
oversampled_credit<- oversampling_result$data
table(oversampled_credit$Class)

ggplot(data=oversampled_credit, aes(x=V1, y=V2, col=Class))+
  geom_point(position=position_jitter(width=0.2))+
  theme_bw()+
  scale_color_manual(values=c('blue','red'))

# ------------------------------------------------------------------------------------

# Random Under sampling
# Reduce number of legit cases
table(train_data$Class)

n_fraud<-35
new_frac_fraud<-0.50
new_n_total<-n_fraud/new_frac_fraud

undersampling_result<- ovun.sample(Class~.,
                                   data=train_data,
                                   method="under",
                                   N=new_n_total,
                                   seed=2019)

undersampled_credit<- undersampling_result$data

table(undersampled_credit$Class)
confusionMatrix(data=undersampled_credit, reference=credit_card$Class)
ggplot(data=undersampled_credit, aes(x=V1, y=V2, col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values=c('blue','red'))
# data loss


# Let's perform ROS and RUS together

n_new<- nrow(train_data)
fraction_fraud_new<-0.50

sampling_result<- ovun.sample(Class~.,
                              data=train_data,
                              method="both",
                              p=fraction_fraud_new,
                              seed=2019
                              )
sampled_credit<- sampling_result$data
table(sampled_credit$Class)

prop.table(table(sampled_credit$Class))

ggplot(data=sampled_credit, aes(x=V1, y=V2, col=Class))+
  geom_point(position=position_jitter(width=0.2))+
  theme_bw()+
  scale_color_manual(values=c('blue','red'))

# using SMOTE ( synthetic minority over-sampling technique)

library(smotefamily)
table(train_data$Class)

n0<- 22750
n1<-35
r0<- 0.6  # 60% legit and 40% fraud

ntimes<- ((1-r0)/r0)*(n0/n1)-1

smote_output = SMOTE(X= train_data[ , -c(1,31)],
                     target=train_data$Class,
                     K=5,
                     dup_size=ntimes)


credit_smote<- smote_output$data

colnames(credit_smote)[30]<- "Class"

prop.table(table(credit_smote$Class))

ggplot(data=train_data, aes(x=V1, y=V2, col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values=c('blue','red'))

ggplot(data=credit_smote, aes(x=V1, y=V2, col=Class))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values=c('blue','red'))

library(rpart)
library(rpart.plot)

cart_model<- rpart(Class~. , credit_smote)
rpart.plot(cart_model, extra=0, type=5, tweak=1.2)
predicted_val<- predict(cart_model, test_data, type='class')
predicted_val

# confusion matrix
library(caret)
confusionMatrix(predicted_val, test_data$Class)


#-------------------------------------------------------------

# Decision tree without SMOTE

cart_model<- rpart(Class~., train_data[,-1])
rpart.plot(cart_model, extra=0, type=5, tweak=1.2)
predicted_val<- predict(cart_model, test_data[,-1],type="class")
confusionMatrix(predicted_val, test_data$Class)

# so with smote its better

predicted_val<- predict(cart_model, credit_card[,-1],type="class")
confusionMatrix(predicted_val, credit_card$Class)


# conclusion - 
'''
SMOTE was best compared to Random under sampling and Random over sampling
23, 24, 26 th columns had very less p value and hence they were rejected
Decision tree, logistic Regression results were not very accurate, so we decided to use xgboost


