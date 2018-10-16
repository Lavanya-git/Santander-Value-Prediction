#Santander value prediction

#Read the csv file
transaction.data <- read.csv(file="train.csv", header=TRUE, sep=",")
test_non_zero_base<-read.csv("test.csv", header= TRUE, sep=",")
attach(transaction.data)
attach(test_non_zero_base)
options("scipen" = 999, "digits" = 10)
set_plot_dimensions <- function(width_choice, height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}
str(transaction.data, list.len = 10, vec.len = 5)

#summary of data
summary <- summary.data.frame(transaction.data)
summary[1:6, 1:10]

#include all packages
library(DataExplorer)
library(ggplot2)
library(data.table)
library(dplyr)
library(plotly)
library(e1071)
library(tidyr)
library(purrr)
library(compare)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(mltools)
library(psych)
library(rpart)
library(rpart.plot)
library(arules)
library(arulesViz)
library(knitr)
library(randomForest)

#plot_missing(transaction.data)
#transaction.data[!complete.cases(transaction.data),]
#sapply(transaction.data, function(x) sum(is.na(x)))
#Due to the size of the data set, commands above are difficult to print in the report
sum(is.na(transaction.data))

#Histogram of target variable
ggplot(transaction.data,aes(x=target))+geom_histogram(fill="blue",bins=50)+scale_x_continuous(trans='log2')+ggtitle("Histogram Distribution of Target")

#boxplot of target variable
box_plot <- ggplot(transaction.data, aes(y= target)) + 
  geom_boxplot() + 
  ylab("Target") +
  scale_y_continuous(trans='log2')+
  ggtitle("Box Plot of Target")
box_plot

#QQplot of target distribution
qqnorm(transaction.data$target,
       datax = TRUE,
       col = "red",
       main = "Normal Q-Q Plot of target Distribution")
qqline(transaction.data$target,
       col = "blue",
       datax = TRUE)

#statistics of target
(min(target))
(max(target))
(target_lcutoff <- quantile(target,.25))
(target_ucutoff <- quantile(target,.75))
(median(target))
(mean(target))

#To calculate number of zeros vs. the number of unique values, draw histogram
tran.data.zero<-data.table(transaction.data)
n_zeros <- tran.data.zero[, lapply(.SD, function(x) sum(x == 0) / length(x))] %>% unlist
a <-list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0.6,
  dtick = 0.1,
  range = c(0.6, 1),
  ticklen = 5,
  tickwidth = 2,
  tickcolor = toRGB("blue")
)
plot_ly(x = ~n_zeros, type = "histogram",
        marker = list(color = "dodgerblue")) %>% 
  layout(xaxis = a, title = "Histogram of % of zeros in dataset", titlefont = list(color = '#000000', size = 20), margin = list(l = 50, t=40))

x<-colSums(transaction.data != 0)
y<-colnames(transaction.data)
x_name<-"Count"
y_name<-"Col_name"
Train_nz<- data.frame(x, y)
colnames(Train_nz) <- c(x_name, y_name)
#Include columns with non_zero values greater than 800
Subset1<-Train_nz[Train_nz$Count>800,]
Subset1$Col_name<-as.character(Subset1$Col_name)
train_non_zero<-transaction.data[Subset1$Col_name]

#subset of the training data where columns have more than 800 non-zero values and rows have more than 650
w<-rowSums(transaction.data != 0)
t<-rownames(transaction.data)
w_name<-"Count"
t_name<-"Row_name"
Train_nz2<- data.frame(w, t)
colnames(Train_nz2) <- c(w_name, t_name)
#Include rows with non_zero values greater than 650
Subset1a<-Train_nz2[Train_nz2$Count>650,]
Subset1a$Row_name<-as.character(Subset1a$Row_name)
train_non_zero<-train_non_zero[Subset1a$Row_name,]
head(train_non_zero,3)

#create a csv file 
write.csv(train_non_zero, file = "train_non_zero.csv",row.names=FALSE)

#Perform Principal Component Analysis to further group the variables
train2<-subset(train_non_zero,select=-c(target,ID))
pc<-prcomp(train2)
summary(pc)
#plot(pc)
plot(pc,type="l")
set_plot_dimensions(1200, 1200)
biplot(pc, cex.lab=0.8, cex.axis=0.8, cex.main=0.7, cex.sub=0.5)
#to get the structure and summary of the non-zero subset 
str(train_non_zero, list.len = 10, vec.len = 5)
summary.subset <- summary.data.frame(train_non_zero)
summary.subset[1:6, 1:10]
plot_histogram(train_non_zero)
#correlation plot
plot_correlation(train_non_zero,type="continuous", theme_grey(base_size=5))

#Compare test and train base files
comparison <- compare(transaction.data,test_non_zero_base,allowAll=TRUE)
comparison$tM
semi_join(transaction.data,test_non_zero_base)

subset_colnames<-colnames(train_non_zero)
subset_ID<-as.character(train_non_zero$ID)
test_names<-names(test_non_zero_base)[names(test_non_zero_base) %in% subset_colnames]
test_ID<-test_non_zero_base$ID[test_non_zero_base$ID %in% subset_ID]
test_non_zero <-test_non_zero_base[, test_names]

z<-rowSums(test_non_zero_base != 0)
q<-rownames(test_non_zero_base)
z_name<-"Count"
q_name<-"Row_name"
Train_nz3<- data.frame(z, q)
colnames(Train_nz3) <- c(z_name, q_name)
#head(Train_nz3)
#Include rows with non_zero values greater than 950
Subset1b<-Train_nz3[Train_nz3$Count>950,]
Subset1b$Row_name<-as.character(Subset1b$Row_name)
#head(Subset1b$Row_name)
#str(Subset1b$Row_name)
test_row_names<-rownames(test_non_zero)[rownames(test_non_zero) %in% Subset1b$Row_name]
test_non_zero<-test_non_zero[test_row_names, ]
head(test_non_zero,3)
write.csv(test_non_zero, file = "test_non_zero.csv",row.names=FALSE)

#Compare test and train subsets
comparison <- compare(train_non_zero,test_non_zero,allowAll=TRUE)
comparison$tM
semi_join(train_non_zero,test_non_zero)

#Perform two sample T-test
mean1<-mean(train_non_zero[,3])
mean2<-mean(test_non_zero[,2])
sd1<-sd(train_non_zero[,3])
sd2<-sd(test_non_zero[,2])
l1<-length(train_non_zero[,3])
l2<-length(test_non_zero[,2])
dfs <- min(l1 - 1, l2 - 1)
tdata <- (mean1 - mean2) / sqrt((sd1^2/l1)+(sd2^2/l2))
pvalue <- 2*pt(tdata, df = dfs, lower.tail=FALSE)
tdata; pvalue

mean3<-mean(train_non_zero[,4])
mean4<-mean(test_non_zero[,3])
sd3<-sd(train_non_zero[,4])
sd4<-sd(test_non_zero[,3])
l3<-length(train_non_zero[,4])
l4<-length(test_non_zero[,3])
dfs <- min(l3 - 1, l4 - 1)
tdata1 <- (mean3 - mean4) / sqrt((sd3^2/l3)+(sd4^2/l4))
pvalue1 <- 2*pt(tdata1, df = dfs, lower.tail=FALSE)
tdata1; pvalue1

#standardise using z-score
Hide
train_non_zero_scaled<-scale(train_non_zero[,-1])
train_non_zero_scaled<-data.frame(train_non_zero_scaled)
train_non_zero_scaled$ID<-train_non_zero$ID 
train_non_zero_scaled<-train_non_zero_scaled[c(71,1:70)]
train_non_zero_scaled$target<-train_non_zero$target

#Plot correlation
plot_correlation(train_non_zero_scaled,type="continuous")
pairs(train_non_zero_scaled[2:10])

test_non_zero_scaled<-scale(test_non_zero[,-1])
test_non_zero_scaled<-data.frame(test_non_zero_scaled)
test_non_zero_scaled$ID<-test_non_zero$ID 
test_non_zero_scaled<-test_non_zero_scaled[c(69,1:68)]

outliers <- function(dataframe){
  dataframe %>%
    select_if(is.numeric) %>% 
    map(~ boxplot.stats(.x)$out) 
}
head(outliers(train_non_zero_scaled), 5)
summary(train_non_zero_scaled$target)

Hide
train_non_zero_scaled$log_target<-log(train_non_zero_scaled$target)
train_non_zero_scaled<-train_non_zero_scaled[c(1:2,72,3:71)]
skew(train_non_zero_scaled$log_target)

ggplot(train_non_zero_scaled,aes(x=log_target))+geom_histogram(fill="blue")+ggtitle("Histogram of Normalized Target")

qqnorm(train_non_zero_scaled$log_target,
       datax = TRUE,
       col = "red",
       main = "Normal Q-Q Plot of log_target Distribution")
qqline(train_non_zero_scaled$log_target,
       col = "blue",
       datax = TRUE)

plot(rowSums(train_non_zero_scaled[,4:43]), train_non_zero_scaled$log_target, main="Scatterplot of log_target vs row sums", xlab="Row Sums", ylab="Log of Target", pch=16)

#Binning one of the predictors
N1 <- length(train_non_zero_scaled[,4])
nbins1 <- 5
whichbin1 <- c(rep(0, N1))
freq1 <- N1/nbins1
train_non_zero_scaled <- train_non_zero_scaled[order(train_non_zero_scaled[,4]),]
for (i in 1:nbins1) {
  for (j in 1:N1) {
    if((i-1)*freq1 < j && j <=i*freq1)
      whichbin1[j] <- i
  }
}
whichbin1<-gsub(pattern = "1", replacement = "VLow", whichbin1)
whichbin1<-gsub(pattern = "2", replacement = "Low", whichbin1)
whichbin1<-gsub(pattern = "3", replacement = "Medium", whichbin1)
whichbin1<-gsub(pattern = "4", replacement = "High", whichbin1)
whichbin1<-gsub(pattern = "5", replacement = "VHigh", whichbin1)
train_non_zero_scaled[,4]<-whichbin1


#plot frequencies in the bins
barplot(table(train_non_zero_scaled[,4]))

N2 <- length(test_non_zero_scaled[,2])
nbins2 <- 5
whichbin2 <- c(rep(0, N2))
freq2 <- N2/nbins2
test_non_zero_scaled <- test_non_zero_scaled[order(test_non_zero_scaled[,2]),]
for (i in 1:nbins2) {
  for (j in 1:N2) {
    if((i-1)*freq2 < j && j <=i*freq2)
      whichbin2[j] <- i
  }
}
whichbin2<-gsub(pattern = "1", replacement = "VLow", whichbin2)
whichbin2<-gsub(pattern = "2", replacement = "Low", whichbin2)
whichbin2<-gsub(pattern = "3", replacement = "Medium", whichbin2)
whichbin2<-gsub(pattern = "4", replacement = "High", whichbin2)
whichbin2<-gsub(pattern = "5", replacement = "VHigh", whichbin2)
test_non_zero_scaled[,2]<-whichbin2

#plot frequencies in the bins
barplot(table(test_non_zero_scaled[,2]))

write.csv(train_non_zero_scaled, file = "train_non_zero_scaled.csv",row.names=FALSE)

#Binning Target Variable
Nt <- length(train_non_zero_scaled$log_target)
nbinst <- 5
whichbint <- c(rep(0, Nt))
freqt <- Nt/nbinst
train_non_zero_scaled_sorted <- train_non_zero_scaled[order(train_non_zero_scaled$log_target),]
for (i in 1:nbinst) {
  for (j in 1:Nt) {
    if((i-1)*freqt < j && j <=i*freqt)
      whichbint[j] <- i
  }
}
whichbint1<-gsub(pattern = "1", replacement = "VLow", whichbint)
whichbint2<-gsub(pattern = "2", replacement = "Low", whichbint1)
whichbint3<-gsub(pattern = "3", replacement = "Medium", whichbint2)
whichbint4<-gsub(pattern = "4", replacement = "High", whichbint3)
whichbint5<-gsub(pattern = "5", replacement = "VHigh", whichbint4)
train_non_zero_scaled_sorted$bin_target<-whichbint5
train_non_zero_scaled_sorted<-train_non_zero_scaled_sorted[c(1:3,73,4:72)]

#plot frequencies in the bins
barplot(table(train_non_zero_scaled_sorted$bin_target))

#clustering
##calculate distance matrix (default is Euclidean distance)
distance = dist(train_non_zero_scaled[,5:42])

# Hierarchical agglomerative clustering using default complete linkage 
train.hclust = hclust(distance)
plot(train.hclust)
member = cutree(train.hclust,3)
table(member)

##calculate the same for the test subset
distance2 = dist(test_non_zero_scaled[,3:41])
train.hclust2 = hclust(distance2)
member2 = cutree(train.hclust2,3)
table(member2)

#*use train_non_zero_scaled on log_target*
full.model<-lm(log_target ~., data = train_non_zero_scaled[,3:43])
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

#Linear regression
#*use train_non_zero_scaled on log_target*
full.model<-lm(log_target ~., data = train_non_zero_scaled[,3:72])
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)
#Evaluate using the same training set
train_non_zero_scaled$pred_target_value_reg<-exp(predict(step.model,train_non_zero_scaled))
(rmse1a<-RMSE(train_non_zero_scaled$target, train_non_zero_scaled$pred_target_value_reg))
(rmsle1a<-rmsle(preds = as.numeric(train_non_zero_scaled$pred_target_value_reg), actuals = as.numeric(train_non_zero_scaled$target)))

par(mfrow = c(2, 2))
plot(step.model)

#decision tree
#*use train_non_zero_scaled on log_target*
  tree1<-rpart(log_target ~ ., data=train_non_zero_scaled[,3:72], method="anova", model=TRUE)
tmp<-printcp(tree1)
(rsq.val <- 1-tmp[,3])
set_plot_dimensions(1200, 1200)
prp(tree1)
#*use train_non_zero_scaled_sorted on bin_target*
tree2<-rpart(bin_target ~ ., data=train_non_zero_scaled_sorted[,4:73], method="class",model=TRUE)
printcp(tree2)
prp(tree2)
#evaluating the first tree model (continous) using the same training set
train_non_zero_scaled$pred_target_value_tree1<-exp(predict(tree1, train_non_zero_scaled))
(rmse2a<-RMSE(train_non_zero_scaled$target, train_non_zero_scaled$pred_target_value_tree1))
(rmsle2a<-rmsle(preds = as.numeric(train_non_zero_scaled$pred_target_value_tree1), actuals = as.numeric(train_non_zero_scaled$target)))
#evaluating the second tree model (categorical) using the same training set
train_non_zero_scaled_sorted$pred_target_value_tree2<-predict(tree2, train_non_zero_scaled_sorted, type = "class")
confusionMatrix(train_non_zero_scaled_sorted$pred_target_value_tree2,as.factor(train_non_zero_scaled_sorted$bin_target), dnn = c("Prediction", "True Value"))

#Association rules
#*use train_non_zero_scaled_sorted_ruled on target*
train_non_zero_scaled_sorted_ruled <- as(train_non_zero_scaled_sorted_ruled, "transactions")
target_rules <- apriori(data=train_non_zero_scaled_sorted_ruled, parameter=list (supp=0.035,conf = 0.6, minlen=3, maxlen=5), appearance = list (rhs=c("target=VLow", "target=Low", "target=Medium", "target=High", "target=VHigh")))
inspect(target_rules[1:10])
target_rules<-sort(target_rules, by="confidence", decreasing=TRUE)

set_plot_dimensions(1200, 1200)
plot(target_rules[1:10], measure = "support", method="graph", shading="confidence", cex=0.7)
