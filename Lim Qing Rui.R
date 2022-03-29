library(data.table)
library(ggplot2)
library(ggcorrplot)
library(class)
library(dplyr)
library(pdp)
library(vip)
library(reshape)
library(tidyquant)
library(zoo)
library(Metrics)
library(MLmetrics)
library(tidyverse)
library(corrr)
library(randomForest)


setwd("C:/Users/Qing Rui/Desktop/BC2407 Analytics II/AY21S2 BC2407 CBA/AY21S2 BC2407 CBA")
data1 = fread("data01.csv",stringsAsFactors = T)
View(data1)
summary(data1) # for this data, na values will not be removed as NA values will be replaced in subsequent code

##### Question 1 #####

### a)
nrow(data1) # 1177 rows

length(unique(data1$ID)) # 1177 unique rows/ID, so ID does not have to be categorised

library(tidyverse)
library(magrittr) # library to mutate (Change column class type) multiple columns
cols = c("group", "outcome", "gendera", "hypertensive","atrialfibrillation", "CHD with no MI", "diabetes",
          "deficiencyanemias","depression", "Hyperlipemia", "Renal failure", "COPD") #12 variables for factorisation

data1 %<>% mutate_at(cols, factor) # Assignment pipe %<>% to assign columns into dataframe to update category
str(data1) # check data type and 12 columns are factorised


### c) Table of missing value counts
summary(data1)

#Count columns with NA values and extract their names
is.na(data1) # Check if value is missing
misCol = colSums(is.na(data1)) #Count missing values per columns
which(colSums(is.na(data1))>0) #Column number of columns with at least one missing value

misName = names(which(colSums(is.na(data1))>0)) # Retrieved column names that contain missing value
misCol1 = misCol[which(misCol != 0)] #Obtain count of missing value for col names have more than 0 missing values
length(names(which(colSums(is.na(data1))>0))) #20 columns with missing names

df2 = data.frame(Variable.Name=names(data1), Type=sapply(data1,class)) # obtain information from str() information and parse into new dataframe

#Create table to display missing column names
t1 = data.frame('Variable.Name' = 1:20, 'NA.Count' = 1:20)
# Convert all column to string as CI is a confidence interval and not one number.
t1 = data.frame(lapply(t1, as.character))
t1$Variable.Name = NA
t1$NA.Count = NA
t1

t1[,1]= misName #add missing names into column 1
t1[,2]= misCol1 #add NA counts into column 3

t1= merge(df2,t1, by = 'Variable.Name', all=TRUE) # merge data1 and df2 into table to display name, datatype and na count
t1 = na.omit(t1) #remove na values from t1
row.names(t1) = 1:nrow(t1) #re-name row number
colnames(t1)[2]='Data.Type' #Change column name of 2nd column
View(t1)


### d) Explore data1. Produce charts, tables or/and statistics to explain 3 interesting findings.
#Explore trainset variables
trainexplore = subset(data1, data1$group=='1') 
Only_variablesTrain = subset(trainexplore,select= -c(group, ID)) #Filter out columns such that only variables remain

model.matrix(~0+., data=Only_variablesTrain) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2) #Correlation plot of variables used to investigate Heart Failure

#Explore testset variables
testexplore = subset(data1, data1$group=='2') 
Only_variablesTest = subset(testexplore,select= -c(group, ID)) #Filter out columns such that only variables remain

model.matrix(~0+., data=Only_variablesTest) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2) #Correlation plot of variables used to investigate Heart Failure

# Check Age of participants (trainset + testset)
# Create mode function for Age
getMode = function(x){
  test = table(x) 
  if(length(test) > 1){
    test = table(x) %>% sort(decreasing = T) %>% as.data.frame(stringsAsFactors = F) %>%
      mutate(x = as.numeric(x)) %>% filter(Freq == max(Freq))
    test$x
  } else {
    NULL
  }
}
modeAge = getMode(data1$age) #Mode Age is 89
mean(data1$age) #Mean age is 74.06 years old
median(data1$age) #Median Age is 77 year old

death1 = subset(data1, data1$outcome == '1')
death0 = subset(data1, data1$outcome == '0')

# Plot left Skewed graph for Age
ggplot(data1, aes(x= age), show.legend = TRUE) +geom_density() +
  geom_density(data=death1,aes(x=age), color="darkgreen", linetype = "dashed", show.legend = TRUE)+
  geom_density(data=death0,aes(x=age), color="orange", linetype = "dashed", show.legend = TRUE)+
  geom_vline(aes(xintercept=mean(age)),color="blue", linetype="solid", size=1, show.legend = TRUE) +
  geom_vline(aes(xintercept=median(age)),color="red", linetype="solid", size=1, show.legend = TRUE)+ 
  geom_vline(data = as.data.frame(modeAge),aes(xintercept=modeAge, color = "Mode Overall Age"), show.legend = TRUE, size = 1, linetype="solid")+
  scale_color_manual(name="Legend", values = c(`Mean Overall Age`="blue", `Median Overall Age`="red", `Mode Overall Age` = "green", `Dist. for Overall Age`="black",
                                               `Dist. for Heart Failure (Yes) Age` = "darkgreen", `Dist. for Heart Failure (No) Age` = "orange"))+
  theme(legend.position = c(0.3,0.8)) +
  labs(title = "Density plot of Age in data01")


##### Question 2 #####

### a)
data2 = data1
str(data2)

# na.roughfix used in RF package to impute continuous variables by median and categorical variable by mode
data2 = na.roughfix(data2)
summary(data2)

colSums(is.na(data2)) # no missing values in each column for data2
sum(is.na(data2))
View(data2) #Output after imputing respective values into NA values


### b)
trainset = subset(data2, data2$group =='1')
summary(trainset) #Verify that there are 825 individuals in trainset
trainset = subset(trainset,select= -c(group, ID)) # subset to remove group and ID columns
table1 = as.table(table(trainset$outcome))
prop.table(table1) #Proportion of outcomes for alive vs dead

### c)
testset = subset(data2, data2$group =='2')
summary(testset) #Verify that there are 352 individuals in testset
testset = subset(testset,select= -c(group, ID))  # subset to remove group and ID columns
table2 = as.table(table(testset$outcome))
prop.table(table2) #Proportion of outcomes for alive vs dead




##### Question 5 #####
# Create dataframe
t2 = data.frame('Model' = 1:4, 'FPR' = 1:4,'FNR' = 1:4, 'Err' = 1:4)
t2 <- data.frame(lapply(t2, as.character))
t2$FPR = NA
t2$FNR = NA
t2$Err = NA
t2[,1] = c('Logistic Reg (BE)','Random Forest' ,'GWTG','Nomogram')
View(t2)

#Logistic Regression
library(caTools)
set.seed(22)
LR1 = glm(outcome ~ . , family = binomial, data = trainset)
LR1 = step(LR1, direction='backward', scope=formula(LR1), trace=0)
summary(LR1)

threshold1 = 0.5
# Confusion Matrix on Trainset
prob.train = predict(LR1,type = 'response')
LR1.predict.train <- ifelse(prob.train > threshold1, "Yes", "No")
table3 = table(Trainset.Actual = trainset$outcome, LR1.predict.train, deparse.level = 2)
table3
table3.prop = round(prop.table(table3),3)

LR.FPR = 16/(16+693) #FPR = FP/(FP+TN)
t2[1,2] = paste(round(LR.FPR,5))

LR.FNR = 70/(70+46) #FNR = FN/(TP+FN)
t2[1,3] = paste(round(LR.FNR,5))

LR.OE = (16+70)/(693+16+70+46) #Overall Error = (FP+FN)/(FP + FN +TP+TN)
t2[1,4] = paste(round(LR.OE,5))

#Random Forest
set.seed(22)
sum(is.na(trainset))
library(randomForest)
library(caret)
names(trainset) = make.names(names(trainset))
RF1 = randomForest(outcome ~ . , data = trainset, importance = T)
RF1  ## shows defaults are B = 500

var.impt = importance(RF1)
varImpPlot(RF1, type = 1)

RF.FPR = 2/(2+707) #FPR = FP/(FP+TN)
t2[2,2] = paste(round(RF.FPR,5))

RF.FNR = 104/(104+12) #FNR = FN/(TP+FN)
t2[2,3] = paste(round(RF.FNR,5))

RF.OE = (2+104)/(707+2+104+12)  #Overall Error = (FP+FN)/(FP + FN +TP+TN)
t2[2,4] = paste(round(RF.OE,5))


# GWTG Trainset
trainGWTG = trainset
trainGWTG$`Systolic blood pressure score`=cut(trainGWTG$`Systolic.blood.pressure`,
                                              breaks = c(0,59,69,79,89,99,109,119,129,139,149,159,169,179,189,199,
                                                         max(trainGWTG$`Systolic.blood.pressure`)),
                                              labels = c("28","26","24","23","21","19","17","15","13","11","9","8","6","4","2","0"),
                                              include.lowest = TRUE)

trainGWTG$BUN=cut(trainGWTG$Urea.nitrogen, breaks = c(0,9,19,29,39,49,59,69,79,89,99,109,119,129,139,149,
                                                      max(trainGWTG$Urea.nitrogen)),
                  labels = c("0","2","4","6","8","9","11","13","15","17","19","21","23","25","27","28"),
                  include.lowest = TRUE)

trainGWTG$SodiumScore =cut(trainGWTG$Blood.sodium,
                           breaks = c(0,130,131,132,133,134,135,136,137,138,
                                      max(trainGWTG$Blood.sodium)),
                           labels = c("4","3","3","3","2","2","2","1","1","0"),
                           include.lowest = TRUE)

trainGWTG$Age1 = cut(trainGWTG$age, breaks = c(0,19,29,39,49,59,69,79,89,
                                               max(trainGWTG$age)),
                     labels = c("0","3","6","8","11","14","17","19","22"),
                     include.lowest = TRUE)

trainGWTG$HeartRate = cut(trainGWTG$heart.rate, breaks = c(0,79,84,89,94,99,104,
                                                           max(trainGWTG$heart.rate)),
                          labels = c("0","1","3","4","5","6","8"),
                          include.lowest = TRUE)

trainGWTG$COPDScore = ifelse(trainGWTG$COPD =="1", 2,0)


trainGWTG$ttlscore = as.numeric(as.character(trainGWTG$Age1)) + as.numeric(as.character(trainGWTG$BUN))+
  as.numeric(as.character(trainGWTG$`Systolic blood pressure score`)) + as.numeric(as.character(trainGWTG$SodiumScore)) +
  as.numeric(as.character(trainGWTG$HeartRate)) + as.numeric(as.character(trainGWTG$COPDScore)) + 3

trainGWTG$outcome2 = factor(ifelse(trainGWTG$ttlscore >= 79, 1,0))
class(trainGWTG$outcome2)
levels(trainGWTG$outcome2)
summary(trainGWTG)


GWTG_conf_matrix=confusionMatrix(data= as.factor(trainGWTG$outcome2), trainGWTG$outcome, positive = "1")
GWTG_conf_matrix

GWTG.FPR = 0/(0+709) #FPR = FP/(FP+TN)
t2[3,2] = paste(round(GWTG.FPR,5))

GWTG.FNR = 116/(116+0) #FNR = FN/(TP+FN)
t2[3,3] = paste(round(GWTG.FNR,5))

GWTG.OE = (116)/(116+709)  #Overall Error = (FP+FN)/(FP + FN +TP+TN) or 1 - Accuracy
t2[3,4] = paste(round(GWTG.OE,5))


# Nomogram

trainNOMO = trainset
trainNOMO$outcome1 = (4.62536+0.24559*trainNOMO$Anion.gap +0.61542*(trainNOMO$Lactic.acid)- 1.04993*(trainNOMO$Blood.calcium)+
  0.02687*(trainNOMO$Urea.nitrogen)-1.76330*as.numeric(as.character(trainNOMO$Renal.failure))-0.05633*(trainNOMO$Diastolic.blood.pressure))

trainNOMO$prob1 = exp(trainNOMO$outcome1)/(1+exp(trainNOMO$outcome1))
summary(trainNOMO)

trainNOMO$outcome2 = factor(ifelse(trainNOMO$prob1 > 0.5, 1,0))

NOMO_conf_matrix=confusionMatrix(data= as.factor(trainNOMO$outcome2), trainNOMO$outcome, positive = "1")
NOMO_conf_matrix

NOMO.FPR = 7/(702+7) #FPR = FP/(FP+TN)
t2[4,2] = paste(round(NOMO.FPR,5))

NOMO.FNR = 86/(86+30) #FNR = FN/(TP+FN)
t2[4,3] = paste(round(NOMO.FNR,5))

NOMO.OE = (7+86)/(7+86+702+30)  #Overall Error = (FP+FN)/(FP + FN +TP+TN) or 1 - Accuracy
t2[4,4] = paste(round(NOMO.OE,5))



##### Question 6 #####
# Create dataframe
t3 = data.frame('Model' = 1:4, 'FPR' = 1:4,'FNR' = 1:4, 'Err' = 1:4)
t3 <- data.frame(lapply(t3, as.character))
t3$FPR = NA
t3$FNR = NA
t3$Err = NA
t3[,1] = c('Logistic Reg (BE)','Random Forest' ,'GWTG','Nomogram')
View(t3)

#Logistic Regression
library(caTools)
set.seed(22)
LR2 = glm(outcome ~ . , family = binomial, data = trainset)
LR2 = step(LR2, direction='backward', scope=formula(LR2), trace=0)
summary(LR2)

threshold1 = 0.5
# Confusion Matrix on Testset
prob.test= predict(LR2, newdata = testset, type = 'response')
LR2.predict.test <- ifelse(prob.test > threshold1, 1 , 0)
table5 = table(Testset.Actual = testset$outcome, LR2.predict.test, deparse.level = 2)
table5
table5.prop = round(prop.table(table5),3)


LR2.FPR = 12/(12+297) #FPR = FP/(FP+TN)
t3[1,2] = paste(round(LR2.FPR,5))

LR2.FNR = 29/(29+14) #FNR = FN/(TP+FN)
t3[1,3] = paste(round(LR2.FNR,5))

LR2.OE = (12+29)/(12+29+14+297) #Overall Error = (FP+FN)/(FP + FN +TP+TN)
t3[1,4] = paste(round(LR2.OE,5))

#Random Forest
set.seed(22)
sum(is.na(testset))
library(randomForest)
library(caret)
names(testset) = make.names(names(testset))
RF2 = randomForest(outcome ~ . , data = trainset, importance = T)
RF2  ## shows defaults are B = 500

var.impt = importance(RF2)
varImpPlot(RF2, type = 1)

prob.test.RF2 = predict(RF2, newdata = testset, type = 'response')
RF2.table = table(Testset.Actual = testset$outcome, prob.test.RF2, deparse.level = 2)
RF2.table
RF2.table.prop = round(prop.table(RF2.table),3)

RF2.FPR = 0/(0+309) #FPR = FP/(FP+TN)
t3[2,2] = paste(round(RF2.FPR,5))

RF2.FNR = 39/(39+4) #FNR = FN/(TP+FN)
t3[2,3] = paste(round(RF2.FNR,5))

RF2.OE = (39+0)/(39+0+4+309)  #Overall Error = (FP+FN)/(FP + FN +TP+TN)
t3[2,4] = paste(round(RF2.OE,5))


# GWTG testset
testGWTG = testset
testGWTG$`Systolic blood pressure score`=cut(testGWTG$`Systolic.blood.pressure`,
                                             breaks = c(0,59,69,79,89,99,109,119,129,139,149,159,169,179,189,199,
                                                        max(testGWTG$`Systolic.blood.pressure`)),
                                             labels = c("28","26","24","23","21","19","17","15","13","11","9","8","6","4","2","0"),
                                             include.lowest = TRUE)

testGWTG$BUN=cut(testGWTG$Urea.nitrogen, breaks = c(0,9,19,29,39,49,59,69,79,89,99,109,119,129,139,149,
                                                    max(testGWTG$Urea.nitrogen)),
                 labels = c("0","2","4","6","8","9","11","13","15","17","19","21","23","25","27","28"),
                 include.lowest = TRUE)

testGWTG$SodiumScore =cut(testGWTG$Blood.sodium,
                          breaks = c(0,130,131,132,133,134,135,136,137,138,
                                     max(testGWTG$Blood.sodium)),
                          labels = c("4","3","3","3","2","2","2","1","1","0"),
                          include.lowest = TRUE)

testGWTG$Age1 = cut(testGWTG$age, breaks = c(0,19,29,39,49,59,69,79,89,
                                             max(testGWTG$age)),
                    labels = c("0","3","6","8","11","14","17","19","22"),
                    include.lowest = TRUE)

testGWTG$HeartRate = cut(testGWTG$heart.rate, breaks = c(0,79,84,89,94,99,104,
                                                         max(testGWTG$heart.rate)),
                         labels = c("0","1","3","4","5","6","8"),
                         include.lowest = TRUE)

testGWTG$COPDScore = ifelse(testGWTG$COPD =="1", 2,0)


testGWTG$ttlscore = as.numeric(as.character(testGWTG$Age1)) + as.numeric(as.character(testGWTG$BUN))+
  as.numeric(as.character(testGWTG$`Systolic blood pressure score`)) + as.numeric(as.character(testGWTG$SodiumScore)) +
  as.numeric(as.character(testGWTG$HeartRate)) + as.numeric(as.character(testGWTG$COPDScore)) + 3

testGWTG$outcome2 = factor(ifelse(testGWTG$ttlscore >= 79, 1,0))
class(testGWTG$outcome2)
levels(testGWTG$outcome2)
summary(testGWTG)


GWTG_conf_matrixTest=confusionMatrix(data= as.factor(testGWTG$outcome2), testGWTG$outcome, positive = "1")
GWTG_conf_matrixTest

GWTGTest.FPR = 0/(0+309) #FPR = FP/(FP+TN)
t3[3,2] = paste(round(GWTGTest.FPR,5))

GWTGTest.FNR = 43/(43+0) #FNR = FN/(TP+FN)
t3[3,3] = paste(round(GWTGTest.FNR,5))

GWTGTest.OE = (43)/(43+309)  #Overall Error = (FP+FN)/(FP + FN +TP+TN) or 1 - Accuracy
t3[3,4] = paste(round(GWTGTest.OE,5))


# Nomogram

testNOMO = testset
testNOMO$outcome1 = (4.62536+0.24559*testNOMO$Anion.gap +0.61542*(testNOMO$Lactic.acid)- 1.04993*(testNOMO$Blood.calcium)+
                           0.02687*(testNOMO$Urea.nitrogen)-1.76330*as.numeric(as.character(testNOMO$Renal.failure))-0.05633*(testNOMO$Diastolic.blood.pressure))

summary(testNOMO)

testNOMO$prob1 = exp(testNOMO$outcome1)/(1+exp(testNOMO$outcome1))
summary(trainNOMO)
testNOMO$outcome2 = factor(ifelse(testNOMO$prob1 > 0.5, 1,0))

NOMO_conf_matrix=confusionMatrix(data= as.factor(testNOMO$outcome2), testNOMO$outcome, positive = "1")
NOMO_conf_matrix

NOMOTest.FPR = 5/(5+304) #FPR = FP/(FP+TN)
t3[4,2] = paste(round(NOMOTest.FPR,5))

NOMOTest.FNR = 32/(32+11) #FNR = FN/(TP+FN)
t3[4,3] = paste(round(NOMOTest.FNR,5))

NOMOTest.OE = (32+5)/(32+5+11+304)  #Overall Error = (FP+FN)/(FP + FN +TP+TN) or 1 - Accuracy
t3[4,4] = paste(round(NOMOTest.OE,5))




##### Question 7 ######

# Random sample from majority class Default = No and combine with Default = 
#Yes to form new trainset ----- 
set.seed(22)
majority = trainset[outcome == "0"]
set.seed(22)
minority = trainset[outcome == "1"] 

# Randomly sample the row numbers to be in trainset. Same sample size as  minority cases.
set.seed(22)
chosen = sample(seq(1:nrow(majority)), size = nrow(minority)) 

# Subset the original trainset based on randomly chosen row numbers. 
majority.chosen = majority[chosen] 

# Combine two data tables by appending the rows 
trainset.bal = rbind(majority.chosen, minority) 
summary(trainset.bal) 
## Check trainset is balanced. #outcome = 0 and 1 are 116 each
summary(testset)


# Create dataframe
t4 = data.frame('Model' = 1:2, 'FPR' = 1:2,'FNR' = 1:2, 'Err' = 1:2)
t4 <- data.frame(lapply(t4, as.character))
t4$FPR = NA
t4$FNR = NA
t4$Err = NA
t4[,1] = c('Logistic Reg (BE)','Random Forest')
View(t4)

#Logistic Regression
library(caTools)
set.seed(22)
LR3 = glm(outcome ~ . , family = binomial, data = trainset.bal)
LR3 = step(LR3, direction='backward', scope=formula(LR3), trace=0)
summary(LR3)

threshold1 = 0.5
# Confusion Matrix on Testset
prob.test= predict(LR3, newdata = testset, type = 'response')
LR3.predict.test <- ifelse(prob.test > threshold1, 1 , 0)
table7 = table(Testset.Actual = testset$outcome, LR3.predict.test, deparse.level = 2)
table7
table7.prop = round(prop.table(table7),3)


LR3.FPR = 94/(94+215) #FPR = FP/(FP+TN)
t4[1,2] = paste(round(LR3.FPR,5))

LR3.FNR = 14/(29+14) #FNR = FN/(TP+FN)
t4[1,3] = paste(round(LR3.FNR,5))

LR3.OE = (94+14)/(215+29+94+14) #Overall Error = (FP+FN)/(FP + FN +TP+TN)
t4[1,4] = paste(round(LR3.OE,5))

#Random Forest
set.seed(22)
sum(is.na(testset))
library(randomForest)
library(caret)
names(testset) = make.names(names(testset))
RF3 = randomForest(outcome ~ . , data = trainset.bal, importance = T)
RF3  ## shows defaults are B = 500

var.impt = importance(RF3)
varImpPlot(RF3, type = 1)

prob.test.RF3 = predict(RF3, newdata = testset, type = 'response')
RF3.table = table(Testset.Actual = testset$outcome, prob.test.RF3, deparse.level = 2)
RF3.table
RF3.table.prop = round(prop.table(RF3.table),3)

RF3.FPR = 89/(89+220) #FPR = FP/(FP+TN)
t4[2,2] = paste(round(RF3.FPR,5))

RF3.FNR = 10/(10+33) #FNR = FN/(TP+FN)
t4[2,3] = paste(round(RF3.FNR,5))

RF3.OE = (89+10)/(89+10+220+33)  #Overall Error = (FP+FN)/(FP + FN +TP+TN)
t4[2,4] = paste(round(RF3.OE,5))






##### Question 8 ######
t5 = t4
t5[3,]=c("RF VarImpt into Logistic Reg", NA, NA, NA) #Create new row in t5

#Random Forest
set.seed(22)
sum(is.na(testset))
library(randomForest)
library(caret)
names(testset) = make.names(names(testset))
RF3 = randomForest(outcome ~ . , data = trainset.bal, importance = T)
RF3  ## shows defaults are B = 500

var.impt = importance(RF3)

varImpPlot(RF3, type = 1, n.var = 20, main = "Top 20 Predicted Variables by RF") # n.var obtains top 20 variables
?varImpPlot
# RF Variables into Log Reg
trainset.balLR = trainset.bal[,c("Blood.calcium","outcome","Bicarbonate","Lymphocyte",
                                 "heart.rate","Anion.gap","Leucocyte","Respiratory.rate",
                                 "Creatine.kinase","Lactic.acid","Basophils","RDW","BMI",
                                 "Blood.sodium","Neutrophils","NT.proBNP","age", "Platelets",
                                 "INR","MCV","glucose")]
View(trainset.balLR)
summary(trainset.balLR)
length(trainset.balLR) #21 columns

library(caTools)
set.seed(22)
LR4 = glm(outcome ~ . , family = binomial, data = trainset.balLR)
# LR4 = step(LR4, direction='backward', scope=formula(LR4), trace=0)
summary(LR4)

threshold1 = 0.5
# Confusion Matrix on Testset
prob.test= predict(LR4, newdata = testset, type = 'response')
LR4.predict.test <- ifelse(prob.test > threshold1, 1 , 0)
table8 = table(Testset.Actual = testset$outcome, LR4.predict.test, deparse.level = 2)
table8
table8.prop = round(prop.table(table8),3)


LR4.FPR = 100/(100+209) #FPR = FP/(FP+TN)
t5[3,2] = paste(round(LR4.FPR,5))

LR4.FNR = 17/(17+26) #FNR = FN/(TP+FN)
t5[3,3] = paste(round(LR4.FNR,5))

LR4.OE = (17+100)/(17+100+26+209) #Overall Error = (FP+FN)/(FP + FN +TP+TN)
t5[3,4] = paste(round(LR4.OE,5))
View(t5)






