rm(list=ls())
NSDUH = load("~/NHOPD/data/NSDUH_2014/DS0001/NSDUH.rda")
data = da36361.0001
rm(da36361.0001)
which(names(data) == "HEREVER")

#Available covariates in synthetic population data
#Age: AGE2
#Sex: IRSEX
#Race: NEWRACE2
#Employed outside the home: don't think this exists.  
#Household income: IRFAMIN3 or INCOME
#Household size: IRHHSIZ2
#Age of oldest member of household: not availab.e  IRHH65_2 gives number of people in household aged 65 or more though.

Heroin = as.character(data[,"HEREVER"])
Vicodin = as.character(data[,"VICOLOR"])
Percocet = as.character(data[,"PERCTYLX"])
Codeine = as.character(data[,"CODEINE"])
Hydrocodone = as.character(data[,"HYDROCOD"])
Methadone = as.character(data[,"METHDON"])
Morphine = as.character(data[,"MORPHINE"])
Oxycontin = as.character(data[,"OXYCONTN"])

Opiates = cbind(Heroin, Vicodin, Percocet, Codeine, Hydrocodone, Methadone, Morphine, Oxycontin)
nOpiates = dim(Opiates)[2]
count(Morphine)
for(j in 1:nOpiates){
  if(j<=3){
    Opiates[Opiates[,j] == "(1) Yes",j] = 1
  }else{
    Opiates[Opiates[,j] == "(1) Response entered",j] = 1
  }
  Opiates[Opiates[,j]!=1,j] = 0  
  Opiates[is.na(Opiates[,j]),j] = 0
}

Opioids = matrix(nrow = dim(Opiates)[1], ncol = nOpiates)
for(j in 1:nOpiates){
  Opioids[,j] = as.numeric(Opiates[,j])
}

apply(Opioids,2,sum)

OpioidCount = apply(Opioids,1,sum)
OpioidEver = OpioidCount
OpioidEver[OpioidCount>0] = 1
data$OPIOIDEVER = OpioidEver

#remove all missing cases of HEREVER



#Need to split up the age groups.
#Less than 18
#18 - 25
#26-34
#35 - 49
#50 - 64
# 65 or older
levels(data$AGE2) = list( LessThan18 = c("(01) Respondent is 12 years old" , "(02) Respondent is 13 years old",
                                         "(03) Respondent is 14 years old","(04) Respondent is 15 years old",
                                         "(05) Respondent is 16 years old", "(06) Respondent is 17 years old"), 
                          x18to25 = c("(07) Respondent is 18 years old", "(08) Respondent is 19 years old",
                                      "(09) Respondent is 20 years old", "(10) Respondent is 21 years old",
                                      "(11) Respondent is 22 or 23 years old", "(12) Respondent is 24 or 25 years old"), 
                          x26to34 = c("(13) Respondent is between 26 and 29 years old", "(14) Respondent is between 30 and 34 years old"),
                          x35to49 = c("(15) Respondent is between 35 and 49 years old"), 
                          x50to64 = c("(16) Respondent is between 50 and 64 years old"), 
                          x65orOlder = c("(17) Respondent is 65 years old or older") )
library(plyr)
data$AGE2 = mapvalues(data$AGE2, from = c("LessThan18", "x18to25", "x26to34", "x35to49", "x50to64", "x65orOlder"), to = c("(1): Less than 18", "(2): 18 to 25", "(3): 26 to 34", "(4): 35 to 49", "(5): 50 to 64", "(6): 65 or older"))





#Demographics codebook
# IMOTHER: mother in household
pct = .5 # percentage of data to train on
n = dim(data)[1]
train_index = sample(1:n, size = round(pct*n),replace=F)
train = data[train_index,]
nTrain = dim(train)[1]
test = data[-train_index,]
nTest = dim(test)[1]
#see how many heroin users are in training data
sum(train$OPIOIDEVER==1,na.rm=T)
sum(test$OPIOIDEVER==1,na.rm=T)


m1 = glm( OPIOIDEVER ~ AGE2 + IRSEX + IRFAMIN3 + NEWRACE2 + IRHHSIZ2, data = train, family = binomial(link = 'logit') )
summary(m1)

library(xtable)
xtable(m1)

anova(m1, test="Chisq")


fitted.results <- predict(m1,newdata=test,type='response')
pdf("~/NHOPD/data/NSDUH_2014/Fitted_Rates.pdf")
hist(fitted.results, xlab = "Probability of heroin use")
dev.off()

thresh = .25
predicted.response = ifelse(fitted.results>=thresh, 1, 0)
misClasificError <- mean(predicted.response != test$HEREVER)
diff = predicted.response - test$OPIOIDEVER

sum(diff==1)/nTest #false positive
sum(diff==-1)/nTest #false negative
sum(diff==0)/nTest #accuracy
# IRSEX: Gender
# NEWRACE2: Race


library(ROCR)
p <- predict(m1, newdata=test, type="response")
pr <- prediction(p, test$OPIOIDEVER)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc.prf = performance(pr, measure = "auc")
auc.prf@y.values


pdf("~/NHOPD/data/NSDUH_2014/ROC.pdf")
plot(prf)
dev.off()


#random forest.  
library(randomForest)

RF_fit <- randomForest(as.factor(OPIOIDEVER) ~ AGE2 + IRSEX + IRFAMIN3 + NEWRACE2 + IRHHSIZ2,
                    data = train,
                    importance=TRUE, 
                    ntree=128)
varImpPlot(RF_fit)
p_RF <- predict(RF_fit, newdata=test, type="prob")
pr_RF <- prediction(p_RF[,2], test$OPIOIDEVER )
prf_RF <- performance(pr_RF, measure = "tpr", x.measure = "fpr")
plot(prf_RF)
auc.prf = performance(pr_RF, measure = "auc")
auc.prf@y.values


#try neural network maybe.



