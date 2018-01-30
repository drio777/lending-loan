setwd('C:/Users/user/Desktop/lending')
dat = read.csv('ourLendingFinal.csv')
rawDat = read.csv('ourLendingFinalRaw.csv')
lec = read.csv('lending club loan2.csv',header=T)

####Start Modeling####
# install.packages('randomForest')
library("randomForest")
# install.packages('mgcv')
library('mgcv')
# install.packages('caret')
library('caret')
# install.packages('e1071')
library('e1071')
# install.packages('nnet')
library('nnet')


###jeonchoiri

#1. acc_now_delinq/open_acc = delinqOpen_ratio

dat$delinqOpen_ratio = dat$acc_now_delinq/dat$open_acc

sum(dat$acc_now_delinq==0)
sum(rawDat$acc_now_delinq==0)
sum(dat$sangWon_ratio==0)

## NaN because there is 0/0

dat[is.na(dat$delinqOpen_ratio),colnames(dat)=='delinqOpen_ratio'] = 0 # change it to 0

#2. total_acc-open_acc = close_acc
dat$close_acc = dat$total_acc - dat$open_acc

#3. make pub_rec to categorical
dat$pub_rec2[dat$pub_rec>0] = T
dat$pub_rec2[dat$pub_rec==0] = F

levels(factor(dat$pub_rec))

#4.  
sum(dat$emp_length=='n/a')


###make dat for modeling
x = c('late', 'term', 'int_rate', 'grade', 'emp_length', 'home_ownership', 'annual_inc', 'desc',
      'purpose', 'dti', 'delinq_2yrs', 'inq_last_6mths', 'open_acc', 'pub_rec2', 'close_acc', 'installment')

idx = NULL
for (i in colnames(dat)) idx = c(idx, i %in% x)
datM = dat[,idx]


#1. factorize
x = c('int_rate', 'annual_inc', 'dti', 'delinq_2yrs', 'inq_last_6mths', 'open_acc', 'close_acc', 'installment',
      'inq_last_6mths', 'pub_rec2') #numeric

idx = NULL; for (i in colnames(datM)) idx = c(idx, i %in% x)
for (i in 1:sum(!idx)) datM[,!idx][,i] = factor(datM[,!idx][,i])
for (i in 1:sum(idx)) datM[,idx][,i] = as.numeric(datM[,idx][,i])
#logical is logical
x = c('late', 'pub_rec2', 'desc')
idx = NULL; for (i in colnames(datM)) idx = c(idx, i %in% x)
for (i in 1:sum(idx)) datM[,idx][,i] = as.logical(datM[,idx][,i])

###Changing variable

#2. revol_bal
good = data.frame(id = lec$id, revol_bal = lec$revol_bal, initial_list_status=lec$initial_list_status)
dat = merge(dat,good, by = 'id', all.x = T )

#2. ###make dat for modeling
x = c('late', 'term', 'int_rate', 'grade', 'emp_length', 'home_ownership', 'annual_inc', 'desc',
      'purpose', 'dti', 'delinq_2yrs', 'inq_last_6mths', 'open_acc', 'pub_rec2', 'close_acc', 'installment', 'revol_bal',
      "initial_list_status")

idx = NULL
for (i in colnames(dat)) idx = c(idx, i %in% x)
datM = dat[,idx]

#2. ###1. factorize
x = c('int_rate', 'annual_inc', 'dti', 'delinq_2yrs', 'inq_last_6mths', 'open_acc', 'close_acc', 'installment',
      'inq_last_6mths', 'pub_rec2', 'revol_bal') #numeric

idx = NULL; for (i in colnames(datM)) idx = c(idx, i %in% x)
for (i in 1:sum(!idx)) datM[,!idx][,i] = factor(datM[,!idx][,i])
for (i in 1:sum(idx)) datM[,idx][,i] = as.numeric(datM[,idx][,i])
#logical is logical
x = c('late', 'pub_rec2', 'desc')
idx = NULL; for (i in colnames(datM)) idx = c(idx, i %in% x)
for (i in 1:sum(idx)) datM[,idx][,i] = as.logical(datM[,idx][,i])

###Adding variable
#1. verification_status & addr_state
levels(lec$addr_state)=c(levels(lec$addr_state),"West","MidWest","South",'NorthEast')
levels(lec$addr_state)
lecNew2 = lec
West= lecNew2$addr_state=='WA'|lecNew2$addr_state=='OR'|lecNew2$addr_state=='ID'|lecNew2$addr_state=='MT'|lecNew2$addr_state=='WY'|lecNew2$addr_state=='CO'|lecNew2$addr_state=='UT'|lecNew2$addr_state=='NV'|lecNew2$addr_state=='CA'|lecNew2$addr_state=='AZ'|lecNew2$addr_state=='NM'|lecNew2$addr_state=='HI'|lecNew2$addr_state=='AK' 
MidWest= lecNew2$addr_state=='ND'|lecNew2$addr_state=='SD'|lecNew2$addr_state=='NE'|lecNew2$addr_state=='KS'|lecNew2$addr_state=='MN'|lecNew2$addr_state=='IA'|lecNew2$addr_state=='MO'|lecNew2$addr_state=='WI'|lecNew2$addr_state=='IL'|lecNew2$addr_state=='IN'|lecNew2$addr_state=='OH'|lecNew2$addr_state=='MI'
South= lecNew2$addr_state=='TX'|lecNew2$addr_state=='OK'|lecNew2$addr_state=='AR'|lecNew2$addr_state=='LA'|lecNew2$addr_state=='MS'|lecNew2$addr_state=='TN'|lecNew2$addr_state=='AL'|lecNew2$addr_state=='KY'|lecNew2$addr_state=='WV'|lecNew2$addr_state=='MD'|lecNew2$addr_state=='DE'|lecNew2$addr_state=='VA'|lecNew2$addr_state=='NC'|lecNew2$addr_state=='SC'|lecNew2$addr_state=='GA'|lecNew2$addr_state=='FL'|lecNew2$addr_state=='DC'
NorthEast= lecNew2$addr_state=='PA'|lecNew2$addr_state=='NJ'|lecNew2$addr_state=='NY'|lecNew2$addr_state=='CT'|lecNew2$addr_state=='RI'|lecNew2$addr_state=='MA'|lecNew2$addr_state=='NH'|lecNew2$addr_state=='VT'|lecNew2$addr_state=='ME'
lecNew2$addr_state[West]='West'
lecNew2$addr_state[MidWest]='MidWest'
lecNew2$addr_state[South]='South'
lecNew2$addr_state[NorthEast]='NorthEast'
lecNew2$addr_state=factor(lecNew2$addr_state)
levels(lecNew2$addr_state)
tapply(lecNew2$id,factor(lecNew2$addr_state),length)

lec = lecNew2
good = data.frame(id=lec$id,verification_status=lec$verification_status,addr_state=lec$addr_state)
dat = merge(dat,good,by='id',all.x=T)

#2. ###make dat for modeling
x = c('late', 'term', 'int_rate', 'grade', 'emp_length', 'home_ownership', 'annual_inc', 'desc',
      'purpose', 'dti', 'delinq_2yrs', 'inq_last_6mths', 'open_acc', 'pub_rec2', 'close_acc', 'installment', 'revol_bal',
      "initial_list_status", "verification_status", "addr_state")

idx = NULL
for (i in colnames(dat)) idx = c(idx, i %in% x)
datM = dat[,idx]

#1. Purpose
levels(datM$purpose) = c(levels(datM$purpose), "other1", "other2", "other3")
other1 = datM$purpose=="renewable_energy"|datM$purpose=="car"|datM$purpose=="home_improvement"|
  datM$purpose=="moving"
other2 = datM$purpose=="medical"|datM$purpose=="wedding"|datM$purpose=="vacation"|datM$purpose=="major_purchase"
other3 = datM$purpose=="other"|datM$purpose=="small_business"
levels(datM$purpose)
datM$purpose[other1]="other1"; datM$purpose[other2]="other2"; datM$purpose[other3]="other3"
datM$purpose=factor(datM$purpose)
levels(datM$purpose)

#2. ###1. factorize
x = c('int_rate', 'annual_inc', 'dti', 'delinq_2yrs', 'inq_last_6mths', 'open_acc', 'close_acc', 'installment',
      'inq_last_6mths', 'pub_rec2', 'revol_bal') #numeric

idx = NULL; for (i in colnames(datM)) idx = c(idx, i %in% x)
for (i in 1:sum(!idx)) datM[,!idx][,i] = factor(datM[,!idx][,i])
for (i in 1:sum(idx)) datM[,idx][,i] = as.numeric(datM[,idx][,i])
#logical is logical
x = c('late', 'pub_rec2', 'desc')
idx = NULL; for (i in colnames(datM)) idx = c(idx, i %in% x)
for (i in 1:sum(idx)) datM[,idx][,i] = as.logical(datM[,idx][,i])

#
#
##
### plz make the datM first! start modeling
##
#
#

rm(i,idx,MidWest,NorthEast,other1,other2,other3,South,West,x,dat,good,lec,lecNew2,rawDat)



#0. devide train/test
index = sample(1:nrow(datM),0.7*nrow(datM))
datMTrain = datM[index,]
datMTrain$late = factor(datMTrain$late)
datMTrainDown=downSample(datMTrain,datMTrain$late)
datMTrainDown$Class=NULL
datMTest = datM[-index,]

#1. random forest # you should factorize response variable 'late'

datM$late = factor(datM$late)

datRF = randomForest(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
                     + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + sangWon_ratio,
                     data = datMTrain, ntree=1000,mtry=4) #hozoongs where is ntree choosing function
summary(datRF)
plot(datRF)
importance(datRF)
varImpPlot(datRF)
print(datRF)
datRF$importance
datRF_pred = predict(datRF, newdata = datMTest)
plot(datRF_pred)
confusionMatrix(data = datRF_pred, reference = datMTest$late)

##1. 1. without sangWon_ratio
datRF = randomForest(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
                     + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2,
                     data = datMTrain, ntree=300,mtry=4)

##1. 2. with addr_state, verification_status withour initial
datRF = randomForest(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
                     + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc + revol_bal 
                     + addr_state + verification_status,
                     data = datMTrainDown, ntree=300,mtry=4)
varImpPlot(datRF)
datRF_pred = predict(datRF, newdata = datMTest)
confusionMatrix(data = datRF_pred, reference = datMTest$late)
recall(data = datRF_pred, reference = datMTest$late)

# 2. GAM
#installment, loan_amt, title, id, ot_prncp
##
index = sample(1:nrow(datM),0.7*nrow(datM))
datMTrain = datM[index,]
datM$late = factor(datM$late)
datMTrainDown=downSample(datMTrain,datMTrain$late)
datMTrainDown$Class=NULL
datMTest = datM[-index,]

datGAM = gam(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + sangWon_ratio, 
             data = datMTrain)
datGAM_pred = predict.gam(datGAM, newdata = datMTest, type = 'response')
plot(datGAM_pred)
summary(datGAM)

datGAM_pred2 = ifelse(datGAM_pred>=0.5, T, F)
confusionMatrix(data=datGAM_pred2,reference = datMTest$late)

##2. 2. without sangWon_ratio
datGAM = gam(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2, 
             data = datMTrain)

##2. 3. with installment, close_acc
datGAM = gam(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc, 
             data = datMTrain)

##2. 4. with revol_bal
datGAM = gam(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc + revol_bal, 
             data = datMTrain)

##2. 5. with addr_state, verification_status withour initial
datGAM = gam(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc + revol_bal 
             + addr_state + verification_status,
             data = datMTrain)

datGAM_pred = predict.gam(datGAM, newdata = datMTest, type = 'response')
plot(datGAM_pred)
summary(datGAM)

datGAM_pred2 = ifelse(datGAM_pred>=0.49, T, F)
confusionMatrix(data=datGAM_pred2,reference = datMTest$late)

## 3. GLM

datM$late = as.logical(datM$late)
index = sample(1:nrow(datM),0.7*nrow(datM))
datMTrain = datM[index,]
datMTest = datM[-index,]

datGLM = glm(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc, 
             data = datMTrain, family = 'binomial')

datGLM_pred = predict(datGLM, newdata = datMTest, type = 'response')
#plot(datGLM_pred)
summary(datGLM)

datGLM_pred2 = ifelse(datGLM_pred>=0.51, T, F)
confusionMatrix(data=datGLM_pred2,reference = datMTest$late)

##3. 1. with revol_bal
datGLM = glm(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc + revol_bal, 
             data = datMTrain, family = 'binomial')

##3. 2. with initial_status
datGLM = glm(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc + revol_bal + 
               initial_list_status, 
             data = datMTrain, family = 'binomial')

##3. 3. with addr_state, verification_status withour initial
datGLM = glm(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
             + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc + revol_bal 
             + addr_state + verification_status,
             data = datMTrain, family = 'binomial')

datGLM_pred = predict(datGLM, newdata = datMTest, type = 'response')
plot(datGLM_pred)
summary(datGLM)

datGLM_pred2 = ifelse(datGLM_pred>=0.51, T, F)
confusionMatrix(data=datGLM_pred2,reference = datMTest$late)

#4. neural

datM$late = as.logical(datM$late)

##1. scaling
x = c('int_rate', 'annual_inc', 'dti', 'delinq_2yrs', 'inq_last_6mths', 'open_acc', 'close_acc', 'installment',
      'inq_last_6mths', 'pub_rec2', 'revol_bal') #numeric
idx = NULL; for (i in colnames(datM)) idx = c(idx, i %in% x)
for (i in 1:sum(idx)) datM[,idx][,i] = scale(datM[,idx][,i])


##neural (multi = neuralnet)
library('nnet')
str(datMTrain)

datNeural = nnet(late ~ term + int_rate + grade + emp_length + home_ownership + annual_inc + desc
                 + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 + installment + close_acc + revol_bal + 
                   initial_list_status, data=datMTrain, size=4, decay= 0.023, maxit=1000)


datNeural_pred = predict(datNeural, datMTest, type = 'raw')
par(mfrow=c(1,1)); plot(datNeural_pred)
datNeural_pred2 = ifelse(datNeural_pred>=0.48, T, F)
confusionMatrix(datNeural_pred2,reference = datMTest$late)