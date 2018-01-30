# read the data
install.packages("VIM")

setwd('C:/Users/user/Desktop/lending')
lecorig = read.csv('lending club loan2.csv',header=T)
lec = read.csv('lending club loan2.csv',header=T)
rm(loan,loanSample)

tapply(lecorig$id,factor(lecorig$open_acc_6m),length)
tapply(lecorig$id,factor(lecorig$max_bal_bc),length)



plot(lec$loan_status)

# deleting variables

##1. is there a different between id & mid?

length(levels(factor(lec$id))) = length(levels(factor(lec$member_id)))

###same therefore erase one

lec = lec[,-2]

##2. loan_amnt - funded_amnt

sum(lec[,2]-lec[,3]!=0)

###same therefore erase one

lec = lec[,-3]

##3. loan_amnt - funded_amnt_inv

sum(lec$loan_amnt - lec$funded_amnt_inv) #2487800

###cannot say anything

##4. erase the cols that have a lot na (gt50%)

lecNa = apply(is.na(lec),2,sum)/nrow(lec)
sum(lecNa<=0.5) #54
lec = lec[,lecNa<=0.5]
lecNa = NULL
lec$open_acc_6m=lecorig$open_acc_6m
##5. erase the data that cannot be used #url, policy code #row 10240

#colnames(lec) != 'url'
lec = lec[,colnames(lec) != 'url']

################for the easier work save the csv

# ?write.csv

# write.csv(lec, file = "lending loan.csv")

#deleting var again################3

##delete the 'current'

idx = lec$loan_status!='Current' #sum(idx) #55790
lecNew = lec[idx,]

##make it two fator
#sum(lecNew$loan_status=='Fully Paid')#37850
idx = lecNew$loan_status=='Fully Paid'
lecNew$late[idx] = T
lecNew$late[!idx] = F #head(lecNew$late)


tapply(lecNew$id,factor(lecNew$verification_status),length)
sum(is.na(lecNew$verification_status))

tapply(lecNew$id,factor(lecNew$addr_state),length)

sum(is.na(lecNew$pymnt_plan))

##only var we select

lecNew2 = data.frame(id=lecNew$id,loan_amnt=lecNew$loan_amnt, term=lecNew$term, int_rate=lecNew$int_rate,
                     installment=lecNew$installment, grade=lecNew$grade,
                     emp_length=lecNew$emp_length, home_ownership = lecNew$home_ownership,
                     annual_inc = lecNew$annual_inc, desc=lecNew$desc, purpose=lecNew$purpose,
                     title=lecNew$title, dti=lecNew$dti,delinq_2yrs=lecNew$delinq_2yrs,
                     inq_last_6mths=lecNew$inq_last_6mths,
                     mths_since_last_delinq=lecNew$mths_since_last_delinq,
                     open_acc=lecNew$open_acc, pub_rec=lecNew$pub_rec, total_acc=lecNew$total_acc,
                     out_prncp=lecNew$out_prncp, application_type=lecNew$application_type,
                     acc_now_delinq=lecNew$acc_now_delinq, late=lecNew$late, addr_state=lecNew$addr_state, open_acc_6m=lecNew$open_acc_6m)

library(dplyr)
tapply(lecNew2$id,factor(lecNew2$open_acc_6m),length)
sum(is.na(lecNew2$open_acc_6m))/nrow(lecNew2)


lecNewLate=filter(lecNew2,lecNew2$late==TRUE)
sum(is.na(lecNewLate$open_acc_6m))/nrow(lecNewLate)
lecNewNLate=filter(lecNew2,lecNew2$late==FALSE)
nrow(lecNewNLate)
sum(is.na(lecNewNLate$open_acc_6m))
sum(is.na(lecNewNLate$open_acc_6m))/nrow(lecNewNLate)
sum(lecNewLate$late==TRUE)

#desc
lecNew2$desc = as.character(lecNew2$desc) 
idx1 = lecNew2$desc=="" #sum(idx1)
idx2 = lecNew2$desc==" " #sum(idx2)
idx = idx1|idx2           #sum(idx)

lecNew2$desc[idx] = F
lecNew2$desc[!(idx)] = T

lecNew2$desc=as.factor(lecNew2$desc)
class(lecNew2$desc)
#원금잔액/상환금액 ratio
lecNew2$sangWon_ratio = lecNew2$out_prncp/lecNew2$loan_amnt

#delinq/open account
lecNew2$delinqOpen_ratio= lecNew2$acc_now_delinq/lecNew2$open_acc
lecNew2[is.na(lecNew2$delinqOpen_ratio),colnames(lecNew2)=='delinqOpen_ratio'] = 0

#deleting joint

delJoint=c(!(lecNew2$application_type=="JOINT"))

lecNew2=lecNew2[delJoint,]

rm(delJoint)

#pub rec TF
lecNew2$pub_rec2[lecNew2$pub_rec>0] = T
lecNew2$pub_rec2[lecNew2$pub_rec==0] = F


#missing visualization
# library(VIM)
# mice_plot <- aggr(lec, col=c('navyblue','yellow'),
#                   numbers=TRUE, sortVars=TRUE, cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))

#length
tapply(lecNew2$id,factor(lecNew2$late),length)
tapply(lecNew2$id,factor(lecNew2$term),length)
tapply(lecNew2$id,factor(lecNew2$grade),length)
tapply(lecNew2$id,factor(lecNew2$emp_length),length)#na가 n/a로 되어있음
tapply(lecNew2$id,factor(lecNew2$home_ownership),length)
tapply(lecNew2$id,factor(lecNew2$mths_since_last_delinq),length)
tapply(lecNew2$id,factor(lecNew2$desc),length)
tapply(lecNew2$id,factor(lecNew2$purpose),length)
tapply(lecNew2$id,factor(lecNew2$title),length)#pupose와 유사, 더 세부적임
tapply(lecNew2$id,factor(lecNew2$inq_last_6mths),length)
tapply(lecNew2$id,factor(lecNew2$grade),length)
tapply(lecNew2$id,factor(lecNew2$pub_rec2),length)
tapply(lecNew2$id,factor(lecNew2$title),length)
tapply(lecNew2$id,factor(lecNew2$inq_last_6mths),length)

#more 데이터 전처리
levels(lecNew2$addr_state)=c(levels(lecNew2$addr_state),"West","MidWest","South",'NorthEast')
levels(lecNew2$addr_state)
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

#output our modeling dataset
write.csv(lecNew2, file = 'ourLending.csv')


#let's do random forest This week

#factorizing
lecNew2 = as.data.frame(apply(lecNew2,2,as.factor))
lecNew2$int_rate=as.numeric(lecNew2$int_rate)
lecNew2$annual_inc=as.numeric(lecNew2$annual_inc)
lecNew2$dti=as.numeric(lecNew2$dti)
lecNew2$delinq_2yrs=as.numeric(lecNew2$delinq_2yrs)
lecNew2$inq_last_6mths=as.numeric(lecNew2$inq_last_6mths)
lecNew2$open_acc=as.numeric(lecNew2$open_acc)
lecNew2$sangWon_ratio=as.numeric(lecNew2$sangWon_ratio)
lecNew2$installment=as.numeric(lecNew2$installment)

#train test
ind = sample(1:nrow(lecNew2),0.7*nrow(lecNew2))
lecNew2Train = lecNew2[ind,] 
lecNew2Test = lecNew2[-ind,]


colnames(lecNew2Train)
lcRF=randomForest(late ~ installment + desc + term + int_rate + grade + emp_length + home_ownership + annual_inc + purpose + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec2 , data=lecNew2Train, ntree=1000,mtry=4)
plot(lcRF)
summary(lcRF)
importance(lcRF)
varImpPlot(lcRF)
print(lcRF)
lcRF_pred=predict(lcRF,newdata=lecNew2Test)
lcRF_pred

library(e1071)
#install.packages("caret")
library(caret)
confusionMatrix(data=lcRF_pred,reference = lecNew2Test$late)
  
# install.packages("randomForest")
library("randomForest")


#nrow with our 'current'

