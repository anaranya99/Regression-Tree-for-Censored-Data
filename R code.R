library(readxl)
library(survival)
library(rpart)
library(rpart.plot)
library(dplyr)
library(rpart)
library(Metrics)
attach(mtcars)
tree1=rpart(mtcars$mpg~mtcars$cyl+mtcars$hp+mtcars$gear,data=mtcars,method = "anova",minsplit=2)
rpart.plot(tree1)
attach(ptitanic)
head(ptitanic)
tree2=rpart(ptitanic$survived~., data = ptitanic,minsplit=1)
rpart.plot(tree2)
attach(Titanic)
library(LTRCtrees)
z=read.csv("C:/Users/DELL/Desktop/churn.csv")
tree4=rpart(z$Exited~.,data=z,minsplit=2,method="anova")
rpart.plot(tree4)
plotcp(tree4)
z$Gender=as.factor(z$Gender)
tree_model7=rpart(Exited ~ ., data = z, method = "class")
rpart.plot(tree_model7, type = 1, extra = 106, under = TRUE, nn = TRUE)
q=read_xlsx("C:/Users/DELL/Desktop/REMISSION.xlsx")
surv_remission=Surv(q$`6-MP Patients`,q$Status)
tree6=rpart(q$`Remission Status`~surv_remission,data=q,minsplit=1)
summary(tree6)
rpart.plot(tree6)
##################### Remmission Status........###########
library(rpart)
data90 <- data.frame(
  Remission_Status = c("PR", "CR", "CR", "CR", "CR", "PR", "CR", "CR", "CR", "CR",
                       "CR", "PR", "CR", "CR", "CR", "PR", "PR", "CR", "CR", "CR",
                       "CR", "CR"),
  Placebo_patients = c(1, 22, 3, 12, 8, 17, 2, 11, 8, 12, 2, 5, 4, 15, 8, 23, 5, 11, 4, 1, 8, 10),
  MP6_Patients = c(10, 7, 32, 23, 22, 6, 16, 34, 32, 25, 11, 20, 19, 6, 17, 35, 6, 13, 9, 6, 10, 19),
  Status = c(1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0)
)
tree_model <-rpart(Status~ ., data = data90, method = "anova",minsplit=2)
print(tree_model)
rpart.plot(tree_model)
###################   HEART TRANSPLANT DATA###############################
library(survival)
jasa$subject=1:nrow(jasa)

tdata= with(jasa, data.frame(subject = subject,
                               futime= pmax(.5, fu.date - accept.dt),
                               txtime= ifelse(tx.date== fu.date,
                                              (tx.date -accept.dt) -.5,
                                              (tx.date - accept.dt)),
                               fustat = fustat))

sdata=tmerge(jasa, tdata, id=subject,death = event(futime, fustat),trt = tdc(txtime), options= list(idname="subject"))

sdata$age=sdata$age - 48

sdata$year=as.numeric(sdata$accept.dt - as.Date("1967-10-01"))/365.25
LTRCART.fit=LTRCART(Surv(tstart, tstop, death) ~ age + transplant, data = sdata)
LTRCIT.fit=LTRCIT(Surv(tstart, tstop, death) ~ age + transplant, data = sdata)
rpart.plot(LTRCART.fit)
plot(LTRCIT.fit)
library(survival)
library(rpart)
data1=read.csv("C:/Users/DELL/Desktop/heart_transplant (1).csv")
data1$survtime=as.numeric(data1$survtime)
data1$survived=as.factor(data1$survived)
data1$prior=as.factor(data1$prior)
data1$transplant=as.factor(data1$transplant)
survival_tree=rpart(Surv(survtime, survived) ~ age + prior + transplant + wait, data = data1, method = "exp")
set.seed(0)
library(survival)
first <- with(pbcseq, c(TRUE, diff(id) !=0)) #first id for each subject
last <- c(first[-1], TRUE) #last id
time1 <- with(pbcseq, ifelse(first, 0, day))
time2 <- with(pbcseq, ifelse(last, futime, c(day[-1], 0)))
event <- with(pbcseq, ifelse(last, status, 0))
event <- 1*(event==2)
pbcseq$time1 <- time1
pbcseq$time2 <- time2
pbcseq$event <-  event
fit.cox <- coxph(Surv(time1, time2, event) ~ age + sex + log(bili), pbcseq)
LTRCIT.fit1 <- LTRCIT(Surv(time1, time2, event) ~ age + sex + log(bili), pbcseq)
LTRCART.fit2 <- LTRCART(Surv(time1, time2, event) ~ age + sex + log(bili), pbcseq)
fit.cox 
prp(LTRCART.fit1,type=0, roundint=FALSE)
plot(LTRCIT.fit1)
prp(LTRCART.fit1)
################################################
library(survival)
library(rpart)
library(rpart.plot)
attach(jasa)
survivaltree=rpart(Surv(time,status)~age+t5,data=stanford2)
rpart.plot(survivaltree)
prune.rpart(survivaltree,cp=0.001)
rpart.plot(prune.rpart(survivaltree,cp=0.001))
summary(survivaltree)





