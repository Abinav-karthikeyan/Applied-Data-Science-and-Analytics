# Libraries required for analysis

library(readxl)
library(meditations)
library(mediateP)
library(ResourceSelection)
library(performance)
library(bda)
library(e1071)
library(gbm)


setwd("C:\\Users\\ADMIN\\Desktop\\acads\\RABAT\\Retire")
head(d)
d3=read_xlsx("Excel_DS_Retirement_KL.xlsx")
head(d3)
library(lavaan)
library(semPlot)
library(plotly)
model2 = 'SOBE1~ PONS1 + POVC1+PVVC1+PJNS1 + PJDA1+ORG_AFFECT1+RETADAPT1+INCOME_SAT1+HEALTH_SAT1+
AGE+SENIORITY+EDU+SALARY+DEP_PERSONS
PJDA1~PVVC1'
fit1 = cfa(model2, data = d)
summary(fit1)
d$PV
str(d$SOBE1)
d$SOBE1=ifelse(d$SOBE1>3.8,"yes","no")
d$SOBE1=factor(d$SOBE1,ordered=TRUE,levels=c("no","yes"))
g1=glm(formula = SOBE1~ PONS1 + POVC1+PVVC1+PJNS1 + PJDA1+ORG_AFFECT1+RETADAPT1+INCOME_SAT1+HEALTH_SAT1+
      AGE+SENIORITY+EDU+SALARY+DEP_PERSONS,d,family ="binomial")
g1$coefficients
cor(d$PONS1,d$PONS2)
summary(g1)
summary(d)
str(d)
gg
summary(d)
plot(d$PONS1,d$PONS2)
d$PVFIT=d$PVVC2+d$PVVC3
d$POFIT=d$POVC2+d$PONS3+d$PONS2+d$PVVC1
d$PJFIT=d$PJDA2+d$PJNS1+d$PJNS2+d$PJDA3
write.csv(sum$PE,"")
boxplot(d$SOBE1)
head(d)
summary(d)
d1=data.frame(PVFIT=d$PVFIT,POFIT=d$POFIT,PJFIT=d$PJFIT,AGE=d$AGE,EDU=d$EDU,SENIORITY=d$SENIORITY,
            DEP=d$DEP_PERSONS,HEALTH=(d$HEALTH_SAT1+d$HEALTH_SAT2+d$HEALTH_SAT3+d$HEALTH_SAT4),INCOME=(d$INCOME_SAT1+d$INCOME_SAT2+d$INCOME_SAT3),SOBE=d$SOBE1,AOC=d$ORG_AFFECT1)
head(d)

mat=cor(d[,10:15])
mat$sum=apply(mat,1,sum)
mat$sum
head(d1)
d$A
model2 = 'SOBE~PVFIT+POFIT+PJFIT+AGE+EDU+SENIORITY+DEP+HEALTH+INCOME+AOC
          AOC~POFIT+PJFIT
          POFIT~PVFIT
          PJFIT~PVFIT'
          
fit1 = cfa(model2, data = d1)
g2=glm(formula = model2,d1,family ="binomial" )
g2
summary(g2)
summary(fit1)

chisq.test(g2$coefficients)
semPaths(fit1)
semPaths(fit1, 'std', 'est', curveAdjacent = TRUE, style = "lisrel")
summary(d1)
sum(is.na(d))
cor(d[,1:3])
cor(d[,4:6])
cor(d[,1:10])
d$SO
core=cor(subset(d,select=-c(SOBE1,SOBE2)))
fig <- plot_ly(
 x=colnames(cfg),y=row.names(cfg),
 z = as.matrix(cfg), type = "heatmap",colors = colorRamp(c("red", "green","blue")))
fig
d2=select()
model_performance(fit1)
core
mediate()
m1=glm(formula=SOBE~PVFIT+POFIT+PJFIT+AGE+EDU+SENIORITY+DEP+HEALTH+INCOME+AOC
       ,family ="binomial",data =d1)
summary(m1)
model2 = 'SOBE~PVFIT+POFIT+PJFIT+AGE+EDU+SENIORITY+DEP+HEALTH+INCOME+AOC
          AOC~POFIT+PJFIT'
summary(d)
g2=glm(AOC~POFIT+PJFIT,d1,family="gaussian")
g3=glm(POFIT~PJFIT,d1,family="gaussian")
g4=glm(POFIT~PVFIT,d1,family="gaussian")
g1
fitMed <- mediate(g2,g3,g4 , mediator="AOC")
hoslem.test(g1,10)
d2=d1
summary(d2)
d2$AOC=ifelse(d$SOBE1>3.8,"yes","no")
performance_hosmer(g1,10)
performance_hosmer(g2,10)
mediation.test(d4$PVFIT,d4$POFIT,d4$AOC,d4$SOBE)

sum=summary(fit1)
sum$PE
0.443*0.608/sqrt(0.608^2*0.099^2+ 0.044^2*0.443^2)
(1-pnorm(-1.047)
0.843*0.386/sqrt(0.843^2*0.032^2+ 0.386^2*0.175^2)
)
0.843*0.313*0.049/sqrt(0.843^2*0.098^2*0.033^2+ 0.313^2* 0.098^2*0.175^2 + 0.049^2*0.175^2*0.033^2)
pnorm(-1.144)*2

write.csv(dft,"C:\\Users\\ADMIN\\Desktop\\acads\\RABAT\\Retire\\Class.csv")





media
d3$PVFIT=d3$PVVC2+d3$PVVC3
d3$POFIT=d3$POVC2+d3$PONS3+d3$PONS2+d3$PVVC1
d3$PJFIT=d3$PJDA2+d3$PJNS1+d3$PJNS2+d3$PJDA3
d4=data.frame(PVFIT=d3$PVFIT,POFIT=d3$POFIT,PJFIT=d3$PJFIT,AGE=d3$AGE,EDU=d3$EDU,SENIORITY=d3$SENIORITY,
              DEP=d3$DEP_PERSONS,HEALTH=(d3$HEALTH_SAT1+d3$HEALTH_SAT2+d3$HEALTH_SAT3+d3$HEALTH_SAT4),INCOME=(d3$INCOME_SAT1+d3$INCOME_SAT2+d3$INCOME_SAT3),SOBE=d3$SOBE1+d3$SOBE2,AOC=d3$ORG_AFFECT1)

m1=mediation.test(d4$PVFIT,d4$POFIT,d4$SOBE)
m1
summary(d4)

model1 = 'PVFIT~POFIT
         AOC~POFIT+PVFIT
         SOBE~POFIT+PVFIT+AOC'

model2='PJFIT~PVFIT
       AOC~PJFIT+PVFIT
       SOBE~PJFIT+PVFIT+AOC'

model3='POFIT~PVFIT
       SOBE~PVFIT+POFIT'

model4='PJFIT~PVFIT
       SOBE~PVFIT+PJFIT'

model5='SOBE~PVFIT'

fit1 = cfa(model1, data = d4)
fit2 = cfa(model2, data = d4)
fit3 = cfa(model3, data = d4)
fit4 = cfa(model4, data = d4)
fit5 = cfa(model5, data = d4)
su1=summary(fit1)
su2=summary(fit2)
su3=summary(fit3)
su4=summary(fit4)
su5=summary(fit5)
su1
su2
rb=rbind(su1$PE,su2$PE,su3$PE,su4$PE,su5$PE)
rb
su=summary(fit)
su$PE
(1-pnorm(2.32))*2
d5=d4
summary(d5$SOBE)
d5$SOBE=ifelse(d5$SOBE>9,"yes","no")
d5$SOBE=factor(d5$SOBE,ordered=TRUE,levels=c("no","yes"))
library(randomForest)

rf1=randomForest(SOBE~.,d5,importance=TRUE)
rf1i=rf1$importance
rf1i=as.data.frame(rf1i)
pl1=plotly(x=row.names(rf1i),y=rf1i$MeanDecreaseGini)
summary(fit1,rsquare=TRUE)
l1=lm(model5,data=d4)
summary(l1)

cfg=cor(d4)
cfg


d3=as.data.frame(d3)
head(d3)
library(psych)
help("polychoric")
pc1=polychoric(d3[,1:3])
pc1
pc2=principal(d3[,1:3],2)
pc2$r.scores
fa.poly(pc1$rho)
pc1$rho
pc2$sc
summary(pc2)
pc2
pc1=as.matrix(pc1$rho)
p4=pca(d3[,1:3],cor ="poly")
p4$scores
p5=pca(d3[,1:3])
dfg=data.frame(p4$scores,p5$scores,p6$scores)
head(dfg)
p5=pca(d3[,1:3])
p6=pca(d3[,1:3])

View(d4)
d$PVFIT=d$PVVC2+d$PVVC3
d$POFIT=d$POVC2+d$PONS3+d$PONS2+d$PVVC1
d$PJFIT=d$PJDA2+d$PJNS1+d$PJNS2+d$PJDA3
dfnm=c("POFIT","PJFIT","PVFIT","AOE","SOBE","RET","INC","HEALTH")
j=1
dft=data.frame(d3$DEP_PERSONS)
for(i in seq(1,(length(list)-1),2))
{  prs=pca(d4[,list[i]:list[i+1]],cor ="poly")
   dft=cbind(dft,prs$scores)
   
   
   
}
head(dft)

d
py=pca(d4[,5:8],cor ="poly")
py$scores[1:5]
dft=dft[,c(-1)]
colnames(dft)=dfnm

dft$SOBE1=d3$SOBE1+d3$SOBE2
dft$SOBE=ifelse(dft$SOBE>9,"yes","no")
head(dft)
dft$SOBE=as.factor(dft$SOBE)
summary(dft$SOBE)
head(dft)
library(caret)
library(caTools)
nrow(dft)
sample=sample.split(dft,SplitRatio=0.75) #splitting to understand the nature of data
train1=subset(dft,sample==TRUE)
test1=subset(dft,sample==FALSE)
train.control <- trainControl(method = "cv", number = 10)
nrow(train1)
View(dft)
svm1=train(SOBE~., data=dft[,1:5],method="gbm",trControl = train.control)
svm1$bestTune
sv
svm1$bestTune
summary(svm1)
svm1$xlevels
svm1
svm2 =svm(SOBE~., data=dft[,1:5] , kernel ="linear",cost=0.1,scale =FALSE )
summary(svm2)

gbmpred=predict.gbm(gbm1,test2,type ="response")
c1=confusionMatrix(gbmpred,test2$SOBE)
c1
hea

gbmpred=as.factor(gbmpred)
test2$SOBE=as.factor(test2$SOBE)
test2$SOBE
levels(gbmpred)
head(gbmpred)
summary(gbmpred)
gbmpred=ifelse(gbmpred>0.383,"1","0")

dft=read.csv("Class.csv")
head(dft)
gbm1=gbm(
   SOBE~., data=train2[,2:6],
   distribution = "bernoulli",
   
   cv.folds = 5
   
) 
min(gbm1$cv.error)

gbm1$fit
dft1
train2=train1
head(dft1)
test2=test1
test2$SOBE=ifelse(test2$SOBE=="yes",1,0)
dft1$SOBE=as.factor(dft1$SOBE)
c1$byClass


library(kerasR)
library(tidyverse)
library(neur)
dft=read.csv("Class.csv")
head(dft)
dft=dft[,-c(1,7,8,9,10)]
head(kr)
library(caTools)
library(caret)

sample=sample.split(dft,SplitRatio=0.75) #splitting to understand the nature of data
train1=subset(dft,sample==TRUE)
test1=subset(dft,sample==FALSE)
X_train <- train1[,1:4] %>% scale()
y_train=train1$SOBE
select()
model <- keras_model_sequential() 

model %>% 
   layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>% 
   layer_dropout(rate = 0.4) %>% 
   layer_dense(units = 128, activation = 'relu') %>%
   layer_dropout(rate = 0.3) %>%
   layer_dense(units = 2, activation = 'sigmoid')

#Computing a NN model 
library(neuralnet)
nn=neuralnet(SOBE~.,data=train1, hidden=3,act.fct = "logistic",
             linear.output = FALSE)
plot(nn)
pr=compute(nn,test1)
table(pr,test1$SOBE)
head(pr)
prc=cbind(pr,test1$SOBE)
str(test1)
pr=pr$net.result
pr=as.data.frame(pr)
pr$V1=ifelse(pr$V1>0.7,"yes","no")
pr$V2=test1$SOBE
pr
table(pr$V1,pr$V2)
c2=confusionMatrix(pr$V1,pr$V2)
str(pr)
pr$V1=as.factor(pr$V1)
c2$byClass



