##PART A

#Question 1
setwd("/nfs/cfs/home2/zctq/zctqraa/a_kumaladewi_pols0010")
load('2022essay_q1.Rda')

#i) Choose a logit model that predicts support for longer prison sentences, carefully justifying your selection of variables for the model. You must use a minimum of three independent variables.
mod1 <- glm(formula = sentences ~ sex + age + leftright + wclass + university + urban + married + hincome, family = binomial(link = 'logit'), data = a)
summary(mod1)

#Justify your selection of variables
set.seed(5)
training.rows <- sample(nrow(a),(nrow(a)/2))
training.data <- a[training.rows,]
test.data <- a[-training.rows,]

mod2 <- glm(sentences ~ sex + age + leftright + wclass + university + urban + married + hincome, family = binomial(link = 'logit'), data = training.data)
summary(mod2)

#predicted probabilities for the test data
logit.probs.test <- predict(mod2,test.data, type = "response")
#predicting the class (0 or 1) for each observation in the test data. 1 = predicted probability >0.5
logit.preds.test <- ifelse(logit.probs.test>0.5,1,0)
#forming a confusion matrix to calculate total error rate, sensitivity and specificity of the model
x <- table(logit.preds.test,test.data$sentences) 

#error rate
(x[1,2]+x[2,1])/nrow(test.data)
#sensitivity
x[2,2]/(x[1,2]+x[2,2])
#specificity
x[1,1]/(x[1,1]+x[2,1])

#model 3
mod3 <-glm(sentences ~ age + leftright + university + hincome, family = binomial(link = 'logit'), data = a)
summary(mod3)
#model 4: model 3 testing data
mod4 <- glm(sentences ~ age + leftright + university + hincome, family = binomial(link = 'logit'), data = training.data)
summary(mod4)

#test
logit.preds.test2 <- ifelse(predict(mod4,test.data, type="response")>0.5,1,0)
# Error rate
x2<-table(logit.preds.test2,test.data$sentences)
round(x2[1,2]+x2[2,1]/nrow(test.data)*100,2) 
# Sensitivity
round(x2[2,2]/(x2[1,2]+x2[2,2])*100,2)
# Specificity
round(x2[1,1]/(x2[1,1]+x2[2,1])*100,2)
logit.probs.test2 <- predict(mod4,test.data, type="response")
logit.preds.test <- ifelse(logit.probs.test>0.5,1,0)

#training data
logit.preds.training2 <- ifelse(predict(mod2,training.data, type="response")>0.5,1,0)
x3 <- table(logit.preds.training2,training.data$sentences)

logit.preds.training4 <- ifelse(predict(mod4,training.data, type="response")>0.5,1,0)
x4 <- table(logit.preds.training4,training.data$sentences)

# Error rate
round(x3[1,2]+x3[2,1]/nrow(training.data)*100,2)
round(x4[1,2]+x4[2,1]/nrow(training.data)*100,2)

# Sensitivity
round(x3[2,2]/(x3[1,2]+x3[2,2])*100,2)
round(x4[2,2]/(x4[1,2]+x4[2,2])*100,2)

# Specificity
round(x3[1,1]/(x3[2,1]+x3[1,1])*100,2)
round(x4[1,1]/(x4[2,1]+x4[1,1])*100,2)

#ROC Plot
library(pROC)
roc.plot <- roc(test.data$sentences, logit.probs.test)
roc.plot$auc
plot(roc.plot,legacy.axes=T)
roc.plot2 <- roc(test.data$sentences, logit.probs.test2)
roc.plot2$auc
plot(roc.plot2,legacy.axes=T)

#Question 2
#let us use mod1 first since logit test shows its more reliable
library(mfx)
logitmfx(mod3, data = a, atmean = F)

#find ARE
#Find difference in probabilities
library(arm)
library(boot)
set.seed(1)
regsims <-sim(mod3,n.sims=1000)
coefs <-coef(regsims)

#predicted probabilities for hincome
values1 <- c(1, mean(a$age),
             1, 
             1,
             median(a$hincome)
             )
pred.outcomes1 <- inv.logit(values1 %*% t(coefs))

values2 <- c(1, mean(a$age),
             5,
             1,
             median(a$hincome)
             )
pred.outcomes2 <- inv.logit(values2 %*% t(coefs))


pred.diff <- pred.outcomes2 - pred.outcomes1

mean(pred.diff, na.rm = TRUE)

quantile(pred.diff, c(0.025,0.975))

#predicted probabilities for hincome
values3 <- c(1, mean(a$age),
             3, 
             1,
             1
)
pred.outcomes3 <- inv.logit(values3 %*% t(coefs))

values4 <- c(1, mean(a$age),
             3,
             1,
             10
)
pred.outcomes4 <- inv.logit(values4 %*% t(coefs))


pred.diff2 <- pred.outcomes4 - pred.outcomes3

mean(pred.diff2, na.rm = TRUE)

quantile(pred.diff2, c(0.025,0.975))
