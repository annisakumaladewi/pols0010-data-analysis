load('eusurvey.Rda')
load('eupoststrat.Rda')

#packages
library(lme4)
library(lmerTest)
library(arm)
library(data.table)

# Question 1: we are going to do 3 estimates

m0 <- lmer(leave~(1|ccode),data = e)
library(performance)
icc(m0) 

# Varying intercept model
m1 <- lmer(leave ~ votecon + voteukip + female + age + highed + lowed + (1|ccode), data=e)
summary(m1)
fixef(m1)
ranef(m1)
coef(m1)

#Varying intercept model with group level predictors
m2 <- lmer(leave ~ votecon + voteukip + female + age + highed + lowed + c_con15 + c_ukip15 + c_unemployed + c_whitebritish + c_deprived + (1|ccode), data=e)
summary(m2)

#Varying intercept and varying slopes model
m3 <- lmer(leave ~ votecon + voteukip + female + age + highed + lowed + c_con15 + c_ukip15 + c_unemployed + c_whitebritish + c_deprived + (1 + votecon + voteukip + female + highed|ccode), data=e)
summary(m3)
fixef(m3)

#test and training data
set.seed(5)
training.rows2 <- sample(nrow(e),(nrow(e)/2))
training.data2 <- e[training.rows2,]
test.data2 <- e[-training.rows2,]

#model 1: varying intercept model
test.m1 <- lmer(leave ~ votecon + voteukip + female + age + highed + lowed + (1|ccode), data=training.data2)
summary(test.m1)

#predicted probabilities for the test data
logit.probs.test2 <- predict(test.m1,test.data2, type = "response",allow.new.levels=TRUE)
#predicting the class (0 or 1) for each observation in the test data. 1 = predicted probability >0.5
logit.preds.test2 <- ifelse(logit.probs.test2>0.5,1,0)
#forming a confusion matrix to calculate total error rate, sensitivity and specificity of the model
x2 <- table(logit.preds.test2,test.data2$leave)

#error rate
(x2[1,2]+x2[2,1])/nrow(test.data2)
#sensitivity
x2[2,2]/(x2[1,2]+x2[2,2])
#specificity
x2[1,1]/(x2[1,1]+x2[2,1])

# Question 2
# Produce post-stratified estimates
# we will see which variables could improve the model: 
reg <- glmer(leave ~ votecon + voteukip + female + age + highed + lowed + c_con15 + c_ukip15 + c_unemployed + c_whitebritish + c_deprived + (1|ccode), nAGQ = 0, family = binomial(link = "logit"), data = e)
summary(reg)

reg2 <- glmer(leave ~ votecon + voteukip + age + highed + lowed + c_con15 + c_ukip15 + c_unemployed + c_whitebritish + c_deprived + (1|ccode), nAGQ = 0, family = binomial(link = "logit"), data = e)
summary(reg)
#Predict
post$prediction <- predict(reg,newdata=post,type="response",allow.new.levels=TRUE)

#Weight
post$weight.pred <- post$prediction*post$percent*100

# 3. Post-stratify
results <- data.table(post)[ , .(final.est = sum(weight.pred)), by = .(cname)]
View(results)
#10 top and lowest final estimates
results <- results[order(-final.est),]
nrow(results)
results[1:10]
results <- results[order(final.est),]
results[1:10]

#plotting the result against a non-multilevel regression
ccode.means <- tapply(e$leave,e$ccode,mean)
ccode.means
plot(reg$final.est,
     ylim = c(0,100),
     xlab = "Constituency",
     ylab = "Predicted Leave Vote",
     col = "pink")
points(ccode.means*100,col="green")


#odds
coef(reg)
exp(fixef(reg))
cbind(fixef(reg),exp(fixef(reg)))

#MAE
load("existing_estimates.Rda")
library(Metrics)
observed <- (est$estimate)*100
predicted <- results$final.est
mae(observed,predicted)

#plot prediction vs actual
plot(results$final.est, 
     ylim=c(0,100),
     xlab="Constituency",
     ylab="Estimates",
     col="pink")
  points((est$estimate)*100,col = "green")
  legend("topright",
         c("Predicted Estimates","Actual Estimates"),
         pch=c(1,1),
         col=c("pink","green")
)
