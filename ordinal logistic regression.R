set.seed(100)
my_data <- inner_join(stratified[,c(1,3,9,73,74)], noout[,c(1,32:34)], by = c("X" = "X"))
my_data$f1 <- round(my_data$f1)
my_data$f2 <- ceiling(my_data$f2)
my_data$f3 <- round(my_data$f3)
summary(my_data)

my_data$f1 <- factor(my_data$f1,
                     levels = c(0,1,2,3,4,5),
                     labels = c("Not Applicable",
                                "Strongly Agree",
                                "Agree","Neutral",
                                "Disagree",
                                "Strongly Disagree"))
my_data$f2 <- factor(my_data$f2,
                     levels = c(0,1,2,3,4,5),
                     labels = c("Not Applicable",
                                "Strongly Agree",
                                "Agree","Neutral",
                                "Disagree",
                                "Strongly Disagree"))
my_data$f3 <- factor(my_data$f3,
                     levels = c(0,1,2,3,4,5),
                     labels = c("Not Applicable",
                                "Strongly Agree",
                                "Agree","Neutral",
                                "Disagree",
                                "Strongly Disagree"))

my_data$C.G.P <- factor(my_data$C.G.P,
                        levels = unique(c("4.5- 5.0","3.5 - 4.4","2.5 - 3.4","1.5- 2.4")),
                        labels = c("First class","Second class","Second class lower","Third class"))

# exploration
summary(my_data[,-1])
write.csv(my_data,"dataset/Ordinal.csv")


attach(my_data)
# OLR using ordinal package
#### model null
modelnull <- clm(as.factor(C.G.P) ~ 1,
                 dataa = my_data,
                 link = "logit")

#### model1
model1 <- clm(as.factor(C.G.P) ~ as.factor(stress) + as.factor(gender),
              data = my_data,
              link = "logit")
anova(modelnull,model1)

nagelkerke(fit = model1, null = modelnull)

summary(model1)
confint(model1)
exp(coef(model1))
exp(confint(model1))
model1$fitted.values
fit <- predict(model1)
# test of goodness
chisq.test(my_data$C.G.P,unlist(predict(model1)))
#Compute confusion table and misclassification error
predictOLR <- predict(model1, my_data)
ctab <- table(predictOLR, fit)
ctab
mean(as.character(my_data$C.G.P) != as.character(predictOLR))
(CCR <- sum(diag(cTab)) / sum(cTab)) # Calculate the classification rate

# Load the DescTools package for calculate the R square
# library("DescTools")
# Calculate the R Square
# PseudoR2(OLRmodel, which = c("CoxSnell","Nagelkerke","McFadden"))
# We can use the coef() function to check the parameter estimates
(OLRestimates <- coef(summary(OLRmodel)))
# Add the p-value to our parameter estimates table
p <- pnorm(abs(OLRestimates[, "t value"]), lower.tail = FALSE) * 2
# (OLRestimates_P <- cbind(OLRestimates, "p value" = p))
# Bulding the newpatient entry to test our model
newpatient <- data.frame("subject" = 1, "Impairment" = NA, "ses" = "high", "lifeevents"=3)
# Use predict function to predict the patient's situation
predict(OLRmodel,newpatient)

# Calculate the R Square
PseudoR2(model1, which = c("CoxSnell","Nagelkerke","McFadden"))
# We can use the coef() function to check the parameter estimates
(OLRestimates <- coef(summary(model1)))
