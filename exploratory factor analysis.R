set.seed(42)
library(foreign)
library(reshape2)
library(tidyverse)
#--------Sampling design--------
stratified <- survey %>%
  group_by(gender) %>%
  sample_n(size = 100)

# subseting required variables
master <- stratified[,c(1,43:72)]

##accuracy
#summary(master[,-1])

# handling missing data
# replacing NA's with 3
master[is.na(master)] <- 0

##missing
percentmissing <- function(x){ sum(is.na(x))/length(x) * 100}
missing <- apply(master, 1, percentmissing)
#table(missing)

##outliers
cutoff <- qchisq(1-.001,ncol(master))
mahal <- mahalanobis(master[,-1],colMeans(master[,-1]),
                     cov(master[,-1]))
#cutoff ##cutoff score

#summary(mahal < cutoff)

##exclude outlier
noout <- subset(master, mahal < cutoff)

##additive
correl <- cor(noout[,-1], use = "pairwise.complete.obs")
#symnum(correl)
#correl

##assumption set up
random <- rchisq(nrow(noout), 5)
fake <- lm(random~., data = noout[,-1])

standardized <- rstudent(fake)
fitted <- scale(fake$fitted.values)

## normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

#homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##running the EFA analysis

##correlation adequacy Bartlett's test
cor.test <- cortest.bartlett(correl, n = nrow(noout[,-1]))

##sampling adequacy KMO test
kmo <- KMO(correl)

##how many factors?
nofactors <- fa.parallel(noout, fm = "ml", fa = "fa")
oldKaiser <- sum(nofactors$fa.values > 1.0) ##old kaiser criterion
newKaiser <- sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a two factor model
round1 <- fa(noout[,-1], nfactors = 2, rotate = "oblimin", fm = "ml")
#round1
round2 <- fa(noout[ , c(3,10,13,17,19,21,23:27,30,31)], nfactors = 2, rotate = "oblimin", fm = "ml")

##simple structure with a three factor model
round3 <- fa(noout[,-1], nfactors = 3, rotate = "oblimin", fm = "ml")
#round3
round4 <- fa(noout[ , c(3,10,12,13,16,17,19,21,23:27,30,31)], nfactors = 3, rotate = "oblimin", fm = "ml")

##get cfi
finalmodel <- round4
model <- 1 - ((finalmodel$STATISTIC - finalmodel$dof)/(finalmodel$null.chisq - finalmodel$null.dof))

##reliability
factor1 <- c(3,13,17,21,27,31)
factor2 <- c(19,23:26)
factor3 <- c(12,16)
alpha1 <- psych::alpha(noout[ ,factor1])
alpha2 <-psych::alpha(noout[ ,factor2])
alpha3 <-psych::alpha(noout[ ,factor3], check.keys = TRUE)

##create new factor scores
noout$f1 = apply(noout[ , factor1], 1, mean) #create average score
noout$f2 = apply(noout[ , factor2], 1, mean) #create average score
noout$f3 = apply(noout[ , factor3], 1, mean) #create average score

Summarynewfacto <- summary(noout[,c(32:34)])
sd1 <-sd(noout$f1)
sd2 <- sd(noout$f2)
sd3 <- sd(noout$f3)



# change variables to factors
my_data <- inner_join(stratified[,c(1:3,8,9,73,74)], noout[,c(1,32:34)], by = c("X" = "X"))
my_data$f1 <- round(my_data$f1)
my_data$f2 <- ceiling(my_data$f2)
my_data$f3 <- round(my_data$f3)
my_data$f1 <- factor(my_data$f1, levels = 5:0,
                     labels = c("Strongly Agree",
                                "Agree",
                                "Neutral",
                                "Disagree",
                                "Strongly Disagree",
                                "Not Applicable"),
                     ordered = T)
my_data$f2 <- factor(my_data$f2, levels = 5:0,labels = c("Strongly Agree",
                                "Agree",
                                "Neutral",
                                "Disagree",
                                "Strongly Disagree",
                                "Not Applicable"),
                     ordered = T)
my_data$f3 <- factor(my_data$f3, levels = 5:0,
                     labels = c("Strongly Agree",
                                "Agree",
                                "Neutral",
                                "Disagree",
                                "Strongly Disagree",
                                "Not Applicable"),
                     ordered = T)
my_data$C.G.P <- factor(my_data$C.G.P,
                        levels = unique(c("4.5- 5.0",
                                          "3.5 - 4.4",
                                          "2.5 - 3.4",
                                          "1.5- 2.4"
                                          )),
                        labels = c("First class",
                                   "Second class",
                                   "Second class lower",
                                   "Third class"
                                   ),
                        ordered = TRUE)
my_data$G.P.A <- factor(my_data$G.P.A,
                        levels = unique(c("4.5 - 5.0",
                                          "3.5 - 4.4",
                                          "2.5 - 3.4",
                                          "1.5- 2.4"
                                          )),
                        labels = c("First class",
                                   "Second class",
                                   "Second class lower",
                                   "Third class"
                                   ),
                        ordered = TRUE)


summary(my_data)

