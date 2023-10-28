#--------EXPLORATORY FACTOR ANALYSIS-------
# library
library(psych)
library(GPArotation)
library(mice)


set.seed(42)
#--------Sampling design--------
stratified <- survey %>%
  group_by(gender) %>%
  sample_n(size = 100)

# sub-seting required variables
master <- stratified[,c(1,43:72)]


##accuracy
summary(master)

# handling missing data
# replacing NA's with 3
master[is.na(master)] <- 0



##missing
percentmissing <- function(x){ sum(is.na(x))/length(x) * 100}
missing <- apply(master, 1, percentmissing)
table(missing)

##outliers
cutoff <- qchisq(1-.001,ncol(master))
mahal <- mahalanobis(master,colMeans(master),
                     cov(master))
cutoff ##cutoff score

summary(mahal < cutoff)

##exclude outlier
noout <- subset(master, mahal < cutoff)

##additive
correl <- cor(noout, use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random <- rchisq(nrow(noout), 5)
fake <- lm(random~., data = noout)

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
cortest.bartlett(correl, n = nrow(noout))

##sampling adequacy KMO test
KMO(correl)

 ##how many factors?
nofactors <- fa.parallel(noout, fm = "ml", fa = "fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 <- fa(noout, nfactors = 3, rotate = "oblimin", fm = "ml")
round1
round2 <- fa(noout[ , -c(18,20)], nfactors = 3, rotate = "oblimin", fm = "ml")
round2

##get cfi
finalmodel <- fa(noout[ , -c(18,20)], nfactors = 3, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC - finalmodel$dof)/(finalmodel$null.chisq - finalmodel$null.dof))

##reliability
factor1 <- c(2,12,16,26,30)
factor2 <- c(11,14,15,22,24,27)
factor3 <- c(10,17,25,28,29)
psych::alpha(noout[ ,factor1])
psych::alpha(noout[ ,factor2])
psych::alpha(noout[ ,factor3], check.keys = TRUE)

##create new factor scores
noout$f1 = ceiling(apply(noout[ , factor1], 1, mean)) #create average score
noout$f2 = ceiling(apply(noout[ , factor2], 1, mean)) #create average score
noout$f3 = ceiling(apply(noout[ , factor3], 1, mean)) #create average score

summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)

