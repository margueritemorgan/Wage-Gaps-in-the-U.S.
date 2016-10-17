library(faraway)
wage <- uswages$wage
educ <- uswages$educ
exper <- uswages$exper
race <- uswages$race
smsa <- uswages$smsa
ne <- uswages$ne
mw <- uswages$mw
so <- uswages$so
we <- uswages$we
pt <- uswages$pt

#just look at fulltime workers
uswages1 <- uswages[pt == 0,]
summary(uswages1) #negative value for experience
uswages2 <- uswages1[uswages1$exper>=0,]
summary(uswages2)
wage1 <- uswages2$wage
educ1 <- uswages2$educ
exper1 <- uswages2$exper
race1 <- uswages2$race
smsa1 <- uswages2$smsa

#initial check data
pairs(~wage1 + educ1 + exper1 + race1 + smsa1)
pairs(~log(wage1) + educ1 + exper1 + race1 + smsa1, main='Scatterplots of Relationships Between Variables')

#compare model assumptions
m1 <- lm(log(wage1) ~ educ1 + exper1 + race1 + smsa1)
summary(m1)
plot(m1, 1, main='Log-Normal Residual Plot')
plot(m1, 2, main='Log-Normal Normal Q-Q Plot')
m2 <- glm(wage1 ~ educ1 + exper1 + race1 + smsa1, family=Gamma (link="log"))
summary(m2)
plot(m2, 1, main='Gamma GLM Residual Plot')
plot(m2, 2, main='Gamma GLM Normal Q-Q Plot')
m3 <- glm(wage1 ~ educ1 + exper1 + race1 + smsa1, family=quasi (link='log' , variance='constant'))
plot(m3, 1)
plot(m3, 2)
m4 <- glm(wage1 ~ educ1 + exper1 + race1 + smsa1, family=quasi (link='log' , variance='mu'))
plot(m4, 1)
plot(m4, 2)
m5 <- glm(wage1 ~ educ1 + exper1 + race1 + smsa1, family=quasi (link='log' , variance='mu^2'))
plot(m5, 1)
plot(m5, 2)

#proceed with m2 - assumption of constant variance is roughly met
summary(m2)

#try jointly modeling mean and dispersion
str(uswages2)
plot(uswages2[,c(1, 2, 3, 4, 5)]) #need log transformation on wage
uswages2 <- within(uswages2, {lwage1 <- log(wage1)})
str(uswages2)
#recheck linearity assumptions
plot(uswages2[,c(11, 2, 3, 4, 5)])
boxplot(log(wage1) ~ educ1, data=uswages2) #educ impacts variance
boxplot(log(wage1) ~ exper1, data=uswages2)
boxplot(log(wage1) ~ race1, data=uswages2)
boxplot(log(wage1) ~ smsa1, data=uswages2)

summary(log(wage1), data=uswages2)

mmod <- lm(log(wage1) ~ educ1 + exper1 + race1 + smsa1, data=uswages2)
plot(mmod, 1)
d <- residuals(mmod)^2
dmod <- glm(d ~ educ1, data=uswages2, family=Gamma(link='log'))
plot(dmod, 1)
mmod1 <- mmod

# iterate
TOLERANCE <- 1e-6
mmod <- lm(log(wage1) ~ educ1 + exper1 + race1 + smsa1, data=uswages2)
dm <- deviance(mmod)
repeat {
  d <- residuals(mmod) ^ 2
  dmod <- glm(d ~ educ1, data=uswages2, family=Gamma(link='log'))
  mmod <- lm(log(wage1) ~ educ1 + exper1 + race1 + smsa1, data=uswages2,
             weights = 1 / fitted(dmod))
  dm.new <- deviance(mmod)
  if (abs(dm.new - dm) / dm < TOLERANCE) break
  dm <- dm.new
}

# compare with original fit
plot(mmod, 1) 
plot(mmod1, 1)
summary(mmod1)
summary(mmod)
#models look extremeley similar

#compare models
gb0 <- glm(wage1 ~ 1, data=uswages2, family=Gamma (link='log'))
gb1 <- glm(wage1 ~ educ1, data=uswages2, family=Gamma (link='log'))
gb2 <- glm(wage1 ~ educ1 + exper1, data=uswages2, family=Gamma (link='log'))
gb3 <- glm(wage1 ~ educ1 + exper1 + race1, data=uswages2, family=Gamma (link='log'))
gb4 <- glm(wage1 ~ educ1 + exper1 + race1 + smsa1, data=uswages2, family=Gamma (link='log'))
gb5 <- glm(wage1 ~ educ1 + exper1 + educ1:exper1 + race1 + smsa1, data=uswages2, family=Gamma (link='log'))
gb6 <- glm(wage1 ~ educ1 + exper1 + educ1:exper1 + race1 + smsa1 + educ1:race1, data=uswages2, family=Gamma (link='log'))
gb7 <- glm(wage1 ~ educ1 + exper1 + educ1:exper1 + race1 + smsa1 + exper1:race1 + exper1:race1, data=uswages2, family=Gamma (link='log'))
anova(gb0, gb1, gb2, gb3, gb4, gb5, gb6, gb7, test='F')

summary(gb4)
#gb4 is best model

loglinearmodel <- glm(log(wage1) ~ educ1 + exper1 + race1 + smsa1, data=uswages2, family=gaussian)
gammamodel <- glm(wage1 ~ educ1 + exper1 + race1 + smsa1, data=uswages2, family=Gamma (link='log'))
summary(loglinearmodel) #has large variance 
summary(gammamodel) #1/phi is not very large so not well approximated by lognormal

sqrt(0.5468585)
library(MASS)
gamma.dispersion(gammamodel) #MLE of phi = X^2/n-p #not the same as dispersion in gamma model
sum(residuals(gammamodel, type='pearson')^2) / df.residual(gammamodel)
nu <- 1/gamma.dispersion(gammamodel, verbose=T)
log(nu) - digamma(nu)

library(AER)


deviance(gammamodel)/(2*nrow(uswages1))


#plots
x <- seq(0, 5, by=0.05)
plot(x, dgamma(x, 1/0.5468585, scale=0.5468585), ylab='', xlab='', yaxs='i', ylim=c(0,1))
plot(x, dlnorm(x, meanlog=-0.30551,sdlog=sqrt(0.2940268)), ylab='', xlab='', yaxs='i', ylim=c(0,1.2))
#lognormal has higher kurtosis

#compare loglinear and gamma
summary(m1)
summary(m2)
plot(m2)
plot(m1)


summary(wage1)
summary(log(wage1))

boxplot(log(wage1) ~ exper1, data=uswages2, main='Log(Wage) ~ Experience')
boxplot(log(wage1) ~ educ1, data=uswages2, main='Log(Wage) ~ Education')
boxplot(log(wage1) ~ race1, data=uswages2, main='Log(Wage) ~ Race')
boxplot(log(wage1) ~ smsa1, data=uswages2, main='Log(Wage) ~ Metropolitan Area')

hist(wage1) #very right skewed, all positive values - gamma regression good



#is there a significant difference between having more than college education and not
college <- ifelse(educ1 > 12, 1, 0)
educ_model <- glm(wage1 ~ race1 + smsa1 + college + exper, data=uswages2, family=Gamma (link='log'))
summary(educ_model)

sum(work4)
