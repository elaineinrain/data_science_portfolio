# Data Cleaning
zara <- read.csv(file.choose(), sep = ";", header=TRUE, stringsAsFactors = FALSE)
head(zara)
any(is.na(zara)) # No Missingness
which(zara == "", arr.ind = TRUE) # 3 empty cells in unwanted columns
zara.anl <- zara[,-c(1,4,7,8,9,10,12)] # remove unwanted columns
# 1. Data Exploration
# Convert chr to factor
library(dplyr)
zara.anl <- zara.anl %>%
  mutate(across(where(is.character), as.factor))
attach(zara.anl)
# Scatter plot between price and Sales.Volume
plot(Sales.Volume~price)
abline(lm(Sales.Volume~price), col = "red", lwd = 2)
# 2. Variable Selection
# 2.1 VIF Screening
x=cbind(Product.Position, Promotion, Seasonal, price, terms, section, season, material, origin)
y=Sales.Volume
library(usdm)
vifstep(x, th = 5)
# 3. Find the best first-order model
# 3.1 Fit the base model
fit.full <- lm(Sales.Volume~., data=zara.anl)
summary(fit.full)
# 3.2 All-Subset Selection
library(leaps)
# Mallow's CP
result.cp=leaps(x,y, method="Cp")
which.min(result.cp$Cp)
result.cp$which[60,]
fit.cp <- lm(Sales.Volume~Promotion+Seasonal+price+terms+section+season+material, data=zara.anl)
summary(fit.cp)
# Adjusted R-squared
result.adjr2=leaps(x,y, method="adjr2")
which.max(result.adjr2$adjr2)
result.adjr2$which[60,]
fit.adjr2 <- lm(price~Promotion+Seasonal+price+terms+section+season+material, data=zara.anl) # same as fit.cp
summary(fit.adjr2)
# 3.3 Stepwise Regression
# AIC
step(fit, direction="both", trace=0, k=2)
fit.aic <- lm(Sales.Volume ~ Promotion + Seasonal + price + section + season, data = zara.anl)
summary(fit.aic)
# BIC
step(fit, direction="both", trace=0, k=log(nrow(zara.anl)))
fit.bic <- lm(Sales.Volume ~ Promotion + price + section + season, data = zara.anl)
summary(fit.bic)
# 4. Model Comparison
# models: fit, fit.cp, fit.aic, fit.bic
# 4.1 Leave-One-Out CV (PRESS)
n <- nrow(zara.anl) #sample size
library(asbio)
sqrt(press(fit.full)/n)
sqrt(press(fit.cp)/n)
sqrt(press(fit.aic)/n)
sqrt(press(fit.bic)/n)
# 4.2 5-fold CV
set.seed(318)
K <- 5
n.fold <- floor(n/K)
n.shuffle <- sample(1:n, n, replace=FALSE)
index.fold <- list()
for(i in 1:K)
  {
    if(i<K)
      {
        index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)]
        }else
          {
            index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]
            }
}
# fit.full
CV.score.fitfull <- 0
for(i in 1:K)
  {
  fit <- lm(Sales.Volume~., data=zara.anl[-index.fold[[i]],])
  pred <- predict(fit, newdata=zara.anl[index.fold[[i]],])
  CV.score.fitfull <- CV.score.fitfull+(1/n)*sum((Sales.Volume[index.fold[[i]]]-pred)^2)
  }
# fit.cp
CV.score.fitcp <- 0
for(i in 1:K)
{
  fit <- lm(Sales.Volume~Promotion+Seasonal+price+terms+section+season+material, data=zara.anl[-index.fold[[i]],])
  pred <- predict(fit, newdata=zara.anl[index.fold[[i]],])
  CV.score.fitcp <- CV.score.fitcp+(1/n)*sum((Sales.Volume[index.fold[[i]]]-pred)^2)
}
# fit.aic
CV.score.fitaic <- 0
for(i in 1:K)
{
  fit <- lm(Sales.Volume ~ Promotion + Seasonal + price + section + season, data=zara.anl[-index.fold[[i]],])
  pred <- predict(fit, newdata=zara.anl[index.fold[[i]],])
  CV.score.fitaic <- CV.score.fitaic+(1/n)*sum((Sales.Volume[index.fold[[i]]]-pred)^2)
}
# fit.bic
CV.score.fitbic <- 0
for(i in 1:K)
{
  fit <- lm(Sales.Volume ~ Promotion + price + section + season, data=zara.anl[-index.fold[[i]],])
  pred <- predict(fit, newdata=zara.anl[index.fold[[i]],])
  CV.score.fitbic <- CV.score.fitbic+(1/n)*sum((Sales.Volume[index.fold[[i]]]-pred)^2)
}
# 5-fold CV scores
sqrt(CV.score.fitfull)
sqrt(CV.score.fitcp)
sqrt(CV.score.fitaic)
sqrt(CV.score.fitbic)
# 4.3 BIC scores
BIC(fit.full)
BIC(fit.cp)
BIC(fit.aic)
BIC(fit.bic)
# 4.4 AIC scores
AIC(fit.full)
AIC(fit.cp)
AIC(fit.aic)
AIC(fit.bic)
# 5. Model Diagnostic and Refinement
# 5.1 Plots
# Residual Plot
par(mfrow = c(1, 1))
plot(fit.bic$residuals~fit.bic$fitted.values, main="Residual Plot of fit.bic")
abline(h = 0, col = "red")
# Q-Q Plot
qqnorm(resid(fit.bic))
qqline(resid(fit.bic), col = "red")
# Time Sequence Plot
plot(resid(fit.bic), type="b", main="Time Sequence Plot")
abline(h = 0, col = "red")
# 5.2 Outliers and Influential Points
# Outliers wrt Y
sdr <- rstudent(fit.bic)
out.y <- which(abs(sdr)>2)
length(which(abs(sdr)>2))
# Outliers wrt X
h <- hatvalues(fit.bic)
cutoff.h <- 2*7/n
out.x <- which(h > cutoff.h)
length(which(h > cutoff.h))
# Influential Observations
Di <- cooks.distance(fit.quad) # Cook’s distance
cutoff.d <- qf(0.5, df1=7, df2=n-7)
which(Di > cutoff.d)
# 5.3 Box-Cox Transformation
bc <- boxcox(fit.bic)
bc$x[which.max(bc$y)]
fit.bic.new <- lm(I(Sales.Volume^0.1) ~ Promotion + price + section + season, data=zara.anl)
summary(fit.bic.new)
# 5.3 Plots after Transformation
# Residual Plot
plot(fit.bic.new$residuals~fit.bic.new$fitted.values, main="Residual Plot of fit.bic.new")
abline(h = 0, col = "red")
# Q-Q Plot
qqnorm(resid(fit.bic.new))
qqline(resid(fit.bic.new), col = "red")
# Time Sequence Plot
plot(resid(fit.bic.new), type="b", main="Time Sequence Plot")
abline(h = 0, col = "red")
# 6. Consider interaction terms and higher order terms
# 6.1 Interaction
fit.int <- lm(I(Sales.Volume^0.1) ~ (Promotion + price + section + season)^4,
              data = zara.anl)
summary(fit.int)
anova(fit.int) # Promotion:section is significant
fit.intt <- lm(I(Sales.Volume^0.1) ~ Promotion + price + section + season + Promotion:section, 
              data = zara.anl)
summary(fit.intt)
# 6.2 Higher Order Terms
fit.quad <-lm(I(Sales.Volume^0.1) ~ Promotion + section + season +
                price + I(price^2),
              data=zara.anl)
summary(fit.quad) # Higher order term of price is significant
# Since neither the interaction or the higher order term
# significantly improves model performance overall, 
# we didn't consider them in the final model comparison. 
# 6.3 Model Comparison
# PRESS (transformed to the original scale)
LOOCV <- 0
for (i in 1:n) {
  fit <- lm(
    I(Sales.Volume^0.1) ~ Promotion + price + section + season,
    data = zara.anl[-i, ]
  )
  z_hat <- predict(fit, newdata = zara.anl[i, ])
  y_hat <- z_hat^10
  y_true <- zara.anl$Sales.Volume[i]
  LOOCV <- LOOCV + (y_true - y_hat)^2
}
RMSE_LOOCV <- sqrt(LOOCV / n)
RMSE_LOOCV
# 5-fold (transformed to the original scale)
CV.score.bicnew <- 0
for (i in 1:K) {
  fit <- lm(I(Sales.Volume^0.1) ~ Promotion + price + section + season,
            data = zara.anl[-index.fold[[i]], ])
  z_hat <- predict(fit, newdata = zara.anl[index.fold[[i]], ])
  y_hat  <- z_hat^10
  y_true <- zara.anl$Sales.Volume[index.fold[[i]]]
  CV.score.bicnew <- CV.score.bicnew + (1/n)*sum((y_true - y_hat)^2)
}
sqrt(CV.score.bicnew)
# BIC
BIC(fit.bic.new)
# AIC
AIC(fit.bic.new)
# 7. Final Model
summary(fit.bic.new)
# 7.1 Plots for interpretation
# Effect of Promotion
ggplot(zara.anl, aes(price, Sales.Volume)) +
  geom_point(aes(col=Promotion)) +
  labs(
    title = "Scatterplot between Price and Sales Volume",
    x = "Price",
    y = "Sales Volume"
  )
# Effect of section
ggplot(zara.anl, aes(price, Sales.Volume)) +
  geom_point(aes(col=section)) +
  labs(
    title = "Scatterplot between Price and Sales Volume",
    x = "Price",
    y = "Sales Volume"
  )
# Effect of season
ggplot(zara.anl, aes(price, Sales.Volume)) +
  geom_point(aes(col=season)) +
  labs(
    title = "Scatterplot between Price and Sales Volume",
    x = "Price",
    y = "Sales Volume"
  )