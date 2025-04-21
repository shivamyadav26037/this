#linear regression
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48) 

model <- lm(y~x)
summary(model)
new_data <- data.frame(x = 170)
predicted_weight <- predict(model, newdata = new_data)
predicted_weight

coeffs <- coef(model)
cat("y=", coeffs[1], "+" , coeffs[2],"* x", "\n")
intercept <- coef(model)[1]
slop <- coef(model)[2]
cat("y=", intercept, "+" , slop,"* x", "\n")
#Slope (0.675):
 # For every 1 cm increase in height, the weight increases by approximately 0.675 kg.
#Intercept (-38.455):
 # When height is 0 cm (not realistic in context), the predicted weight would be -38.455 kg. This doesn't make physical sense, but it’s a mathematical component of the regression line.

#multiple
# Load dataset
data(mtcars)
# Fit multiple linear regression model
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
# Print model summary
summary(model)
# Extract coefficients (slope & intercept)
coeffs <- coef(model)
intercept <- coeffs[1]
disp_slope <- coeffs[2]
hp_slope <- coeffs[3]
wt_slope <- coeffs[4]
# Print the regression equation
cat("mpg =", intercept, 
    "+", disp_slope, "* disp",
    "+", hp_slope, "* hp",
    "+", wt_slope, "* wt", "\n")


#2
original <- c(-2, 1, -3, 2, 3, 5, 4, 6, 5, 6, 7)
predicted <- c(-1, -1, -2, 2, 3, 4, 4, 5, 5, 7, 7)
lines(x, predicted, col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1)
       ,cex = 0.7)
d = original - predicted
mse <- mean((d)^2)
mae <- mean(abs(d))
rmse <- sqrt(mse)

R2 = 1-((sum(d)^2)/sum(original-mean(original))^2)
cat("MAE:",mae,"\n","MSE:",mse,"\n","RMSE:",rmse,"\n","R-squared:",R2)

#3
#2nd-degree polynomial 
# Load the dataset
data(mtcars)
# Fit a 2nd-degree polynomial regression model
model_poly <- lm(mpg ~ wt + I(wt^2), data = mtcars)
# Summary of the model (optional)
summary(model_poly)
# Plot the original data
plot(mtcars$wt, mtcars$mpg,
     main = "2nd-Degree Polynomial Regression",
     xlab = "Weight (wt)",
     ylab = "Miles Per Gallon (mpg)",
     pch = 19,
     col = "blue")
curve(predict(lm(mpg ~ wt + I(wt^2), data=mtcars), data.frame(wt=x)), add=TRUE, col="red", lwd=2)


# Load the dataset
data(mtcars)
# Fit a 3rd-degree polynomial regression model
model_poly <- lm(mpg ~ wt + I(wt^2)+ I(wt^3), data = mtcars)
# Summary of the model (optional)
summary(model_poly)
# Plot the original data
plot(mtcars$wt, mtcars$mpg,
     main = "3nd-Degree Polynomial Regression",
     xlab = "Weight (wt)",
     ylab = "Miles Per Gallon (mpg)",
     pch = 19,
     col = "blue")
curve(predict(lm(mpg ~ wt + I(wt^2)+ I(wt^3), data=mtcars), data.frame(wt=x)), add=TRUE, col="red", lwd=2)

set.seed(322)
x1<-rnorm(20,1,0.5)
x2<-rnorm(20,2,0.98)
y1<-rnorm(20,8,2.15)
Model1_3degree<-lm(y1~polym(x1,x2,degree=3,raw=TRUE))
summary(Model1_3degree)

# Load dataset
data(mtcars)
# Fit logistic regression model to predict transmission (am)
model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)
# Summary of the model
summary(model)
# Extract coefficients
coeffs <- coef(model)
# Print logistic regression equation
cat("Logistic Regression Equation:\n")
cat("log(p / (1 - p)) = ",
    coeffs[1], " + ",
    coeffs[2], " * mpg + ",
    coeffs[3], " * hp + ",
    coeffs[4], " * wt", "\n")
#In summary(model) output, look at:
#Coefficients: Each predictor’s estimate (log-odds)
#Pr(>|z|): P-value to test significance
#p < 0.05 → statistically significant predictor

#Poisson Regression
input <- warpbreaks 
print(head(input)) 
output <- glm(formula = breaks ~ wool + tension,
             data =warpbreaks,family = poisson) 
print(summary(output))
#In the summary(model):
#Coefficients represent the log of expected warp breaks.
#Significance codes (p-values) help determine if predictors (wool, tension) are statistically significant.
#Interpreting coefficients:
#If a coefficient is positive → increases expected count
#If negative → decreases expected count
#What type of response variable is suitable for Poisson regression?
#Count data: Non-negative integers (0, 1, 2, …)
#Discrete events over a fixed period or space (e.g., number of breaks, accidents, calls)
#Response should follow a Poisson distribution:
#Mean ≈ Variance (check for overdispersion if variance > mean)




#4
install.packages("lmtest")
library(lmtest)
x<-c(1,2,3,4,5,6,7,8,9,10)
y<-c(2,5,9,13,18,24,31,39,48,60) + rnorm(10, mean=0, sd=x)
model<-lm(y~x)
plot(x, residuals(model), main="Residuals vs X", ylab="Residuals", xlab="X")
abline(h=0, col="red")
bptest(model)



install.packages("stats")
library(stats)
sales_data<-c(100, 102, 104, 103, 105, 107, 106, 108, 110, 112)
acf(sales_data, main="Autocorrelation of Sales Data")
acf_values<-acf(sales_data, plot=FALSE)
print(acf_values)



install.packages("car")
library(car)
set.seed(123)
X1<-c(1,2,3,4,5,6,7,8,9,10)
X2<-X1*2+rnorm(10, mean=0, sd=0.5)
Y<-3+2*X1+1.5*X2+rnorm(10, mean=0, sd=1)
model_mc<-lm(Y~X1+X2)
vif_values<-vif(model_mc)
print(vif_values)

# Step 1: Load dataset
data(mtcars)
# Step 2: Fit the linear regression model
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
# Step 3: Load car package (for VIF)
install.packages("car")       # Run only once
library(car)
# Step 4: Calculate VIF
vif(model)
#(Look at summary(model) output:Coefficients: How much mpg changes per unit increase in each predictor.
#Pr(>|t|):
  #p-value — if < 0.05, the predictor is statistically significant.
#R-squared: How much variance in mpg is explained by the model.Variance Inflation Factor (VIF)
#VIF > 5 → moderate multicollinearity
#VIF > 10 → high multicollinearity
#disp and hp show moderate multicollinearity
#If VIF is too high, consider removing or combining variables)



#5 practical
set.seed(123)
x <- 1:100
y <- 5 + 0.5 * x + rnorm(100, sd = x / 10)
model1 <- lm(y ~ x)
summary(model1)
plot(model1$fitted.values, resid(model1), main = "Residual Plot",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
resid_sq <- resid(model1)^2
var_model <- lm(resid_sq ~ x)
pred_var <- predict(var_model)
pred_var[pred_var <= 0] <- min(pred_var[pred_var > 0]) + 1e-6
weights <- 1 / pred_var
model_wls <- lm(y ~ x, weights = weights)
summary(model_wls)





