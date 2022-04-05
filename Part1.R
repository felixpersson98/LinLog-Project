# Import ggplot library
library(ggplot2)

# Read data file
data <- read.delim(file = "Data/plasma.txt")
head(data)
summary(data)

# Delete invalid datapoint and store minimum
data <- data[data$betaplasma > 0, ]
minage <- min(data$betaplasma)


###### Linear model ######
model1.linear <- lm(betaplasma ~ I(age - minage), data = data)
summary(model1.linear)
confint(model1.linear)

# Fit a line and display data
model1.linear.pred <- cbind(data, 
        fit = predict(model1.linear),
        conf = predict(model1.linear, interval = "confidence"),
        pred = predict(model1.linear, interval = "prediction"))
head(model1.linear.pred)

# Get rid of extra fits
model1.linear.pred$conf.fit <- model1.linear.pred$pred.fit <- NULL
head(model1.linear.pred)

# Plot beta-carotene against age and save image
(
  plot1.data <- 
    ggplot(data = model1.linear.pred, aes(x = age, y = betaplasma)) + 
    geom_point(size = 2) +
    xlab("Age (years)") +
    ylab("Beta-carotene (ng/ml)") +
    labs(title = "Age and data beta-carotene") +
    theme(text = element_text(size = 12))
)

# Add the fitted line to the data plot
(
  plot1.line <- plot1.data + 
    geom_line(aes(y = fit), color = "blue", size = 1) +
    labs(title = "Age and data beta-carotene, linear model")
)


###### Logarithmic model ######
model2.log <- lm(log(betaplasma) ~ I(age - minage), data = data)

# Fit a log line and display head
model2.log.pred <- cbind(data, 
        fit = predict(model2.log),
        conf = predict(model2.log, interval = "confidence"),
        pred = predict(model2.log, interval = "prediction"))
head(model2.log.pred)

# Get rid of the extra fits
model2.log.pred$conf.fit <- model2.log.pred$pred.fit <- NULL
head(model2.log.pred)

# Plot log(beta-carotene) against age
(
  plot2.data <- 
    ggplot(data = model2.log.pred, aes(x = age, y = log(betaplasma))) + 
    geom_point(size = 2) +
    xlab("Age (years)") +
    ylab("log(Beta-carotene (ng/ml))") +
    labs(title = "Age and log(beta-carotene)") +
    theme(text = element_text(size = 12))
)

# Add the fitted line to the data plot
(
  plot2.line <- plot2.data + 
    geom_line(aes(y = fit), color = "blue", size = 1) +
    labs(title = "Age and data beta-carotene, logarithmic model")
)

# Add confidence interval
(
  plot2.conf <- plot2.line + 
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2)
)

# Add prediction interval
plot2.conf + geom_line(aes(y = pred.lwr),
                       color = "red", linetype = "dashed", size = 1) +
             geom_line(aes(y = pred.upr), 
                       color = "red", linetype = "dashed", size = 1) +
             labs(title = "Age and data beta-carotene, logarithmic model, with 
                       confidence and prediction intervals")


###### Residual Analysis ######
### Linear model ###

# Add the residuals to the predicted data
model1.linear.pred$e <- model1.linear$residuals
head(model1.linear.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max1.e <- max(abs(model1.linear.pred$e)))
(model1.linear.pred.elims <- c(-max1.e, max1.e))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
ggplot(data = model1.linear.pred, aes(x = age, y = e)) + geom_point(size = 2) + 
       geom_hline(yintercept = 0) + 
       expand_limits(y = model1.linear.pred.elims) + xlab("År") + 
       ylab("Residual") + labs(title = "Residuals vs x-values") +
       theme(text = element_text(size = 14))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
ggplot(data = model1.linear.pred, aes(x = fit, y = e)) + geom_point(size = 2) +
       geom_hline(yintercept = 0) + expand_limits(y = model1.linear.pred.elims) 
        + xlab("Predicted betakaroten") + ylab("Residual") +
       labs(title = "Residuals vs predicted values Y-hat") +
       theme(text = element_text(size = 14))

# Make a normal qq-plot of the residuals.
ggplot(data = model1.linear.pred, aes(sample = e)) + geom_qq(size = 2) +
       geom_qq_line() + labs(title = "Normal Q-Q-plot of the residuals") +
       theme(text = element_text(size = 14))

# Histogram of the residuals:
ggplot(data = model1.linear.pred, aes(x = e)) +
       geom_histogram(bins = 10) + xlab("Residuals") +
       labs(title = "Histogram of residuals") +
       theme(text = element_text(size = 14))


### Logarithmic model ###

# Add the residuals to the predicted data
model2.log.pred$e <- model2.log$residuals
head(model2.log.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max2.e <- max(abs(model2.log.pred$e)))
(model2.log.pred.elims <- c(-max2.e, max2.e))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = model2.log.pred, aes(x = age, y = e)) + geom_point(size = 2) +
       geom_hline(yintercept = 0) + expand_limits(y = model2.log.pred.elims) +
       xlab("År") + ylab("Residual") + labs(title = "Residuals vs x-values") +
       theme(text = element_text(size = 14))

# Plot residuals against y-hat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.
ggplot(data = model2.log.pred, aes(x = fit, y = e)) + geom_point(size = 2) +
       geom_hline(yintercept = 0) + expand_limits(y = model2.log.pred.elims) +
       xlab("Predicted betakaroten") + ylab("Residual") +
       labs(title = "Residuals vs predicted values Y-hat") +
       theme(text = element_text(size = 14))

# Make a normal qq-plot of the residuals.
ggplot(data = model2.log.pred, aes(sample = e)) + geom_qq(size = 2) +
       geom_qq_line() + labs(title = "Normal Q-Q-plot of the residuals") +
       theme(text = element_text(size = 14))

# Histogram of the residuals:
ggplot(data = model2.log.pred, aes(x = e)) + geom_histogram(bins = 10) +
       xlab("Residuals") + labs(title = "Histogram of residuals") +
       theme(text = element_text(size = 14))


### Choosing logarithmic model! ###
# Calculate the confidence intervals for beta:
(conf <- confint(model2.log))
model2.log$coefficients
exp(conf)
exp(model2.log$coefficients)

# Predict for ages 25, 26, 75, 76
(x0.ages <- data.frame(age = c(25, 26, 75, 76)))
(x0.ages.pred <- cbind(x0.ages, 
                        fit = exp(predict(model2.log, x0.ages)),
                        conf = exp(predict(model2.log, x0.ages, 
                                           interval = "confidence")),
                        pred = exp(predict(model2.log, x0.ages, 
                                           interval = "prediction"))))

x0.ages.pred$interval_width <- x0.ages.pred$conf.upr - x0.ages.pred$conf.lwr

# Display interval ages and corresponding interval width
cbind(x0.ages.pred["age"], x0.ages.pred["interval_width"]) 
