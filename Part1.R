# Import ggplot library
library(ggplot2)

# Read data file
plasma <- read.delim("Data/plasma.txt")
head(plasma)
summary(plasma)

# Delete data point with betaplasma=0
newplasma <- plasma[plasma$betaplasma > 0, ]

# Find minimum age
minage <- min(newplasma$betaplasma)

###### Linear model ######
newplasma.model1 <- lm(betaplasma ~ I(age - minage), data = newplasma)
summary(newplasma.model1)

# Fit a line
newplasma.pred1 <- 
  cbind(newplasma, 
        fit = predict(newplasma.model1),
        conf = predict(newplasma.model1, interval = "confidence"))
        # pred = predict(newplasma.model1, interval = "prediction"))
head(newplasma.pred1)
# get rid of the extra fits
newplasma.pred1$conf.fit <- newplasma.pred1$pred.fit <- NULL
head(newplasma.pred1)

# Plot beta-carotene against age
(
  plot.data1 <- 
    ggplot(data = newplasma.pred1, aes(x = age, y = betaplasma)) + 
    geom_point(size = 2) +
    xlab("Ålder") +
    ylab("Betakaroten") +
    labs(title = "Ålder och betakaroten") +
    theme(text = element_text(size = 14))
)

# Add the fitted line to the data plot
(
  plot.line1 <- plot.data1 + 
    geom_line(aes(y = fit), color = "blue", size = 1) +
    labs(caption = "data and fitted line")
)

###### Logarithmic model ######
newplasma.model2 <- lm(log(betaplasma) ~ I(age - minage), data = newplasma)

# Fit a log line
newplasma.pred2 <- 
  cbind(newplasma, 
        fit = predict(newplasma.model2),
        conf = predict(newplasma.model2, interval = "confidence"))
        # pred = predict(newplasma.model2, interval = "prediction"))
head(newplasma.pred2)
# get rid of the extra fits
# newplasma.pred1$conf.fit <- newplasma.pred2$pred.fit <- NULL
head(newplasma.pred2)

# Plot log(beta-carotene) against age
(
  plot.data2 <- 
    ggplot(data = newplasma.pred2, aes(x = age, y = log(betaplasma))) + 
    geom_point(size = 2) +
    xlab("Ålder") +
    ylab("log(Betakaroten)") +
    labs(title = "Ålder och log(betakaroten)") +
    theme(text = element_text(size = 14))
)
# Add the fitted line to the data plot
(
  plot.line2 <- plot.data2 + 
    geom_line(aes(y = fit), color = "blue", size = 1) +
    labs(caption = "data and fitted line")
)


###### Residual Analysis ######
### Linear model ###

# Add the residuals to the predicted data
newplasma.pred1$e <- newplasma.model1$residuals
head(newplasma.pred1)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max1.e <- max(abs(newplasma.pred1$e)))
(newplasma.elims1 <- c(-max1.e, max1.e))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = newplasma.pred1, 
       aes(x = age, y = e)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  expand_limits(y = newplasma.elims1) +
  xlab("År") +
  ylab("Residual") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 14))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = newplasma.pred1, aes(x = fit, y = e)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  expand_limits(y = newplasma.elims1) +
  xlab("Predicted betakaroten") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 14))

# Make a normal qq-plot of the residuals.
ggplot(data = newplasma.pred1, aes(sample = e)) +
  geom_qq(size = 2) +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 14))

# Histogram of the residuals:
ggplot(data = newplasma.pred1, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 14))




### Logarithmic model ###

# Add the residuals to the predicted data
newplasma.pred2$e <- newplasma.model2$residuals
head(newplasma.pred2)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max2.e <- max(abs(newplasma.pred2$e)))
(newplasma.elims2 <- c(-max2.e, max2.e))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = newplasma.pred2, 
       aes(x = age, y = e)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  expand_limits(y = newplasma.elims2) +
  xlab("År") +
  ylab("Residual") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 14))

# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = newplasma.pred2, aes(x = fit, y = e)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  expand_limits(y = newplasma.elims2) +
  xlab("Predicted betakaroten") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 14))

# Make a normal qq-plot of the residuals.
ggplot(data = newplasma.pred2, aes(sample = e)) +
  geom_qq(size = 2) +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 14))

# Histogram of the residuals:
ggplot(data = newplasma.pred2, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 14))

## Choosing logarithmic model!

