# Libraries
library(gridExtra)

library(ggplot2)
library(GGally)
library(plotly)
library(tidyverse)

# Constants
overwrite.images.3 <- FALSE


##### Part A #####
# Model containing categorical BMI-values
model1 <- lm(log(betaplasma) ~ I(age - minage) + sex + 
               smokstat + bmicat, data = data)
sum1 <- summary(model1)

# Model containing continuous BMI-values
model2 <-  lm(log(betaplasma) ~ I(age - minage) + sex + 
                smokstat + quetelet, data = data)
sum2 <- summary(model2)

(
  comparison.1.2 <- data.frame(
    R2 = c(sum1$r.squared, sum2$r.squared),
    R2.adj = c(sum1$adj.r.squared, sum2$adj.r.squared),
    AIC(model1, model2),
    BIC(model1, model2)
  )
)

background.model <- model1
age.model <- model2.log

##### Part B #####
# Creating data frame with all continuous variables
contx <- data[, c("age", "quetelet", "calories", "fat", "fiber",
                       "alcohol", "cholesterol", "betadiet")]


# Plot each category against each other
ggpairs(data=contx, upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.3,    size=0.2))) +
        theme(
          text = element_text(size = 10), 
          axis.text.x = element_text(angle =45, hjust = 1),
          axis.text.y = element_text(size=9)
        )
if(overwrite.images.3) {
  ggsave(filename = "allcontinuousvariables.png", path="./Images/Part 3/")
}

# Correlation matrix
(correlation.mat <- cor(contx))

# fat/calories and fat/cholesterol have covariance greater than 0.7

# plot fat/calories and fat/cholesterol
ggplot(data = data, aes(x = fat, y = calories)) +
  geom_point(size = 1.5) +
  xlab("Fat per day (g)") +
  ylab("Calories per day") +
  labs(title = "Fat and Calories") +
  theme(text = element_text(size = 12))
if(overwrite.images.3) {
  ggsave(filename = "fatvscalories.png", path="./Images/Part 3/")
}

ggplot(data = data, aes(x = fat, y = cholesterol)) +
  geom_point(size = 1.5) +
  xlab("Fat per day (g)") +
  ylab("Cholesterol per day (mg)") +
  labs(title = "Fat and Cholesterol") +
  theme(text = element_text(size = 12))
if(overwrite.images.3) {
  ggsave(filename = "fatvscholesterol.png", path="./Images/Part 3/")
}


head(data)
# Frequency table of vitamin usage

# Change reference category to No.
data$vituse <- factor(data$vituse,
                           levels = c(1, 2, 3),
                           labels = c("Fairly often", "Not often", "No"))
table(data$vituse)
data$vituse <- relevel(data$vituse, ref="No")


##### Part C #####
data.model4 <- lm(log(betaplasma) ~ I(age - minage) + quetelet +
                         calories + fat + fiber + alcohol +
                         cholesterol + betadiet, data = data)


# Leverage & yhat
data$v <- influence(data.model4)$hat
data$yhat <- predict(data.model4)

# Leverage vs Y-hat
ggplot(cbind(data), aes(x = predict(data.model4), y = v)) +
  geom_jitter(width = 1)  +
  geom_hline(yintercept = 1/nrow(data)) +
  geom_hline(
    yintercept = 2*length(data.model4$coefficients)/nrow(data),
    color = "red"
  ) + expand_limits(y = c(-0.001, 0.006)) + labs(title = "Leverage vs y-hat") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") + xlab("Y-hat (ng/ml)") +
  ylab("Leverage") + theme(text = element_text(size = 12))
if(overwrite.images.3) {
  ggsave(filename = "leveragevsyhat.png", path="./Images/Part 3/")
}

# plot leverage against age
ggplot(cbind(data), aes(x = age, y = v)) +
  geom_jitter(width = 1)  +
  geom_hline(yintercept = 1/nrow(data)) +
  geom_hline(
    yintercept = 2*length(data.model4$coefficients)/nrow(data),
    color = "red"
  ) + expand_limits(y = c(-0.001, 0.006)) + labs(title = "Leverage vs age") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") + xlab("Age (years)") +
  ylab("Leverage") + theme(text = element_text(size = 12))
if(overwrite.images.3) {
  ggsave(filename = "leveragevsage.png", path="./Images/Part 3/")
}

# leverage against alcohol
ggplot(cbind(data), aes(x = alcohol, y = v)) +
  geom_jitter(width = 1)  + geom_hline(yintercept = 1/nrow(data)) +
  geom_hline(
    yintercept = 2*length(data.model4$coefficients)/nrow(data), 
    color = "red"
  ) + expand_limits(y = c(-0.001, 0.006)) + 
  labs(title = "Leverage vs alcohol") + 
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  xlab("Alcohol consumption (drinks/week)") + ylab("Leverage") + 
  theme(text = element_text(size = 12))
if(overwrite.images.3) {
  ggsave(filename = "leveragevsalcohol.png", path="./Images/Part 3/")
}

# Check whether it is a good idea to take the logarithm of alcohol consumption
(
  plot11 <- ggplot(data=data, mapping=aes(x = yhat, y = alcohol)) 
  + geom_point(size=2) + geom_hline(yintercept = mean(data$alcohol), 
                                    color="orange")
  + xlab("Y-hat") + ylab("Alcohol consumption (drinks/week)")
)
if(overwrite.images.3) {
  ggsave("AlcoholConsumtionvsYhat.png", path="./Images/Part 3/")
}

# Print the amount of non-drinkers
length(data$alcohol[data$alcohol == 0])

# Plot alcohol consumption against various other variables
# FIXME 
# Hade inte förstått här hur man får ihop flera GGplots i en bild, så körde
# den traditionella approachen
if(overwrite.images.3) {
  png(filename="./Images/Part 3/AlcoholvsVariouscategories.png", 
      width = 600, height = 480, units = "px", pointsize = 12, bg = "white")
  
  par(mfrow= c(2, 2))
  plot(data$age, data$alcohol, xlab="Age", ylab="Alcohol consumption")
  plot(data$quetelet, data$alcohol, xlab="Quetelet", ylab="Alcohol consumption")
  plot(data$calories, data$alcohol, xlab="Calories", ylab="Alcohol consumption")
  plot(data$betaplasma, data$alcohol, xlab="Betaplasma", ylab="Alcohol consumption")
  mtext("Alcohol consumption vs various variables", side = 3, line = -2,
        outer = TRUE)
  dev.off()
}

##### Part D #####
# QQplot for residuals
data$yhat <- predict(data.model4)
data$r <- resid(data.model4)
data$rs <- rstudent(data.model4)
(
  plot.resid.qq <- ggplot(data, aes(sample = r)) + 
    geom_qq(size = 2) + geom_qq_line() + 
    labs(title = "Normal Q-Q-plot of the residuals") +
    xlab("Normal quantiles") + ylab("Residual quantiles") +
    theme(text = element_text(size = 14), plot.title = element_text(size=14))
)
(
  plot.resid.studentized <- ggplot(data=data, aes(x = yhat, y = rs))
    + geom_point(size=2) + geom_hline(yintercept = c(-3, -2, 0, 2, 3)) 
    + labs(title = "Residuals vs Y-hat") + theme(text = element_text(size = 12))
    + xlab("Y-hat") + ylab("Studentized residuals")
)

if(overwrite.images.3){
  ggsave(filename = "residualsdietarymodel.png", path="./Images/Part 3/",
         arrangeGrob(plot.resid.qq, plot.resid.studentized, ncol=2))
}

# Plot studentized residuals vs alcohol and calories where outlier showed
(plot.rs.c.alc <- plot_ly(x=data$alcohol, y=data$calories, z=data$rs, type="scatter3d",
                   mode="markers")%>%
                   layout(
                   title = "Residuals vs alcohol consumption and calories",
                   scene = list(
                     xaxis = list(title = "Alcohol consumption"), 
                     yaxis = list(title = "Calories" ),  
                     zaxis = list(title = "Studentized residuals")
  )))




