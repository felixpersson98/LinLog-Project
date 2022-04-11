library(ggplot2)
library(GGally)
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
ggsave(filename = "allcontinuousvariables.png", path="./Images/Part 3/")

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
ggsave(filename = "fatvscalories.png", path="./Images/Part 3/")

ggplot(data = data, aes(x = fat, y = cholesterol)) +
  geom_point(size = 1.5) +
  xlab("Fat per day (g)") +
  ylab("Cholesterol per day (mg)") +
  labs(title = "Fat and Cholesterol") +
  theme(text = element_text(size = 12))
ggsave(filename = "fatvscholesterol.png", path="./Images/Part 3/")

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
ggsave(filename = "leveragevsyhat.png", path="./Images/Part 3/")


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
ggsave(filename = "leveragevsage.png", path="./Images/Part 3/")

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
ggsave(filename = "leveragevsalcohol.png", path="./Images/Part 3/")

# TODO Plot alcohol consumption against all other categories
# Alcohol vs all other categories
(
  plot11 <- ggplot(data=data, mapping=aes(x = yhat, y = alcohol, color=sex)) + 
    geom_point(size=1) + facet_grid(~ vituse)
)
