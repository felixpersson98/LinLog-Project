##### Part A #####
# Model containing categorical BMI-values
model1 <- lm(log(betaplasma) ~ I(age - minage) + sex + 
               smokstat + bmicat, data = data)

# Model containing continuous BMI-values
model2 <-  lm(log(betaplasma) ~ I(age - minage) + sex + 
                smokstat + quetelet, data = data)

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


ggpairs(data=contx, upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.4,    size=0.2))) + 
        theme(text = element_text(size = 8))
ggsave(filename = "allcontinuousvariables.png", path="./Images/Part 3/")
## HUR GÖR VI DEN LÄSBAR?
# fat/calories and fat/cholesterol have covariance greater than 0.7

# plot fat/calories and fat/cholesterol
ggplot(data = newplasma, aes(x = fat, y = calories)) +
  geom_point(size = 1) +
  xlab("Fat per day (g)") +
  ylab("Calories per day") +
  labs(title = "Fat and Calories") +
  theme(text = element_text(size = 10))

ggplot(data = newplasma, aes(x = fat, y = cholesterol)) +
  geom_point(size = 1) +
  xlab("Fat per day (g)") +
  ylab("Cholesterol per day (mg)") +
  labs(title = "Fat and Cholesterol") +
  theme(text = element_text(size = 10))

# Frequency table of vitamin usage
table(newplasma$vituse)

# Change reference category to No.
newplasma$vituse <- factor(newplasma$vituse,
                           levels = c(1, 2, 3),
                           labels = c("Fairly often", "Not often", "No"))
newplasma$vituse <- relevel(newplasma$vituse, "No")

newplasma.model4 <- lm(log(betaplasma) ~ I(age - minage) + quetelet +
                         calories + fat + fiber + alcohol +
                         cholesterol + betadiet, data = newplasma)

# Leverage
newplasma$v <- influence(newplasma.model4)$hat

# plot leverage against age
ggplot(cbind(newplasma), aes(x = age, y = v)) +
  geom_jitter(width = 1)  +
  geom_hline(yintercept = 1/nrow(newplasma)) +
  geom_hline(yintercept = 2*length(newplasma.model4$coefficients)/nrow(newplasma), 
             color = "red") +
  expand_limits(y = c(-0.001, 0.006)) +
  labs(title = "Leverage vs age") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  xlab("Age (years)") +
  ylab("Leverage")
theme(text = element_text(size = 12))

# leverage against alcohol
ggplot(cbind(newplasma), aes(x = alcohol, y = v)) +
  geom_jitter(width = 1)  +
  geom_hline(yintercept = 1/nrow(newplasma)) +
  geom_hline(yintercept = 2*length(newplasma.model4$coefficients)/nrow(newplasma), 
             color = "red") +
  expand_limits(y = c(-0.001, 0.006)) +
  labs(title = "Leverage vs alcohol") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  xlab("Alcohol consumption (drinks/week)") +
  ylab("Leverage")
theme(text = element_text(size = 12))
