library(GGally)

# Models from part 1 and 2
sum1 <- summary(model1)
sum2 <- summary(model2)

# Collecting R2, AIC and BIC
(collect.R2s <- data.frame(
  nr = seq(1, 3),
  model = c("first model", "second model"),
  R2 = c(sum1$r.squared,
         sum2$r.squared),
  R2.adj = c(sum1$adj.r.squared,
             sum2$adj.r.squared),
  AIC(model1, model2),
  BIC(model1, model2)))

age_model <- model1
background_model <- model2

# Creating data frame with all continuous variables
contx <- newplasma[, c("age", "quetelet", "calories", "fat", "fiber",
                    "alcohol", "cholesterol", "betadiet")]
ggpairs(contx) + theme(text = element_text(size = 14))
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


