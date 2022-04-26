# Constants
save.images2 <- FALSE

##### Part A #####
# Turning the categorical variables into factors
data$sex <- factor(data$sex,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))

data$smokstat <- factor(data$smokstat,
                     levels = c(1, 2, 3),
                     labels = c("Never", "Former", "Current Smoker"))

data$bmicat <- factor(data$bmicat,
                     levels = c(1, 2, 3, 4),
                     labels = c("Underweight", "Normal", "Overweight", "Obese"))

# Frequency table for each categorical variable
table(data$sex)
table(data$smokstat)
table(data$bmicat)


##### Part B #####
# Create model
model3.log <- lm(log(data$betaplasma) ~ bmicat, data = data)

# Display model-properties
summary(model3.log)

# Change reference level in BMI category
data$bmicat <- relevel(data$bmicat, "Normal")
model3.log <- lm(log(betaplasma) ~ bmicat, data = data)
summary(model3.log)

# Change reference level in sex category
data$sex <- relevel(data$sex, "Female")

##### Part C ######
# TODO - switch approach and directly write table in R with relevant data
# Introduce new model with all parameters
model4.log.full <- lm(log(betaplasma) ~ I(age - minage) + sex + smokstat + 
                      bmicat, data = data)
model4.log.full$coefficients
(
  betas <- data.frame(beta = model4.log.full$coefficients, 
                     exp.beta = exp(model4.log.full$coefficients),
                     lower.boundary = exp(confint(model4.log.full))[,1],
                     upper.boundary = exp(confint(model4.log.full)[,2]))
)

### Global F-test - C.1 ###
(model4.log.full.sum <- summary(model4.log.full))
(model4.log.full.fstat <- model4.log.full.sum$fstatistic)
(qf(1 - 0.025, 7, 306, lower.tail=TRUE))

### Partial F-test - C.2 ###
(model2.log.model4.full.log.anova <- anova(model2.log, model4.log.full))

# Collect F-value, compute quantiles and p-value
(Fvalue <- model2.log.model4.full.log.anova$F[2])
(ref <- qf(1 - 0.025, 6, 306))
(pf(Fvalue, 6, 306, lower.tail = FALSE))

### Global F-test - C.3 ###
summary(model4.log.full)
confint(model4.log.full)

# Age
model4.red.age <- lm(log(betaplasma) ~ sex + smokstat + bmicat, data = data)
(model4.red.age.anova <- anova(model4.red.age, model4.log.full))

# Collect F-value, compute quantiles and p-value
(model4.red.age.fvalue <- model4.red.age.anova$F[2])
(model4.red.age.ref <- qf(1 - 0.025, 1, 306))

# Sex
model4.red.sex <- lm(log(betaplasma) ~ I(age - minage) + smokstat + bmicat,
                     data = data)
(model4.red.sex.anova <- anova(model4.red.sex, model4.log.full))

# Collect F-value, compute quantiles and p-value
(model4.red.sex.fvalue <- model4.red.sex.anova$F[2])
(model4.red.sex.ref <- qf(1 - 0.025, 6, 306))

# Smoke status
model4.red.smokstat <- lm(log(betaplasma) ~ I(age - minage) + sex + bmicat, 
                          data = data)
(model4.red.smokstat.anova <- anova(model4.red.smokstat, model4.log.full))

# Collect F-value, compute quantiles and p-value
(model4.red.smokstat.fvalue <- model4.red.smokstat.anova$F[2])
(model4.red.smokstat.ref <- qf(1 - 0.025, 2, 306))

# BMI category
model4.red.bmicat <- lm(log(betaplasma) ~ I(age - minage) + sex + smokstat, 
                        data = data)
(model4.red.bmicat.anova <- anova(model4.red.bmicat, model4.log.full))

# Collect F-value, compute quantiles and p-value
(model4.red.bmicat.fvalue <- model4.red.bmicat.anova$F[2])
(model4.red.bmicat.ref <- qf(1 - 0.05, 3, 306))

# Display results
(model4.removed.betas <- data.frame(
  row.names = c("Red. age category", "Red. sex category", "Red. smokstat",
                "Red. BMI category"),
  fvalue = c(model4.red.age.fvalue, model4.red.sex.fvalue, 
           model4.red.smokstat.fvalue, model4.red.bmicat.fvalue),
  upper.quantile = c(model4.red.age.ref, model4.red.sex.ref, 
                   model4.red.smokstat.ref, model4.red.bmicat.ref)))


### T-test for underweight - C.4 ###
model4.log.full$coefficients

# BMI category underweight: H0 --> beta_bmicatUnderweight = 0
(underweight.tstat <- model4.log.full.sum$coefficients["bmicatUnderweight",
                                                       "t value"])
(underweight.comp <- data.frame("Lower qt." = qt(0.05/2, 306),
                                 "T-stat" = underweight.tstat,
                                 "Upper qt." = qt(1 - 0.05/2, 306)))

# P-value:
# 2*P(|t| > |tvalue|) to cover both tails:
2*pt(underweight.tstat, 306, lower.tail = FALSE)

##### Part D #####
# Make predictions with the new model
model4.log.full.pred <- cbind(data, 
                         fit = predict(model4.log.full),
                         conf = predict(model4.log.full, 
                                        interval = "confidence"),
                         pred = predict(model4.log.full, 
                                        interval = "prediction"))
head(model4.log.full.pred)

# Plot predictions vs data
(
  plot8.data <- ggplot(data = model4.log.full.pred, 
                      aes(x = age, y = log(betaplasma), color = sex)) + 
               geom_point(size = 2) + 
               geom_hline(yintercept = mean(log(data$betaplasma))) + 
               xlab("age") + ylab("log(beta-carotene)") + 
               labs(title = "Age and log(beta-carotene)") +
               theme(text = element_text(size = 12)) +
               facet_grid(smokstat ~ relevel(bmicat, "Underweight"))
)

average_age <- mean(data$age) - minage

# Add the fitted line to the data plot
(
  plot8.line <- plot8.data + 
    geom_line(aes(y = fit, group=sex), size = 0.5)
)

# Add confidence interval
(
  plot8.conf <- plot8.data + 
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr, group=sex), alpha = 0.2)
)

# Add prediction interval
(
 plot8.full <- plot8.conf + geom_line(aes(y = pred.lwr, group=sex), 
                                      linetype = "dashed", size = 0.5) + 
               geom_line(aes(y = pred.upr, group=sex),  linetype = "dashed",
                         size = 0.5)
)
if(save.images2) {
  ggsave(file="AgeVsLogBetaCaroteneSplit.png", path="./Images/Part 2/")
}
# Beta estimates
model4.log.full$coefficients

##### Part E #####

model5.log.full <- lm(log(betaplasma) ~ I(age - minage) + sex + smokstat + 
                        quetelet, data = data)

parametersMale5 <- data.frame(
  "age" = 40 - minage,
  "sex" = "Male",
  "smokstat" = "Current Smoker",
  "quetelet" = 22
)

parametersFemale5 <- data.frame(
  "age" = 40 - minage,
  "sex" = "Female",
  "smokstat" = "Current Smoker",
  "quetelet" = 22
)
parametersMale4 <- data.frame(
  "age" = 40 - minage,
  "sex" = "Male",
  "smokstat" = "Current Smoker",
  "bmicat" = "Normal"
)
parametersFemale4 <- data.frame(
  "age" = 40 - minage,
  "sex" = "Female",
  "smokstat" = "Current Smoker",
  "bmicat" = "Normal"
)
(
  resultMale5 <- predict(model5.log.full, newdata = parametersMale5, interval = "confidence")
)

(
  resultFemale5 <- predict(model5.log.full, newdata = parametersFemale5, interval = "confidence")
)

(
  resultMale4 <- predict(model4.log.full, newdata = parametersMale4, interval = "confidence")
)
(
  resultFemale4 <- predict(model4.log.full, newdata = parametersFemale4, interval = "confidence")
)

## Part 5 ##

# Use both bmicat and quetelet in the model, ie

model6.log.full <- lm(log(betaplasma) ~ I(age - minage) + sex + smokstat + 
                        quetelet + bmicat, data = data)



