##### Part 1a #####
### Imports ###
library(ggplot2)

### Constants ###
SAVE.IMAGES <- TRUE

# Load data
df <- read.delim("Data/hospital.txt", sep = ";")
head(df)

# Use factors for hospital and health category
df$hosp_cat <- factor(df$hosp, levels = c(0, 1),
                      labels = c("0 days", "1+ days"))

df$health_cat <- factor(df$health, levels = c(1, 2, 3), labels = c("good", 
                                                          "bad", "between"))

# Create dataset to modify
df2 <- data.frame(df)

# Frequency and proportion table
table(df$health_cat, df$hosp)
prop.table(table(df$health_cat, df$hosp_cat), margin = 1)

# Relevel good to the reference category
df$health_cat <- relevel(df$health_cat, ref = "good")

# Create binomial model
model1.glm <- glm(hosp ~ health_cat, family = "binomial", data = df)
summary(model1.glm)

# Log odds + odds
model1.glm$coefficients
(model1.glm.log.ci.beta <- confint(model1.glm))

(model1.glm.log.ci.or <- exp(model1.glm.log.ci.beta))
(model1.glm.or <- exp(model1.glm$coefficients))

# McFadden, AIC, BIC
model0.null <- glm(hosp ~ 1, family = "binomial", data = df)
(1 - logLik(model1.glm)/logLik(model0.null))
(AIC(model1.glm))
(BIC(model1.glm))

# Test the model against the null model
(sum.model1.glm <- summary(model1.glm))
(model1.glm.dd <- sum.model1.glm$null.deviance - sum.model1.glm$deviance)
(model1.glm.df_diff <- sum.model1.glm$df.null - sum.model1.glm$df.residual)

# Compare with Null model using the anova funktion:
(anova.1a <- anova(model0.null, model1.glm))
(D_diff <- anova.1a$Deviance[2])
(df_diff <- anova.1a$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, df_diff)
# or P-value:
pchisq(D_diff, df_diff, lower.tail = FALSE)

# Use the model to calculate the predicted probabilities of having at least 
# one day in hospital for each of the three health categories, with 95 % 
# confidence intervals. Compare with the proportions calculated from the 
# cross-tabulation.

df.pred <- cbind(
  df,
  phat = predict(model1.glm, type = "response")
)

df.pred <- cbind(
  df.pred,
  logit = predict(model1.glm, se.fit = TRUE))

# Assert redundant variable to null
df.pred$logit.residual.scale <- NULL

# CI for log-odds
(lambda <- qnorm(1 - 0.05/2))
df.pred$logit.lwr <- df.pred$logit.fit - lambda * df.pred$logit.se.fit
df.pred$logit.upr <- df.pred$logit.fit + lambda*df.pred$logit.se.fit
head(df.pred)

# Transform the log-odds intervals into C.I. for odds
df.pred$odds.lwr <- exp(df.pred$logit.lwr)
df.pred$odds.upr <- exp(df.pred$logit.upr)
head(df.pred)

# Transform the odds intervals into C.I. for p
df.pred$p.lwr <- df.pred$odds.lwr/(1 + df.pred$odds.lwr)
df.pred$p.upr <- df.pred$odds.upr/(1 + df.pred$odds.upr)
head(df.pred)

# Print health status, probability, and C.Is
head(unique(df.pred[c("health_cat", "phat", "p.lwr", "p.upr")]))

df <- df2

##### ---------- Part 1b ---------- #####
# Plotting hosp against age, with moving average
ggplot(df, aes(age, hosp)) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE, linetype = "dashed") +
  xlab("age") +
  ylab("hospital days") +
  labs(title = "1+ hospital days (=1) or No hospital days (=0) vs age",
       caption = "blue dashed = moving average") +
  theme(text = element_text(size = 14))

# Save
if (SAVE.IMAGES) ggsave(filename = "1b1.png",
                        path="./Images/Part 1/")

# Creating a simple logaritmic model and calculating betas and their confidence 
# intervals
model2 <- glm(hosp ~ I(age), family = "binomial", data = df)
model2$coefficients
confint(model2)
exp(model2$coefficients)
exp(confint(model2))

# McFadden, AIC & BIC
(1 - logLik(model2)/logLik(model0.null))
(AIC(model2))
(BIC(model2))

# Wald's test for the age variable
summary(model2)$coefficients

# Compute change in odds for the age changes
(beta2 <- model2$coefficients[2])
age.changes <- cbind(1, 5)

# Changes
(lambda <- qnorm(1 - 0.05/2))
(odds.logchange <- (beta2 * age.changes)) # Log chanage
(odds.change <- exp(beta2)^age.changes) # Change
(odds.logci <- data.frame(
  "Lwr" = 0,
  "Upr" = 0
  )
)

##### Part 1c #####

#Creating new model with square term
model3 <- glm(hosp ~ age + I(age^2), family = "binomial", data = df)

#calculating betas and their confidence intervals
model3$coefficients
confint(model3)
exp(model3$coefficients)
exp(confint(model3))

# McFadden, AIC & BIC
(1 - logLik(model3)/logLik(model0.null))
(AIC(model3))
(BIC(model3))

#Test the model


##### Part 2a #####

df$sex_cat <- factor(df$sex, levels = c(1, 2), labels = c("male", "female"))
df$civilst_cat <- factor(df$civilst, levels = c(1, 2, 3, 4), labels = 
          c("unmarried", "married", "divorsed/separated", "widow/widower"))
df$exercise_cat <- factor(df$exercise, levels = c(0, 1, 2, 3, 4), labels = 
                        c("no exercise", 
                        "exercises sometimes", 
                        "exercises once a week", 
                        "exercises twice a week", 
                        "exercises > twice a week"))
df$work_norm_cat <- factor(df$work_norm, levels = c(1, 2, 3, 4, 5), labels = 
                             c("employed 1-19 hours", 
                               "employed 20-34 hours", 
                               "employed 35–97 hours", 
                               "farmer or self-employed", 
                               "other, does not work"))

#Frequency tables
table(df$sex_cat)
table(df$civilst_cat)
table(df$exercise_cat)
table(df$work_norm_cat)

#Choosing reference variables
df$sex_cat <- relevel(df$sex_cat, ref = "female")
df$civilst_cat <- relevel(df$civilst_cat, ref = "married")
df$exercise_cat <- relevel(df$exercise_cat, ref = "exercises sometimes")
df$work_norm_cat <- relevel(df$work_norm_cat, ref = "other, does not work")


##### Part 2b #####

#Plot

##### Part 2c #####

#Creating the full model, including categorical variables
model4.full <- glm(hosp ~ age + I(age^2) + sex_cat + civilst_cat + exercise_cat +
             work_norm_cat + inc_hh + inc_tot, family = "binomial", data = df)

# McFadden, AIC & BIC
(1 - logLik(model4.full)/logLik(model0.null))
(AIC(model4.full))
(BIC(model4.full))

#Removing one variable at a time for testing
model4.full.age <- update(model4.full, . ~ . -age -I(age^2))
model4.full.sex <- update(model4.full, . ~ . -sex_cat)
model4.full.civist <- update(model4.full, . ~ . -civilst_cat)
model4.full.exercise <- update(model4.full, . ~ . -exercise_cat)
model4.full.work_norm <- update(model4.full, . ~ . -work_norm_cat)
model.inc_hh <- update(model4.full, . ~ . -inc_hh)
model.inc_tot <- update(model4.full, . ~ . -inc_tot)


##### Part 2d #####
# Stepwise AIC selection
# starting with null model
model5.aic <- step(model0.null, 
     scope = list(lower = model0.null, upper = model4.full),
     direction = "both",
     k = 2)

#What happens to work_norm?
#Boxplot of work_norm against age
ggplot(df, aes(work_norm_cat, age)) +
  geom_boxplot() +
  xlab("working hours") +
  ylab("age") +
  theme(text = element_text(size = 14))

#calculating betas and their confidence intervals
model5.aic$coefficients
confint(model5.aic)
exp(model5.aic$coefficients)
exp(confint(model5.aic))

# McFadden, AIC & BIC
(1 - logLik(model5.aic)/logLik(model0.null))
(AIC(model5.aic))
(BIC(model5.aic))

#Test against the full model
#Partial likelihood test?



##### Part 2e #####
# Stepwise BIC selection
# starting with null model
model6.bic <- step(model0.null, 
               scope = list(lower = model0.null, upper = model4.full),
               direction = "both",
               k = log(nrow(df)))

#calculating betas and their confidence intervals
model6.bic$coefficients
confint(model6.bic)
exp(model6.bic$coefficients)
exp(confint(model6.bic))

# McFadden, AIC & BIC
(1 - logLik(model6.bic)/logLik(model0.null))
(AIC(model6.bic))
(BIC(model6.bic))




##### Part 3a #####

##### Part 3b #####

##### Part 3c #####

##### Part 3d #####

##### Part 4a #####

##### Part 4b #####

##### Part 4c #####

##### Part 4d #####

##### Part 4e #####