##### Part 1a #####
### Imports ###
library(ggplot2)
library(GGally)
library(pROC)

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
(wald1b <- data.frame(summary(model2)$coefficients))

# Compute change in odds for the age changes
(beta2 <- model2$coefficients[2])
(beta2.se <- wald1b[2, "Std..Error"])
age.changes <- cbind(1, 5)

# Changes
(lambda <- qnorm(1 - 0.05/2))
(odds.logchange <- (beta2 * age.changes)) # Log chanage
(odds.change <- exp(beta2)^age.changes) # Change
(odds.logci <- data.frame(
  "Lwr" = exp(beta2 - lambda * beta2.se)^age.changes ,
  "Upr" = exp(beta2 + lambda * beta2.se)^age.changes
  )
)

# Plot age predictions for age model
df.pred <- cbind(
  df,
  phat = predict(model2, type = "response")
)

df.pred <- cbind(
  df.pred,
  logit = predict(model2, se.fit = TRUE))

# Assert redundant variable to null
df.pred$logit.residual.scale <- NULL

# CI for log-odds
(lambda <- qnorm(1 - 0.05/2))
df.pred$logit.lwr <- df.pred$logit.fit - lambda * df.pred$logit.se.fit
df.pred$logit.upr <- df.pred$logit.fit + lambda * df.pred$logit.se.fit
head(df.pred)

# Transform the log-odds intervals into C.I. for odds
df.pred$odds.lwr <- exp(df.pred$logit.lwr)
df.pred$odds.upr <- exp(df.pred$logit.upr)
head(df.pred)

# Transform the odds intervals into C.I. for p
df.pred$p.lwr <- df.pred$odds.lwr/(1 + df.pred$odds.lwr)
df.pred$p.upr <- df.pred$odds.upr/(1 + df.pred$odds.upr)
head(df.pred)

ggplot(df.pred, aes(age, hosp)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Age") +
  ylab("In hospital for 1 + days") +
  labs(title = "1+ hospital days (=1) or No hospital days (=0) vs age with C.I") +
  theme(text = element_text(size = 14), plot.title = element_text(size=20))

# Save
if (SAVE.IMAGES) ggsave(filename = "1b2.png",
                        path="./Images/Part 1/")

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

# Test the model
summary(model3)

# Plot age predictions for age + age^2 model and put in comparison with the
# predictions from the age model
df.pred2 <- cbind(
  df,
  phat = predict(model3, type = "response")
)

df.pred2 <- cbind(
  df.pred2,
  logit = predict(model3, se.fit = TRUE))

# Assert redundant variable to null
df.pred2$logit.residual.scale <- NULL

# CI for log-odds
(lambda <- qnorm(1 - 0.05/2))
df.pred2$logit.lwr <- df.pred2$logit.fit - lambda * df.pred2$logit.se.fit
df.pred2$logit.upr <- df.pred2$logit.fit + lambda * df.pred2$logit.se.fit
head(df.pred2)

# Transform the log-odds intervals into C.I. for odds
df.pred2$odds.lwr <- exp(df.pred2$logit.lwr)
df.pred2$odds.upr <- exp(df.pred2$logit.upr)
head(df.pred2)

# Transform the odds intervals into C.I. for p
df.pred2$p.lwr <- df.pred2$odds.lwr/(1 + df.pred2$odds.lwr)
df.pred2$p.upr <- df.pred2$odds.upr/(1 + df.pred2$odds.upr)
head(df.pred2)

# Insert in original dataframe
df.pred$p2.lwr <- df.pred2$p.lwr
df.pred$p2.upr <- df.pred2$p.upr
df.pred$phat2 <- df.pred2$phat

ggplot(df.pred, aes(age, hosp)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  geom_line(aes(y = phat2), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = p2.lwr, ymax = p2.upr), alpha = 0.2) +
  xlab("Age") +
  ylab("In hospital for 1 + days") +
  labs(title = "1+ hospital days (=1) or No hospital days (=0) vs age with C.I",
       caption="Red line = age model, blue line = age + age squared model") + 
  theme(text = element_text(size = 14), plot.title = element_text(size=20))

# Save
if (SAVE.IMAGES) ggsave(filename = "1c1.png",
                        path="./Images/Part 1/")

# Compute odds ratios
model3$coefficients
age.changes <- cbind(50, 75, 100)
(model3.beta.age <- model3$coefficients[2])
(model3.beta.age2 <- model3$coefficients[3])
(
  model3.odds.ratios <- 
    exp(model3.beta.age + model3.beta.age2 * (2*age.changes + 1))
)

(beta2.se <- wald1b[2, "Std..Error"])
age.changes <- cbind(1, 5)

##### Part 2a #####
df <- df2

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
# Creating data frame with all continuous variables
contx <- df[, c("age", "inc_hh", "inc_tot")]


# Plot each category against each other
ggpairs(data=contx, upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.3,    size=0.2))) +
  theme(
    text = element_text(size = 14), 
    axis.text.x = element_text(angle =45, hjust = 1),
    axis.text.y = element_text(size=9)
  )
if(SAVE.IMAGES) ggsave(filename = "2b.png", 
                       path="./Images/Part 2/")

##### Part 2c #####

# Creating the full model, including categorical variables
model4.full <- glm(hosp ~ age + I(age^2) + sex_cat + health_cat + civilst_cat + exercise_cat +
             work_norm_cat + inc_hh + inc_tot, family = "binomial", data = df)

# McFadden, AIC & BIC
(1 - logLik(model4.full)/logLik(model0.null))
(AIC(model4.full))
(BIC(model4.full))

# Removing one variable at a time for testing
(sum.model4.full <- summary(model4.full))

# Partial Likelihood ratio test for nested model without age variable
model4.age <- update(model4.full, . ~ . -age -I(age^2))

(anova.age <- anova(model4.age, model4.full))
(age.D_diff <- anova.age$Deviance[2])
(age.df_diff <- anova.age$Df[2])

# chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, age.df_diff)
# or P-value:
pchisq(age.D_diff, age.df_diff, lower.tail = FALSE)

####### ---------------------------

# Partial Likelihood ratio test sex
model4.sex <- update(model4.full, . ~ . -sex_cat)

(anova.sex <- anova(model4.sex, model4.full))
(sex.D_diff <- anova.sex$Deviance[2])
(sex.df_diff <- anova.age$Df[2])

# chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, sex.df_diff)
# or P-value:
pchisq(sex.D_diff, sex.df_diff, lower.tail = FALSE)

####### ---------------------------

# Partial Likelihood ratio test health
model4.health <- update(model4.full, . ~ . -health_cat)

(anova.health <- anova(model4.health, model4.full))
(heath.D_diff <- anova.health$Deviance[2])
(health.df_diff <- anova.health$Df[2])

# chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, health.df_diff)
# or P-value:
pchisq(heath.D_diff, health.df_diff, lower.tail = FALSE)

####### ---------------------------

# Partial Likelihood ratio test civil status
model4.civist <- update(model4.full, . ~ . -civilst_cat)

(anova.civist <- anova(model4.civist, model4.full))
(civist.D_diff <- anova.civist$Deviance[2])
(civist.df_diff <- anova.civist$Df[2])

# chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, civist.df_diff)
# or P-value:
pchisq(civist.D_diff, civist.df_diff, lower.tail = FALSE)

####### ---------------------------

# Partial Likelihood ratio test exercise
model4.exercise <- update(model4.full, . ~ . -exercise_cat)

(anova.exercise <- anova(model4.exercise, model4.full))
(exercise.D_diff <- anova.exercise$Deviance[2])
(exercise.df_diff <- anova.exercise$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, exercise.df_diff)
# or P-value:
pchisq(exercise.D_diff, exercise.df_diff, lower.tail = FALSE)

####### ---------------------------

# Partial Likelihood ratio test working hours
model4.work_norm <- update(model4.full, . ~ . -work_norm_cat)
(anova.work_norm <- anova(model4.work_norm, model4.full))
(work_norm.D_diff <- anova.work_norm$Deviance[2])
(work_norm.df_diff <- anova.work_norm$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, work_norm.df_diff)
# or P-value:
pchisq(work_norm.D_diff, work_norm.df_diff, lower.tail = FALSE)

# Wald's test
wald.inc_hh.inc_tot <- summary(model4.full)
wald.inc_hh.inc_tot$coefficients

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
(anova.aic.full <- anova(model5.aic, model4.full))
aic.full.D_diff <- anova.aic.full$Deviance[2]
aic.full.df_diff <- anova.aic.full$Df[2]

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, aic.full.df_diff)
# or P-value:
pchisq(aic.full.D_diff, aic.full.df_diff, lower.tail = FALSE)


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

#Test against the full model
#Partial likelihood test?
(anova.bic.full <- anova(model6.bic, model4.full))
bic.full.D_diff <- anova.bic.full$Deviance[2]
bic.full.df_diff <- anova.bic.full$Df[2]

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, bic.full.df_diff)
# or P-value:
pchisq(bic.full.D_diff, bic.full.df_diff, lower.tail = FALSE)

#The model is nested with the AIC model
#Testing against the AIC model
#Test against the full model
#Partial likelihood test?
(anova.bic.aic <- anova(model6.bic, model5.aic))
bic.aic.D_diff <- anova.bic.aic$Deviance[2]
bic.aic.df_diff <- anova.bic.aic$Df[2]

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, bic.aic.df_diff)
# or P-value:
pchisq(bic.aic.D_diff, bic.aic.df_diff, lower.tail = FALSE)

#Plot predicted probabilities



#Relevel health, bad health = reference category
df$health_cat <- relevel(df$health_cat, ref = "bad")
model6.bic.releveled <- glm(hosp ~ age + I(age^2) + health_cat + sex_cat, 
                            family = "binomial", data = df)
summary(model6.bic.releveled)

# Use factors for health category
df$health_new <- factor(df$health, levels = c(1, 2, 3),
                      labels = c("Good", "Non-good", "Non-good"))

model7.health <- glm(hosp ~ age + I(age^2) + health_new + sex_cat, 
                            family = "binomial", data = df)

#calculating betas and their confidence intervals
model7.health$coefficients
confint(model7.health)
exp(model7.health$coefficients)
exp(confint(model7.health))

# McFadden, AIC & BIC
(1 - logLik(model7.health)/logLik(model0.null))
(AIC(model7.health))
(BIC(model7.health))


##### Part 3a #####

#Plotting leverage against age for all combinations of sex and health status
model7.health.pred <- cbind(df,
                   xb = predict(model7.health),
                   v = influence(model7.health)$hat)
head(model7.health.pred)

(plot.v <- ggplot(model7.health.pred, aes(age, v, color = hosp_cat)) + 
    geom_point() +
    geom_hline(yintercept = 2*length(model7.health$coefficients)/nrow(df), 
               color = "red", size = 1) +
    facet_grid(rows = vars(sex_cat), cols = vars(health_new)) +
    labs(title = "Leverage vs age, by sex and health status",
         caption = "2(p+1)/n in red") +
    theme(text = element_text(size = 14)))

#Highlighting the highest leverage
I.highv <- which(model7.health.pred$v > 0.007)
plot.v +
  geom_point(data = model7.health.pred[I.highv, ], size = 3, 
             color = "red", shape = 24)



##### Part 3b #####

#Standardized deviance residuals
model7.health.pred$devres <- influence(model7.health)$dev.res
model7.health.pred$devstd <- model7.health.pred$devres/
  sqrt(1 - model7.health.pred$v)
head(model7.health.pred)

ggplot(model7.health.pred, aes(xb, devstd, color = as.factor(hosp_cat))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "hospital days") +
  theme(text = element_text(size = 14)) +
  geom_point(data = model7.health.pred[I.highv, ], size = 3, 
             color = "red", shape = 24)


##### Part 3c #####

# Cook's distance####
model7.health.pred$Dcook <- cooks.distance(model7.health)
head(model7.health.pred)

# Plot Cook's distance, highlighting the highest
I.highDcook <- which(model7.health.pred$Dcook > 0.008)
ggplot(model7.health.pred, aes(xb, Dcook, color = as.factor(hosp_cat))) +
  geom_point() +
  geom_point(data = model7.health.pred[I.highDcook, ], size = 3, 
             color = "black", shape = 24) +
  geom_point(data = model7.health.pred[I.highv, ], color = "red",
             shape = 24, size = 3) +
  geom_hline(yintercept = 4/nrow(df), linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor, by sex and health status",
       color = "Y", 
       caption = "4/n in black, high leverage red triangle, 
       high Cook black triangle") +
  theme(text = element_text(size = 14)) +
  facet_grid(rows = vars(health_new), cols = vars(sex_cat))
  

##### Part 3d #####

  #Saving DFBETAS
  model7.health.dfbetas <- dfbetas(model7.health)
  model7.health.pred$dfintercept <- model7.health.dfbetas[, "(Intercept)"]
  model7.health.pred$dfage <- model7.health.dfbetas[, "age"]
  model7.health.pred$dfage2 <- model7.health.dfbetas[, "I(age^2)"]
  model7.health.pred$dfnongoodhealth <- 
    model7.health.dfbetas[, "health_newNon-good"]
  model7.health.pred$dfmale <- model7.health.dfbetas[, "sex_catmale"]
  
  #plotting DFBETAS
  #Intercept
  ggplot(model7.health.pred, aes(age, dfintercept, color = as.factor(hosp_cat))) +
    geom_point() +
    geom_point(data = model7.health.pred[I.highv, ], size = 3, 
               color = "red", shape = 24) +
    geom_point(data = model7.health.pred[I.highDcook, ], size = 3, 
               color = "black", shape = 24) +
    facet_grid(rows = vars(sex_cat), cols = vars(health_new)) +
    labs(title = "DFBETA intercept vs age, by sex and health status") +
    theme(text = element_text(size = 14)) 
  
  #age
  ggplot(model7.health.pred, aes(age, dfage, color = as.factor(hosp_cat))) +
    geom_point() +
    geom_point(data = model7.health.pred[I.highv, ], size = 3, 
               color = "red", shape = 24) +
    geom_point(data = model7.health.pred[I.highDcook, ], size = 3, 
               color = "black", shape = 24) +
    facet_grid(rows = vars(sex_cat), cols = vars(health_new)) +
    labs(title = "DFBETA age vs age, by sex and health status") +
    theme(text = element_text(size = 14)) 
  
  #age^2
  ggplot(model7.health.pred, aes(age, dfage2, color = as.factor(hosp_cat))) +
    geom_point() +
    geom_point(data = model7.health.pred[I.highv, ], size = 3, 
               color = "red", shape = 24) +
    geom_point(data = model7.health.pred[I.highDcook, ], size = 3, 
               color = "black", shape = 24) +
    facet_grid(rows = vars(sex_cat), cols = vars(health_new)) +
    labs(title = "DFBETA age^2 vs age, by sex and health status") +
    theme(text = element_text(size = 14)) 
  
  #non-good health
  ggplot(model7.health.pred, aes(age, dfnongoodhealth, color = as.factor(hosp_cat))) +
    geom_point() +
    geom_point(data = model7.health.pred[I.highv, ], size = 3, 
               color = "red", shape = 24) +
    geom_point(data = model7.health.pred[I.highDcook, ], size = 3, 
               color = "black", shape = 24) +
    facet_grid(rows = vars(sex_cat), cols = vars(health_new)) +
    labs(title = "DFBETA non-good health vs age, by sex and health status") +
    theme(text = element_text(size = 14)) 
  
  #male
  ggplot(model7.health.pred, aes(age, dfmale, color = as.factor(hosp_cat))) +
    geom_point() +
    geom_point(data = model7.health.pred[I.highv, ], size = 3, 
               color = "red", shape = 24) +
    geom_point(data = model7.health.pred[I.highDcook, ], size = 3, 
               color = "black", shape = 24) +
    facet_grid(rows = vars(sex_cat), cols = vars(health_new)) +
    labs(title = "DFBETA male vs age, by sex and health status") +
    theme(text = element_text(size = 14))
  

##### Part 4a #####
  
# Renaming the models
model.health <- model1.glm
model.age_squared <- model3
model.aic <- model5.aic
model.bic3 <- model6.bic
model.bic2 <- model7.health
  
# Estimating p_i for the models
pred.phat <- cbind(
    df,
    p.health = predict(model.health, type = "response"),
    p.age_squared = predict(model.age_squared, type = "response"),
    p.aic = predict(model.aic, type = "response"),
    p.bic3 = predict(model.bic3, type = "response"),
    p.bic2 = predict(model.bic2, type = "response"))
head(pred.phat)
  
# Confusion matrix for model.aic
pred.phat$yhat.aic <- as.numeric(pred.phat$p.aic > 0.5)
(row.01 <- table(df$hosp_cat))
(col.01.aic <- table(pred.phat$yhat.aic))
(confusion.aic <- table(pred.phat$hosp_cat, pred.phat$yhat.aic))
# (sens.aic <- confusion.aic[2, 2] / row.01[2])
(sens.aic <- 0 / row.01[2])
(spec.aic <- confusion.aic[1, 1] / row.01[1])
(accu.aic <- sum(diag(confusion.aic)) / sum(confusion.aic))
#(prec.aic <- confusion.aic[2, 2] / col.01.aic[2])
(prec.aic <- 0 / col.01.aic[2])

# Display values in table
table4a <- data.frame(
  "sens" = sens.aic,
  "spec" = spec.aic,
  "accu" = accu.aic,
  "prec" = prec.aic
)
rownames(table4a) <- "AIC-model"
table4a

##### Part 4b #####
  # ROC-curves
roc.health <- roc(hosp_cat ~ p.health, data = pred.phat)
roc.df.health <- coords(roc.health, transpose = FALSE)
roc.df.health$model <- "health"
roc.age_squared <- roc(hosp_cat ~ p.age_squared, data = pred.phat)
roc.df.age_squared <- coords(roc.age_squared, transpose = FALSE)
roc.df.age_squared$model <- "age_squared"
roc.aic <- roc(hosp_cat ~ p.aic, data = pred.phat)
roc.df.aic <- coords(roc.aic, transpose = FALSE)
roc.df.aic$model <- "aic"
roc.bic3 <- roc(hosp_cat ~ p.bic3, data = pred.phat)
roc.df.bic3 <- coords(roc.bic3, transpose = FALSE)
roc.df.bic3$model <- "bic3"
roc.bic2 <- roc(hosp_cat ~ p.bic2, data = pred.phat)
roc.df.bic2 <- coords(roc.bic2, transpose = FALSE)
roc.df.bic2$model <- "bic2"

  
roc.df <- rbind(roc.df.health, roc.df.age_squared, roc.df.aic, roc.df.bic3, 
                roc.df.bic2)
  
# Plot all the curves, in different colors:
ggplot(roc.df, aes(specificity, sensitivity,
                   color = model)) +
    geom_path(size = 1) +
    coord_fixed() +       # square plotting area
    scale_x_reverse() +   # Reverse scale on the x-axis!
    labs(title = "ROC-curves for the five models") +
    theme(text = element_text(size = 14))

if(SAVE.IMAGES) ggsave(filename="4aRocCurves.png", path="./Images/Part 3")  

# Collecting AUC and intervals for the models
(aucs <- 
    data.frame(
      model = c("health", "age_squared", "aic", "bic3", "bic2"),
      auc = c(auc(roc.health), auc(roc.age_squared), auc(roc.aic), auc(roc.bic3),
              auc(roc.bic2)),
      lwr = c(ci(roc.health)[1], ci(roc.age_squared)[1],
              ci(roc.aic)[1], ci(roc.bic3)[1],
              ci(roc.bic2)[1]),
      upr = c(ci(auc(roc.health))[3], ci(auc(roc.age_squared))[3],
              ci(auc(roc.aic))[3], ci(auc(roc.bic3))[3],
              ci(auc(roc.bic2))[3])))

# Comparing their AUC
roc.test(roc.health, roc.aic)
roc.test(roc.age_squared, roc.aic)
roc.test(roc.bic3, roc.aic)
roc.test(roc.bic2, roc.aic)

##### Part 4c #####
roc.df.aic$sum.spse <- roc.df.aic$specificity + roc.df.aic$sensitivity

# Finding where the sum is the largest 
# Also they should be quite close to each other
roc.df.aic[roc.df.aic$sum.spse > 1.2598, ]
p.new <- 0.2113859

# Collect new confusion matrix
pred.phat <- cbind(
  df,
  p.aic = predict(model.aic, type = "response")
)
head(pred.phat)

# Confusion matrix for model.aic
pred.phat$yhat.aic <- as.numeric(pred.phat$p.aic > p.new)
(row.01 <- table(df$hosp_cat))
(col.01.aic <- table(pred.phat$yhat.aic))
(confusion.aic <- table(pred.phat$hosp_cat, pred.phat$yhat.aic))
(spec.aic <- confusion.aic[1, 1] / row.01[1])
(sens.aic <- confusion.aic[2, 2] / row.01[2])
(accu.aic <- sum(diag(confusion.aic)) / sum(confusion.aic))
(prec.aic <- confusion.aic[2, 2] / col.01.aic[2])

# Display values in table
table4c <- data.frame(
  "sens" = sens.aic,
  "spec" = spec.aic,
  "accu" = accu.aic,
  "prec" = prec.aic
)
rownames(table4c) <- "AIC-model (new p)"


# Compare
table4a
table4c


##### Part 4d #####

##### Part 4e #####