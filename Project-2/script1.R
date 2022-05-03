##### Part 1a #####
### Imports ###
library(ggplot2)

# Load data
df <- read.delim("Data/hospital.txt", sep = ";")
head(df)

# Use factors for hospital and health category
df$hosp_cat <- factor(df$hosp, levels = c(0, 1),
                      labels = c("0 days", "1+ days"))

df$health_cat <- factor(df$health, levels = c(1, 2, 3), labels = c("good", 
                                                          "bad", "between"))

# Frequency and proportion table
table(df$health_cat, df$hosp)
prop.table(table(df$health_cat, df$hosp_cat), margin = 1)

# Relevel
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
model.null <- glm(hosp ~ 1, family = "binomial", data = df)
(1 - logLik(model1.glm)/logLik(model.null))
(AIC(model1.glm))
(BIC(model1.glm))

### Från föreläsningar
# Test the model against the null model
(sum.model1.glm <- summary(model1.glm))
(model1.glm.dd <- sum.model1.glm$null.deviance - sum.model1.glm$deviance)
(model1.glm.df_diff <- sum.model1.glm$df.null - sum.model1.glm$df.residual)

# compare with Null model using the anova funktion:
(anova.1a <- anova(model.null, model1.glm))
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



##### Part 1b #####

#Plotting hosp against age, with moving average
ggplot(df, aes(age, hosp)) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE, linetype = "dashed") +
  xlab("age") +
  ylab("hospital days") +
  labs(title = "1+ hospital days (=1) or No hospital days (=0) vs age",
       caption = "blue dashed = moving average") +
  theme(text = element_text(size = 14))

#Creating a simple logaritmic model and calculating betas and their confidence 
#intervals
model2 <- glm(hosp ~ I(age), family = "binomial", data = df)
model2$coefficients
confint(model2)
exp(model2$coefficients)
exp(confint(model2))

# McFadden, AIC & BIC
(1 - logLik(model2)/logLik(model.null))
(AIC(model2))
(BIC(model2))

#Test the model


##### Part 1c #####

#Creating new model with square term
model3 <- glm(hosp ~ age + I(age^2), family = "binomial", data = df)

#calculating betas and their confidence intervals
model3$coefficients
confint(model3)
exp(model3$coefficients)
exp(confint(model3))

# McFadden, AIC & BIC
(1 - logLik(model3)/logLik(model.null))
(AIC(model3))
(BIC(model3))

#Test the model


##### Part 2a #####
