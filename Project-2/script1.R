##### Part 1 #####
### Imports ###
library(ggplot2)

# Load data
df <- read.delim("Data/hospital.txt", sep=";")
head(df)

# Use factors for hospital and health cateogory
df$hosp <- factor(df$hosp, levels = c(0, 1),
                      labels = c("0 days", "1+ days"))

df$health <- factor(df$health, levels = c (1, 2, 3), labels = c("good", "bad",
                                                                "between"))

# Frequency and proportion table
table(df$health, df$hosp)
prop.table(table(df$health, df$hosp), margin = 1)

# Relevel
df$health <- relevel(df$health, ref="good")

# Create binomial model
model1.glm <- glm(hosp ~ health, family = "binomial", data = df)
summary(model1.glm)

# Log odds + odds
model1.glm$coefficients
(model1.glm.log.ci.beta <- confint(model1.glm))

(model1.glm.log.ci.or <- exp(model1.glm.log.ci.beta))
(model1.glm.or <- exp(model1.glm$coefficients))

# McFadden, AIC, BIC
model1.null <- glm(hosp ~ 1, family="binomial", data = df)
(1-logLik(model1.glm)/logLik(model1.null))
(AIC(model1.glm))
(BIC(model1.glm))

### Från föreläsningar
# Test the model against the null model
(sum.model1.glm <- summary(model1.glm))
(model1.glm.dd <- sum.model1.glm$null.deviance - sum.model1.glm$deviance)
(model1.glm.df_diff <- sum.model1.glm$df.null - sum.model1.glm$df.residual)

# compare with Null model using the anova funktion:
(anova.1a <- anova(model1.null, model1.glm))
(D_diff <- anova.1a$Deviance[2])
(df_diff <- anova.1a$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, df_diff)
# or P-value:
pchisq(D_diff, df_diff, lower.tail = FALSE)

