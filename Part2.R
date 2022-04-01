

# turning the categorical variables into factors
newplasma$sex <- factor(newplasma$sex,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))

newplasma$smokstat <- factor(newplasma$smokstat,
                     levels = c(1, 2, 3),
                     labels = c("Never", "Former", "Current Smoker"))

newplasma$bmicat <- factor(newplasma$bmicat,
                     levels = c(1, 2, 3, 4),
                     labels = c("Underweight", "Normal", "Overweight", "Obese"))

table(newplasma$sex)
table(newplasma$smokstat)
table(newplasma$bmicat)

# Create model and change reference region
newplasma.model3 <- lm(log(betaplasma) ~ I(age - minage) + bmicat, 
                       data = newplasma)

summary(newplasma.model3)

# Change reference category
newplasma$bmicat <- relevel(newplasma$bmicat, "Normal")
newplasma.model3 <- lm(log(betaplasma) ~ I(age - minage) + bmicat,
                       data = newplasma)
summary(newplasma.model3)

newplasma$sex <- relevel(newplasma$sex, "Female")

# New model with all parameters
newplasma.model4 <- lm(log(betaplasma) ~ I(age - minage) +
                         sex + smokstat + bmicat, data = newplasma)

(betas <- data.frame(beta=newplasma.model4$coefficients, 
                     exp.beta=exp(newplasma.model4$coefficients),
                     lower.boundary=exp(confint(newplasma.model4))[,1],
                     upper.boundary=exp(confint(newplasma.model4)[,2])))

# Various tests
# Global F-test for testing the relevance of the full model
(newplasma.model4.summary <- summary(newplasma.model4))


# Partial F-test to see whether categorical variables contributes to model 
# precision
(newplasma.anova <- anova(newplasma.model2, newplasma.model4))
(Fvalue <- newplasma.anova$F[2])
qf(1 - 0.05, 6, 306)

# Calculate P-value:
pf(Fvalue, 6, 306, lower.tail = FALSE)
newplasma.anova$`Pr(>F)`[2]

# --> Reject H0

# Check significance levels among betas
confint(newplasma.model4)

# Former smoker + underweight beta intervals include 0
# --> test model without the variables
newplasma.model5 <- lm(log(betaplasma) ~ I(age - minage) +
                         sex + smokstat + bmicat, data = newplasma)

