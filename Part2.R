# Import ggplot library
library(ggplot2)

# Read data file
plasma <- read.delim("Data/plasma.txt")

# turning the categorical variables into factors
plasma$sex <- factor(plasma$sex,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))

plasma$smokstat <- factor(plasma$smokstat,
                     levels = c(1, 2, 3),
                     labels = c("Never", "Former", "Current Smoker"))

plasma$bmicat <- factor(plasma$bmicat,
                     levels = c(1, 2, 3, 4),
                     labels = c("Underweight", "Normal", "Overweight", "Obese"))

table(plasma$sex)
table(plasma$smokstat)
table(plasma$bmicat)



plasma$region <- relevel(plasma$region, "all")
plasma.model3 <- lm(log(Pb) ~ I(year - 1975) + region, data = plasma)
(plasma.model3.sum <- summary(plasma.model3))