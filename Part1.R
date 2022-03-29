# Import ggplot library
library(ggplot2)

# Read data file
plasma <- read.delim("Data/plasma.txt")
head(plasma)
summary(plasma)

# Delete data point with betaplasma=0
newplasma <- plasma[plasma$betaplasma > 0, ]

# Plot beta-carotene against age
(
  plot.data1 <- 
    ggplot(data = newplasma, aes(x = age, y = betaplasma)) + 
    geom_point(size = 2) +
    xlab("Ålder") +
    ylab("Betakaroten") +
    labs(title = "Ålder och betakaroten") +
    theme(text = element_text(size = 14))
)

# Plot log(beta-carotene) against age
(
  plot.data2 <- 
    ggplot(data = plasma, aes(x = age, y = log(betaplasma))) + 
    geom_point(size = 2) +
    xlab("Ålder") +
    ylab("log(Betakaroten)") +
    labs(title = "Ålder och log(betakaroten)") +
    theme(text = element_text(size = 14))
)
minage <- min(newplasma$betaplasma)

# Linear model
newplasma.model1 <- lm(betaplasma ~ I(age - minage), data = newplasma)
summary(newplasma.model1)

# Logaritmic model
newplasma.model2 <- lm(log(betaplasma) ~ I(age - minage), data = newplasma)
