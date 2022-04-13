# Constants
save.images.3 <- TRUE


##### Part A #####
# Model containing categorical BMI-values
model1 <- lm(log(betaplasma) ~ I(age - minage) + sex + 
               smokstat + bmicat, data = data)
sum1 <- summary(model1)

# Model containing continuous BMI-values
model2 <-  lm(log(betaplasma) ~ I(age - minage) + sex + 
                smokstat + quetelet, data = data)
sum2 <- summary(model2)

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


# Plot each category against each other
ggpairs(data=contx, upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.3,    size=0.2))) +
        theme(
          text = element_text(size = 10), 
          axis.text.x = element_text(angle =45, hjust = 1),
          axis.text.y = element_text(size=9)
        )
if(save.images.3) {
  ggsave(filename = "allcontinuousvariables.png", path="./Images/Part 3/")
}

# Correlation matrix
(correlation.mat <- cor(contx))

# fat/calories and fat/cholesterol have covariance greater than 0.7

# plot fat/calories and fat/cholesterol
(
  plot9.fatvscal <- ggplot(data = data, aes(x = fat, y = calories)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se=FALSE) +
  xlab("fat per day (g)") +
  ylab("calories per day") +
  labs(title = "Fat and calories") +
  theme(text = element_text(size = 12))
)
(
  plot10.fatvschol <- ggplot(data = data, aes(x = fat, y = cholesterol)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se=FALSE) +
  xlab("fat per day (g)") +
  ylab("cholesterol per day (mg)") +
  labs(title = "Fat and cholesterol") +
  theme(text = element_text(size = 12))
)
if(save.images.3) {
  ggsave(filename = "fatvsvarious.png", path="./Images/Part 3/", 
         grid.arrange(grob=plot9.fatvscal, plot10.fatvschol, ncol=2)
  )
}


# Change reference category to No.
data$vituse <- factor(data$vituse,
                           levels = c(1, 2, 3),
                           labels = c("Fairly often", "Not often", "No"))
# Frequency table of vitamin usage
table(data$vituse)
data$vituse <- relevel(data$vituse, ref="No")


##### Part C #####
data.model4 <- lm(log(betaplasma) ~ I(age - minage) + quetelet +
                         calories + fat + fiber + alcohol +
                         cholesterol + betadiet, data = data)


# Leverage & yhat
data$v <- influence(data.model4)$hat
data$yhat <- predict(data.model4)

# Leverage against age
(
  plot11.levage <- ggplot(cbind(data), aes(x = age, y = v)) +
    geom_jitter(width = 1)  +
    geom_hline(yintercept = 1/nrow(data)) +
    geom_hline(
      yintercept = 2*length(data.model4$coefficients)/nrow(data),
      color = "red"
    ) + expand_limits(y = c(-0.001, 0.006)) + labs(title = "Leverage vs age") +
    labs(caption = " ") + xlab("age (years)") +
    ylab("leverage") + theme(text = element_text(size = 12))
)

# Leverage against alcohol consumption
(
  plot12.levalcohol <- ggplot(cbind(data), aes(x = alcohol, y = v)) +
    geom_jitter(width = 1)  + geom_hline(yintercept = 1/nrow(data)) +
    geom_hline(
      yintercept = 2*length(data.model4$coefficients)/nrow(data), 
      color = "red"
    ) + expand_limits(y = c(-0.001, 0.006)) + 
    labs(title = "Leverage vs alcohol") + 
    labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
    xlab("alcohol consumption (drinks/week)") + ylab("leverage") + 
    theme(text = element_text(size = 12))
)

# Save image
if(save.images.3) {
  ggsave(filename = "leveragevsalcoholandage.png", path="./Images/Part 3/", 
         grid.arrange(grob=plot11.levage, plot12.levalcohol, ncol=2)
  )
}

# Print the amount of non-drinkers
length(data$alcohol[data$alcohol == 0])

# Plot alcohol consumption against various other variables
(
  plot11.alcoholquetelet <- ggplot(data=data, aes(x = quetelet, y = alcohol)) +
    xlab("quetelet") + ylab("alcohol consumption") + geom_point(size=1) +
    theme(text = element_text(size=12)) + labs(title="Alcohol vs. quetelet")
)
(
  plot12.alcoholcalories <- ggplot(data=data, aes(x = calories, y = alcohol)) +
    xlab("calories") + ylab("alcohol consumption") + geom_point(size=1) +
    theme(text = element_text(size=12)) + labs(title="Alcohol vs. calories")
)
(
  plot13.alcoholplasma <- ggplot(data=data, aes(x = betaplasma, y = alcohol)) +
    xlab("betaplasma (ng/ml)") + ylab("alcohol consumption") +
    theme(text = element_text(size=12)) + labs(title="Alcohol vs. betaplasma") +
    geom_point(size=1)
)
(
  plot14.alcoholage <- ggplot(data=data, aes(x = age, y = alcohol)) +
    xlab("age") + ylab("alcohol consumption") + geom_point(size=1) + 
    theme(text = element_text(size=12)) + labs(title="Alcohol vs. age")
)

# Save image
if(save.images.3) {
  ggsave(filename = "alcoholvsvarious.png", path="./Images/Part 3/", 
         grid.arrange(
           plot11.alcoholquetelet, 
           plot12.alcoholcalories,
           plot13.alcoholplasma,
           plot14.alcoholage,
           ncol=2, nrow=2)
  )
}

# Histogram of alcoholconsumption
(
  plot15.alc.hist <- ggplot(data = data, aes(x = alcohol)) +
    geom_histogram(bins = 100) + xlab("alcohol consumption (drinks/week)") + 
    ylab("count (n)") +
    labs(title = "Alcohol consumption") + theme(text = element_text(size = 14))
)
if(save.images.3) {
  ggsave(filename = "alcoholhist.png", path="./Images/Part 3/")
}


##### Part D #####
# QQplot for residuals
data$yhat <- predict(data.model4)
data$r <- resid(data.model4)
data$rs <- rstudent(data.model4)
(
  plot.resid.qq <- ggplot(data, aes(sample = r)) + 
    geom_qq(size = 2) + geom_qq_line() + 
    labs(title = "Normal QQ-plot of the residuals") +
    xlab("normal quantiles") + ylab("residual quantiles") +
    theme(text = element_text(size = 14), plot.title = element_text(size=14))
)
(
  plot.resid.studentized <- ggplot(data=data, aes(x = yhat, y = sqrt(abs(rs))))
    + geom_point(size=2) + geom_hline(yintercept = c(0, sqrt(2), sqrt(3))) 
    + labs(title = TeX("Residuals vs $\\hat{y}$")) + theme(text = element_text(size = 12))
    + xlab(TeX("$\\hat{y}$")) + ylab(TeX("$\\sqrt{|r^*_i|}$"))
)

if(save.images.3){
  ggsave(filename = "residualsdietarymodel.png", path="./Images/Part 3/",
         arrangeGrob(plot.resid.qq, plot.resid.studentized, ncol=2))
}

# Plot studentized residuals vs alcohol and calories where outlier showed
(plot.rs.c.alc <- plot_ly(x=data$alcohol, y=data$v, z=data$r, type="scatter3d",
                   mode="markers") %>%
                   layout(
                   title = "Residuals vs leverage and alcohol",
                   scene = list(
                     xaxis = list(title = "Alcohol consumption"), 
                     yaxis = list(title = "Leverage" ),  
                     zaxis = list(title = "Residuals")
)))

###### Part E ######
# Cook's distance
data$cd <- cooks.distance(data.model4)

(
  plot.cd.lev <- ggplot(data=data, mapping=aes(x=v, y=cd)) + geom_point(size=2)
  + xlab("Leverage") + ylab("Cook's distance") 
  + labs(title="Cook's distance vs leverage")
  + theme(text=element_text(size=12))
  + geom_hline(yintercept = 4 / length(data$v), color="orange") 
  + geom_hline(yintercept = qf(0.5, 8, 305))
)
if(save.images.3){
  ggsave(filename = "cooksdistancevsleverage.png", path="./Images/Part 3/")
}

# DF-betas
(dfb <- dfbetas(data.model4))

# DF-betas for various categories
(data.columns <- colnames(dfb))

data$intercept.dfbeta <- dfb[, "(Intercept)"]
data$age.dfbeta <- dfb[, "I(age - minage)"]
data$quetelet.dfbeta <- dfb[, "quetelet"]
data$calories.dfbeta <- dfb[, "calories"]
data$fat.dfbeta <- dfb[, "fat"]
data$fiber.dfbeta <- dfb[, "fiber"]
data$alcohol.dfbeta <- dfb[, "alcohol"]
data$cholesterol.dfbeta <- dfb[, "cholesterol"]
data$betadiet.dfbeta <- dfb[, "betadiet"]
colnames(dfb)

# Betaplots
(
  betaplot1 <- ggplot(data = data, aes(x=age, y=age.dfbeta)) + 
    geom_point(size=1, alpha=0.8) + geom_vline(xintercept=mean(data$age),
                                               color = "red") +
    xlab("Age") + ylab(TeX("$df-\\beta_{age}$"))
)
(
  betaplot2 <- ggplot(data = data, aes(x=quetelet, y=quetelet.dfbeta)) + 
    geom_point(size=1, alpha=0.8) + 
    geom_vline(xintercept=mean(data$quetelet), color = "red") + 
    xlab("quetelet") + ylab(TeX("$df-\\beta_{quetelet}$"))
)
(
  betaplot3 <- ggplot(data = data, aes(x=calories, y=calories.dfbeta)) + 
    geom_point(size=1, alpha=0.8) + geom_vline(xintercept=mean(data$calories),
                                               color = "red") +
    xlab("calories") + ylab(TeX("$df-\\beta_{calories}$"))
)
(
  betaplot4 <- ggplot(data = data, aes(x=fat, y=fat.dfbeta)) + 
    geom_point(size=1, alpha=0.8) + geom_vline(xintercept=mean(data$fat),
                                               color = "red") +
    xlab("fat") + ylab(TeX("$df-\\beta_{fat}$"))
)

grid.arrange(grob=betaplot1, betaplot2, betaplot3, betaplot4,
             top = textGrob("DF-betas",gp=gpar(fontsize=18,font=1)))

if(save.images.3){
  ggsave(filename = "dfbetas1.png", path="./Images/Part 3/", 
         grid.arrange(grob=betaplot1, betaplot2, betaplot3, betaplot4,
                      top = textGrob("DF-betas",gp=gpar(fontsize=18,font=1))
                      )
         )
}

# The remaining categories
(
  betaplot5 <- ggplot(data = data, aes(x=fiber, y=fiber.dfbeta)) + 
    geom_point(size=1) + geom_vline(xintercept=mean(data$fiber),
                                    color = "red") +
    xlab("fiber") + ylab(TeX("$df-\\beta_{fiber}$"))
)
(
  betaplot6 <- ggplot(data = data, aes(x=alcohol, y=alcohol.dfbeta)) + 
    geom_point(size=1)+ geom_vline(xintercept = mean(data$alcohol),
                                   color = "red") +
    xlab("alcohol") + ylab(TeX("$df-\\beta_{alcohol}$"))
)
(
  betaplot7 <- ggplot(data = data, aes(x=cholesterol, y=cholesterol.dfbeta)) + 
    geom_point(size=1) + geom_vline(xintercept=mean(data$cholesterol),
                                    color = "red") +
    xlab("cholesterol") + ylab(TeX("$df-\\beta_{cholesterol}$"))
)
(
  betaplot8 <- ggplot(data = data, aes(x=betadiet, y=betadiet.dfbeta)) + 
    geom_point(size=1) + geom_vline(xintercept=mean(data$betadiet),
                                    color = "red") +
    xlab("betadiet") + ylab(TeX("$df-\\beta_{betadiet}$"))
)

grid.arrange(grob=betaplot5, betaplot6, betaplot7, betaplot8,
             top = textGrob("DF-betas",gp=gpar(fontsize=18,font=1)))

if(save.images.3){
  ggsave(filename = "dfbetas2.png", path="./Images/Part 3/", 
         grid.arrange(grob=betaplot5, betaplot6, betaplot7, betaplot8,
                      top = textGrob("DF-betas",gp=gpar(fontsize=18,font=1))
         )
  )
}

##### Part F #####
model.dietary <- data.model4
model.dietary$coefficients
AIC(model.dietary)
(summary(model.dietary))

# Backwards elimination
model.dietary <- step(model.dietary, direction="backward")

# Display results
model.dietary$coefficients
exp(model.dietary$coefficients)
exp(confint(model.dietary))

##### Part G #####
# Load the data again to remove redundant columns
data <- read.delim(file = "Data/plasma.txt")
data <- data[data$betaplasma > 0, ]

# Categorical variables
data$sex <- factor(data$sex,
                   levels = c(1, 2),
                   labels = c("Male", "Female"))

data$smokstat <- factor(data$smokstat,
                        levels = c(1, 2, 3),
                        labels = c("Never", "Former", "Current Smoker"))

data$bmicat <- factor(data$bmicat,
                      levels = c(1, 2, 3, 4),
                      labels = c("Underweight", "Normal", "Overweight", "Obese"))

data$vituse <- factor(data$vituse,
                      levels = c(1, 2, 3),
                      labels = c("Fairly often", "Not often", "No"))


# Perform necessary releveling
data$bmicat <- relevel(data$bmicat, "Normal")
data$sex <- relevel(data$sex, "Female")
data$vituse <- relevel(data$vituse, ref="No")

# Extract variable names
(dietary.var <- names(model.dietary$coefficients))
(dietary.var <- dietary.var[dietary.var != "(Intercept)"])
(dietary.var <- dietary.var[dietary.var != "I(age - minage)"])
(dietary.formula <- as.formula(
  paste("betaplasma ~ I(age - minage) + bmicat + smokstat + sex + ", 
        paste(c(dietary.var), collapse= "+"))
))

# Lower and upper model
null.model <- lm(betaplasma ~ 1, data = data)
full.model <- lm(dietary.formula, data = data)
summary(full.model)

# AIC
step.aic <- step(model.dietary, 
                 scope = list(lower = null.model, upper = full.model),
                 direction = "both",
                 k = 2)

# Display results
step.aic$coefficients
exp(step.aic$coefficients)
exp(confint(step.aic))

# BIC
step.bic <- step(model.dietary, 
     scope = list(lower = null.model, upper = full.model),
     direction = "both",
     k = log(nrow(data)))

# Display results
step.bic$coefficients
exp(step.bic$coefficients)
exp(confint(step.bic))


##### Part H #####
# R-squared for the various models
(
  model.comparison.r2 <- data.frame(
  "age.model rs" = summary(age.model)$r.squared,
  "background.model rs" = summary(background.model)$r.squared,
  "dietary.model rs" = summary(model.dietary)$r.squared,
  "aic.model rs" = summary(step.aic)$r.squared,
  "bic.model rs" = summary(step.bic)$r.squared 
  )
)

# Adjusted R-squared
(
  model.comparison.adjr2 <- data.frame(
    "age.model adj. rs" = summary(age.model)$adj.r.squared,
    "background.model adj. rs" = summary(background.model)$adj.r.squared,
    "dietary.model adj. rs" = summary(model.dietary)$adj.r.squared,
    "aic.model adj. rs" = summary(step.aic)$adj.r.squared,
    "bic.model adj. rs" = summary(step.bic)$adj.r.squared 
  )
)
