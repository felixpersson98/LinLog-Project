library(GGally)

# model1
# model2

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

# Creating data frame with all continuous variables
contx <- newplasma[, c("age", "quetelet", "calories", "fat", "fiber",
                    "alcohol", "cholesterol", "betadiet")]
ggpairs(contx) + theme(text = element_text(size = 14))
## HUR GÖR VI DEN LÄSBAR?


