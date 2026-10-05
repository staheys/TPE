plants <- data.frame(
  id = 1:20,

  count = c(
    5, 5, 3, 10, 11, 4, 5, 4, 1, 5,
    1, 10, 10, 5, 5, 4, 1, 1, 2, 5
  ),

  fert = factor(c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2
  )),

  weight = c(
    10, 15, 30, 3, 2, 8, 10, 10, 50, 9,
    65, 4, 14, 15, 17, 20, 68, 60, 25, 15
  )
)

plants
str(plants)


model_cov <- aov(weight ~ count + fert, data = plants)
summary(model_cov)


model_lm <- lm(weight ~ count + fert, data = plants)
summary(model_lm)
coef(model_lm)


model_interaction <- aov(
  weight ~ count * fert,
  data = plants
)

summary(model_interaction)