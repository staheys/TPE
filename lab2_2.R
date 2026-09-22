area <- c(
  26, 58.26, 30.85, 183.01,
  141.08, 81.95, 185.99, 47.52,
  40.87, 84.62, 2.1, 132.53
)

acidity <- c(
  5.9, 6.1, 6.1, 4.8,
  5.2, 5.1, 5.5, 5.6,
  6.1, 5.4, 4.7, 5.8
)

nitrogen <- c(
  "высокое", "высокое", "высокое", "низкое",
  "низкое", "низкое", "высокое", "высокое",
  "высокое", "высокое", "низкое", "высокое"
)

nitrogen_low <- ifelse(nitrogen == "низкое", 1, 0)

potassium <- c(
  18.58, 14.42, 17.4, 19.3,
  24.42, 19.59, 10.83, 12.76,
  6.43, 8.64, 8.55, 14.74
)

phosphorus <- c(
  20.5, 23.28, 25.07, 18.52,
  19.55, 17.56, 19.15, 16,
  12.93, 9.33, 13.88, 14.21
)

yield_value <- c(
  9.7, 11.6, 10.9, 2.2,
  3.5, 3.6, 4.8, 6.3,
  5, 4.8, 1.1, 8
)

data2 <- data.frame(
  area,
  acidity,
  nitrogen_low,
  potassium,
  phosphorus,
  yield_value
)

model2 <- lm(
  yield_value ~ area + acidity + nitrogen_low +
    potassium + phosphorus,
  data = data2
)

summary(model2)
coef(model2)