v <- 0:10

T <- c(
  10.000000,
  10.000000,
  9.914322,
  9.783897,
  9.620585,
  9.430680,
  9.218183,
  8.985899,
  8.735920,
  8.469876,
  8.189079
)

data3 <- data.frame(v, T)

model3 <- nls(
  T ~ 10 - a * v^b + c * v,
  data = data3,
  start = list(
    a = 0.1,
    b = 2,
    c = 0.1
  )
)

summary(model3)
coef(model3)

v_grid <- seq(0, 10, by = 0.05)

T_grid <- predict(
  model3,
  newdata = data.frame(v = v_grid)
)

plot(
  v, T,
  pch = 19,
  xlab = "Скорость ветра v",
  ylab = "Ощущаемая температура T",
  main = "Нелинейная регрессия"
)

lines(v_grid, T_grid, lwd = 2)