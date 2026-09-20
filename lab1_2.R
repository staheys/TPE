# y = 8*x^2 + 5*x
# x принадлежит [-1; 2]
# шаг = 0.15

x <- seq(from = -1, to = 2, by = 0.15)
y <- 8 * x^2 + 5 * x


result <- data.frame(x = x, y = y)
print(result)


plot(
  x, y,
  type = "o",
  main = "График функции y = 8x^2 + 5x",
  xlab = "x",
  ylab = "y",
  lwd = 2,
  pch = 16
)

grid()
