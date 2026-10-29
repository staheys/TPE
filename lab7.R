f <- function(x) exp(x - 3) - x^2

x <- seq(5, 6, by = 0.0001)
y <- f(x)

k <- which.min(y)
cat("min: x =", x[k], "\n")
cat("function value:", y[k], "\n")
cat("iterations:", length(x), "\n")

plot(x, y, type = "l", xlab = "x", ylab = "f(x)",
     main = "Function f(x)")
points(x[k], y[k], pch = 19, col = "red")
