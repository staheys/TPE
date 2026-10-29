f <- function(x) exp(x - 3) - x^2

a <- 5
b <- 6
eps <- 0.0001
delta <- eps / 10
n <- 0

while (b - a > eps) {
  x1 <- (a + b - delta) / 2
  x2 <- (a + b + delta) / 2
  if (f(x1) < f(x2)) {
    b <- x2
  } else {
    a <- x1
  }
  n <- n + 1
}

x <- (a + b) / 2
cat("x =", round(x, 7), "\n")
cat("f(x) =", round(f(x), 7), "\n")
cat("iterations =", n, "\n")

curve(f(x), from = 5, to = 6,
      xlab = "x", ylab = "f(x)")
points(x, f(x), pch = 19)
