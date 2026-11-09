
library(lpSolve)

# 1
f <- c(10, 80, 40)
a <- matrix(c(2, 6, 4,
              2, 9, 7,
              6, 4, 0), nrow = 3, byrow = TRUE)
b <- c(90, 60, 150)

r1 <- lp("max", f, a, c("<=", "<=", "<="), b)
cat("1\n")
cat("x =", round(r1$solution, 4), "\n")
cat("Max =", round(r1$objval, 4), "\n\n")

# 2
weight <- c(5, 8, 10, 3)
price <- c(16, 10, 12, 8)

r2 <- lp("max", price, matrix(weight, nrow = 1), "<=", 16,
         all.bin = TRUE)
cat("2\n")
cat("Items:", which(r2$solution == 1), "\n")
cat("Weight =", sum(weight * r2$solution), "\n")
cat("Costs =", r2$objval, "\n\n")

# 3
cost <- matrix(c(8, 3, 7, 6, 1, 4,
                 5, 2, 4, 1, 4, 5,
                 7, 5, 3, 2, 3, 1,
                 3, 2, 7, 4, 4, 5,
                 3, 1, 2, 4, 7, 1), nrow = 5, byrow = TRUE)
stock <- c(90, 70, 15, 160, 90)
need <- c(25, 150, 50, 75, 75, 50)

r3 <- lp.transport(cost, "min", rep("=", 5), stock,
                   rep("=", 6), need)
cat("3\n")
cat("Plan:\n")
print(r3$solution)
cat("Min =", r3$objval, "\n")
