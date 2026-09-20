# 30 элементов, 6 строк, 5 столбцов.

m <- matrix(1:30, nrow = 6, ncol = 5, byrow = TRUE)

cat("Исходная матрица:\n")
print(m)

row_sums <- rowSums(m)

col_sums <- colSums(m)

cat("\nСуммы элементов по строкам:\n")
print(row_sums)

cat("\nСуммы элементов по столбцам:\n")
print(col_sums)