rm(list = ls())

row.names <- c("row1", "row2", "row3")
col.names <- c("col1", "col2", "col3")

matrix1 <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = T, dimnames = list(row.names, col.names))

print(matrix1)
