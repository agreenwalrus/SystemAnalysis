pearson_cor <- function(x, y) {
  print(cor(x, y))
}

linear_regression <- function(x, y) {
  model <- lm(formula = y ~ x)
  dev.new()
  plot(x, y, ylab = "gvh", xlab = "mcg")
  print(summary(model))
  abline(model)
}

dat <- read.table("20_yeast.txt", header = FALSE)
pearson_cor(dat[, 2], dat[, 3])
linear_regression(dat[, 2], dat[, 3])
dir(.libPaths())