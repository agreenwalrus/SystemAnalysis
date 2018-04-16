confidential_interval = 0.95



#read data
data <- read.table("initial.txt", sep = "" , header = F, na.strings ="", stringsAsFactors= F)

first <- data[, 1]
second <- data[, 4]

# mathematical expectation
M_first <- mean(first)
M_second <- mean(second)

# dispersion (variation)
D_first <- var(first)
D_second <- var(second)

t.test(first, conf.level = confidential_interval)
t.test(second, conf.level = confidential_interval)a

# quantiles
quantile(replicate(length(first), mean(first)), prob=c((1 - confidential_interval)/2, confidential_interval + (1 - confidential_interval)/2))
quantile(replicate(length(second), mean(second)), c((1 - confidential_interval)/2, confidential_interval + (1 - confidential_interval)/2))

var.test(first, second, conf.level = confidential_interval)
t.test(first, second, conf.level = confidential_interval)
