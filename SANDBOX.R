i <- sample(500, 50)
bin.windows(i, as.factor = TRUE)
bin.windows(i, as.factor = !TRUE)
bin.windows(i, use.bin = 10, as.factor = TRUE)
bin.windows(i, use.bin = 10, as.factor = !TRUE)

# debug(bin.windows)
bin.windows(i = mtcars[, c("gear", "carb")], use.bin = 2)
undebug(bin.windows)
