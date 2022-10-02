# ::::: VALIDATION :::::
# bin.windows() ====
library(book.of.utilities)
library(data.table)
library(doRNG)
library(purrr)

bin.windows(10)

bin.windows(c(5, 50), use.bin = 3)

bin.windows(array(1:10, dim = 10), use.bin = 3)

X <- cbind(
	a = sample(70, 30)
	, b = sample(100, 30)
	, c = sample(10, 30, TRUE)
	) %>% as.arr %>% bin.windows(use.bin = 7)
dim(X)
X

X <- bin.windows(array(sample(100:200, 60), dim = c(10, 2, 3), dimnames = map2(c(10, 2, 3), LETTERS[1:3], ~rep.int(.y, .x))), use.bin = 7)
dim(X)
X[1,1,]
X[1,2,]
X[,,2]
X[1,2,3]

X <- bin.windows(array(sample(50:500, 120), dim = c(5, 4, 3, 2), dimnames = map2(c(5, 4, 3, 2), LETTERS[1:4], ~rep.int(.y, .x))), use.bin = 20)
dim(X)
X[1,1,,]
X[3,2,3, ]
X[,,3,1]
X[5,2,3, 1]

# debug(bin.windows)
# undebug(bin.windows)

define(x = mtcars, map(.SD, as.list) ~ 1)
define(x = mtcars, map(.SD[, !"rn"], ~.x * 2) ~ .)
define(x = mtcars, map(.SD[, !"rn"], ~.x * 2) ~ gear + mpg)
