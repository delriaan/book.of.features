# ::::: VALIDATION :::::
library(book.of.utilities, include.only = c("%bin%", "%::%", "%?%", "%??%", "factor.int"))
library(data.table, include.only = c("%like%", "%ilike%", "like", "%between%"))
library(magrittr, include.only = c("%>%"))
library(stringi, include.only = c("%s+%"))
library(foreach, include.only = c("%do%", "%dopar%"))
# bin.windows() ====

# debug(make.windows)
make.windows(
	series = c(1:100)
	, window.size = 2
	, increment = 1
	)
make.windows(series = c(1:100), window.size = 2, increment = 2)
make.windows(c(1:100), 7, 1, .complete = TRUE)


# undebug(make.windows)

bin.windows(i = 10)
bin.windows(i = 10, as.factor = TRUE)

bin.windows(c(5, 50), use.bin = 3)
bin.windows(c(5, 50), use.bin = 3, as.factor = TRUE)

bin.windows(array(1:10, dim = 10), use.bin = 3)
bin.windows(array(1:10, dim = 10), use.bin = 3, as.factor = TRUE)

X <- cbind(
	a = sample(70, 30)
	, b = sample(100, 30)
	, c = sample(10, 30, TRUE)
	) |> as.array() |> bin.windows(use.bin = 7)

X <- cbind(
	a = sample(70, 30)
	, b = sample(100, 30)
	, c = sample(10, 30, TRUE)
	) |> as.array() |> bin.windows(use.bin = 7, as.factor = TRUE)

dim(X)
X |> str()

X <- bin.windows(
			i = array(data = sample(100:200, 60)
								, dim = c(10, 2, 3)
								, dimnames = purrr::map2(c(10, 2, 3), LETTERS[1:3], ~rep.int(.y, .x))
								)
			, use.bin = 7
			, as.factor = TRUE
			)
dim(X)
X[1,1,]
X[1,2,]
X[,,2]
X[1,2,3]

X <- bin.windows(array(sample(50:500, 120), dim = c(5, 4, 3, 2), dimnames = purrr::map2(c(5, 4, 3, 2), LETTERS[1:4], ~rep.int(.y, .x))), use.bin = 20)
dim(X)
X[1,1,,]
X[3,2,3, ]
X[,,3,1]
X[5,2,3, 1]

# debug(bin.windows)
# undebug(bin.windows)
# pkgdown::build_site()
