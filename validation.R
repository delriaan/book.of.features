# ::::: VALIDATION :::::
library(book.of.utilities, include.only = c("%bin%", "%tf%", "%?%", "%??%", "factor.int"))
library(data.table, include.only = c("%like%", "%ilike%", "like", "%between%"))
library(magrittr, include.only = c("%>%"))
library(stringi, include.only = c("%s+%"))
library(foreach, include.only = c("%do%", "%dopar%"))
source(dir("pkg/R", full.names = TRUE))

# make.windows() [PASS] ====

# debug(make.windows)
make.windows(
	series = c(1:100)
	, window.size = 2
	, increment = 1
	)
make.windows(series = c(1:100), window.size = 2, increment = 2)
make.windows(c(1:100), 7, 1, .complete = TRUE)


# undebug(make.windows)

# bin.windows() [PASS] ====
# undebug(bin.windows)
bin.windows(40, use.bin = 3)
bin.windows(40, use.bin = 3, as.factor = TRUE)
bin.windows(40, use.bin = 3, silently = TRUE)

.Data <- c(5, 50)
list(.Data, bin.windows(.Data, use.bin = 3))

.Data <- matrix(c(5, 50), nrow = 1)
list(.Data, bin.windows(.Data, use.bin = 3))
list(.Data, bin.windows(.Data, use.bin = 3, as.factor = TRUE))
list(.Data, bin.windows(.Data, use.bin = 3, as.factor = TRUE, silently = TRUE))

.Data <- array(1:10, dim = c(5, 2))
list(.Data, bin.windows(.Data, use.bin = 3))
list(.Data, bin.windows(.Data, use.bin = 3, as.factor = TRUE))

.Data <- array(1:10, dim = c(10, 1))
list(.Data, bin.windows(.Data, use.bin = 3, as.factor = TRUE))

.Data <- cbind(
	a = sample(70, 30)
	, b = sample(100, 30)
	, c = sample(10, 30, TRUE)
	)

list(.Data, bin.windows(.Data, use.bin = 7), .Data %bin% 7)
list(.Data, as.data.frame(.Data) |> bin.windows(use.bin = 7))

.Data <- array(t(.Data) |> as.vector(), dim = c(15, 2, 2), dimnames = list(NULL, c("B", "C"), c("A", "Z")))

list(-.Data
		 , -.Data |> bin.windows(use.bin = 7, as.factor = TRUE)
		 , -.Data |> purrr::array_tree(3) |> purrr::map(`%bin%`, 7) |> abind::abind(along = 3)
		 )

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

#
# make.quantiles() [PASS] ----
# debug(make.quantiles)
sample(900, 20) |> sort() %>% cbind(make.quantiles(., c(0:9/10)))
#
# sigmoid() [PASS] ----
sample(100, 50) %>% cbind(sigmoid(.)) |> plot(xlab = "Input", ylab = "Transformed")
#
# Build Site ----
# pkgdown::build_site(pkg = "pkg", override = list(destination = "../docs"))
