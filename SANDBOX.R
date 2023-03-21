# logic_map
logic_map <- function(fvec, avec = rep(1, length(fvec)), bvec = sort(unique(fvec)), logical.out = FALSE, regex = FALSE, chatty = FALSE){
#' Logical Test Occurrence Map
#'
#' \code{logic_map} conducts a test of a vector or list of tuples against a vector of unique values. The test can be one of \code{identity}, pattern-matching, or some custom function with Boolean output. Parallelism is supported with a registered \code{\link[foreach]{foreach}} backend.  If no backend is registered, \code{\link[foreach]{registerDoSEQ}} is used as a default.
#'
#' @param fvec (vector) Values to be tested: may be a simple vector or a list of tuples
#' @param avec (vector) Optional vector of numeric values to project \emph{a}cross the result (must be the same length as \code{fvec} or length-1)
#' @param bvec (vector) A vector of unique values forming the basis of comparison  If given as a named list, the output will preserve the names when creating columns; otherwise, the values of \code{bvec} are used as the names.
#' @param logical.out (logical, vector) When \code{TRUE}, the output consists of logical values; when a vector of length two (2) is supplied, the first value returns on \code{FALSE}, the second on \code{TRUE}; otherwise, the values of \code{avec} are used.  If the vector form is used, only the first value at each position is used to supply the choices.
#' @param regex (logical | \code{FALSE}) When \code{TRUE}, argument \code{bvec} is interpreted as patterns against which case-sensitive matches are sought are attempted.  This forces the value of \code{test} to invoke \code{\link[data.table]{like}}.
#' @param chatty (logical | \code{FALSE}) When \code{TRUE}, additional information is printed to console
#'
#' @note \code{length(fvec) == length(avec)}
#'
#' @section \strong{Warning}:
#' When combining with the source object, \code{fvec} must \strong{NOT} be sorted during the function call or the values will not map correctly in the output.
#'
#' @return Invisibly, a data.table object, the column names being the values of \code{bvec} or names of \code{bvec} if they exist
#'
#' @export

	bvec <- sort(unique(unlist(bvec)));

	if (is.null(names(bvec))){ names(bvec) <- bvec }

	if (chatty){ print(list(fvec = c(fvec), avec = c(avec), bvec = bvec, out.names = out.names)) }

	action <- { rlang::exprs(
		# logical.out = TRUE; `==` ~ <default>
		`100` = outer(fvec, bvec, `==`)

		, # avec ~ logical.out = FALSE; `==` ~ <default>
		`000` = outer(fvec, bvec, `==`) * avec

		, # logical.out = TRUE, {purrr::map; `%in% } ~ fvec[]
		`101` = purrr::map(fvec, ~rlang::set_names(bvec %in% .x, bvec)) |> purrr::reduce(rbind)

		, # avec ~ logical.out = FALSE; {purrr::map; `%in% } ~ fvec[]
		`001` = purrr::map(fvec, ~rlang::set_names(bvec %in% .x, bvec)) |> purrr::reduce(rbind) * avec

		, # logical.out = TRUE; stringi::stri_detect_regex ~ regex
		`110` = outer(fvec, bvec, stringi::stri_detect_regex)

		, # avec ~ logical.out = FALSE; `stringi::stri_detect_regex ~ regex
		`010` = outer(fvec, bvec, stringi::stri_detect_regex) * avec

		, # logical.out = TRUE, stringi::stri_detect_regex ~ regex, {purrr::map; `%in% } ~ fvec[]
		`111` = purrr::map(fvec, ~{ fv = .x; purrr::map_lgl(bvec, ~stringi::stri_detect_regex(fv, .x) |> any()) }) |>
													 	purrr::reduce(rbind)

		, # avec ~ logical.out = FALSE, stringi::stri_detect_regex ~ regex, {purrr::map; `%in% } ~ fvec[]
		`011` = purrr::map(fvec, ~{ fv = .x; purrr::map_lgl(bvec, ~stringi::stri_detect_regex(fv, .x) |> any()) }) |>
													 	purrr::reduce(rbind)* avec
		)[paste(as.numeric(logical.out), as.numeric(regex), as.numeric(is.list(fvec)), sep = "")]
	}

	eval(action[[1]])
}

# TRIAL 1 ----
logic_map(fvec = letters, bvec = letters[3:10])
logic_map(fvec = letters, bvec = letters[3:10], logical.out = TRUE)

logic_map(fvec = colors()[1:10])
logic_map(fvec = colors()[1:10], logical.out = TRUE)

logic_map(fvec = colors()[1:10], bvec = c(A = "ali", B = "aqu", C = "whi"), regex = TRUE)
logic_map(fvec = colors()[1:10], bvec = c(A = "ali", B = "aqu", C = "whi"), regex = TRUE, logical.out = TRUE)

# TRIAL 2 ----
logic_map(fvec = { sample(letters, length(letters) * 2, TRUE) |>
					slider::slide(.f = c, .after = 1L, .step = 2L, .complete = TRUE) |>
					purrr::compact()
		}, bvec = letters[3:10])

logic_map(fvec = { sample(letters, length(letters) * 2, TRUE) |>
					slider::slide(.f = c, .after = 1L, .step = 2L, .complete = TRUE) |>
					purrr::compact()
		}, bvec = letters[3:10], logical.out = TRUE)

logic_map(fvec = replicate(20, sample(colors()[1:10], 2), simplify = FALSE), bvec = c(A = "ali", B = "aqu", C = "whi"), regex = TRUE)

logic_map(fvec = replicate(20, sample(colors()[1:10], 2), simplify = FALSE), bvec = c(A = "ali", B = "aqu", C = "whi"), regex = TRUE, logical.out = TRUE)


# ----
x <- sample(1000, 100)/1000
x <- sort(x)
p <- plotly::plot_ly()

purrr::map(
	c("guder", "logis", "atan", "tanh", "gen", "gom"), ~{
		y = do.call(sigmoid
					, args = list(input = seq_along(x), family = .x, k = 0.2, center = median))

		p <<- plotly::add_lines(
				p = p, x = x, y = y
				, hovertext = sprintf("x: %s\ny: %s", x, y)
				, type = "scatter", line = list(shape = "spline"), name = .x
				)
	})

data.frame(x = x, y = sigmoid(x, family = "log"))

# make.windows() ----
series <- sample(10, 97, TRUE)
output <- list()
window.size <- 10
increment <- 3
post <- function(i){ lapply(i, `*`, 3)}

while(length(series) > 0){
	output <- c(output, list(series[1:window.size] %>% .[!is.na(.)]))
	series <- series[-c(1:increment)]
}

post(output)
# continuity ----
x <- data.table(
	g = sample(LETTERS[1:4], 2000, TRUE)
	, i = sample(100, 2000, TRUE)
	)[, j := i + sample(100, 2000, TRUE)]

setorder(x, g, i, j)
#' @title Book of Features Overview
#'
#' @description
#' The following functional families are covered in `book.of.workflow`:\cr
#'

#' @name Book of Workflow Package
NULL