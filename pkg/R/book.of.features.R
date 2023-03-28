#' @title Book of Features Overview
#'
#' @description
#' \code{book.of.features} provides feature-engineering helper functions.
#'
#' @importFrom book.of.utilities %bin% factor.int %tf%
#' @importFrom stringi %s+%
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom utils str
#' @import data.table
#'
#' @name book.of.features
NULL
# usethis::use_proprietary_license(copyright_holder = "Chionesu George")
# if (dir(pattern = "yml") |> length() == 0){ usethis::use_pkgdown() }
# pkgdown::build_site()

sigmoid <- function(input, family = "logistic", center = mean, ...){
#' {0,N} Sigmoid Scaler
#'
#' \code{sigmoid} Scales the input to a range of {0,N} using the sigmoid function
#'
#' The input must contain positive and negative values when \code{centered == FALSE}
#'
#' Function arguments for each sigmoid family are as follows:
#' \enumerate{
#'   \item \code{"generalized"}: \code{list(A, K, C, Q, B, v)}
#'   \item \code{"logistic"}: \code{list(L, K)}
#'   \item \code{"gompertz"}: \code{list(A, B, C)}
#' }
#' Families \code{tanh}, \code{atan}, and \code{guder}(mannian) only take the input as its argument
#'
#' @section References:
#' \enumerate{
#' \item{\href{https://en.m.wikipedia.org/wiki/Generalised_logistic_function}{Generalized Logistic Function}}
#' \item{\href{https://en.m.wikipedia.org/wiki/Sigmoid_function}{Sigmoid Function}}
#' \item{\href{https://en.m.wikipedia.org/wiki/Gompertz_function}{Gompertz Function}}
#' \item{\href{https://en.m.wikipedia.org/wiki/Gudermannian_function}{Gudermannian Function}}
#' }
#'
#' @param input (numeric vector or coercible vector)
#' @param family (string | "logistic") The family of sigmoid equation to use: Currently, only "logistic", "generalized", and "gompertz" are supported
#' @param center A function that returns the 'center' of \code{input}
#' @param ... Valid elements that populate the arguments of \code{type} based on the selected \code{family}
#'
#' @return A numeric vector of domain {0, N}
#'
#' @export

	input = as.complex(as.numeric(unlist(input)));

	# Initialize the internal environment with defaults
	L <- K <- A <- C <- Q <- B <- v <- 1;

	family <- as.character(rlang::enexpr(family))

	# Capture the internal environment
	env <- environment((function(){}));

	# Overwrite existing variables with supplied values
	list2env(rlang::list2(...), envir = env);

	# Set the expression list for sigmoid families
	sig.family <- { rlang::exprs(
			generalized = A + (K - A)/(C + (Q * exp(-1 * B * input)))^(1/v)
			, gompertz  = A * exp(-B * exp(-C * input))
			, logistic	= L/(1 + exp(-K * (input - center(input))))
			, tanh			= { exp(input) - exp(-input) } / { exp(input) + exp(-input) }
			, atan			= atan(input)
			, guder			= 2 * atan(tanh(input/2))
			)}

	# Evaluate the family with the supplied arguments
	output <- eval(sig.family[[which(grepl(paste0("^", family), x = names(sig.family)))]]);
	if (all(Im(output) == 0)){ Re(output) } else { output }
}
#
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
#
bin.windows <- function(i = 1, use.bin = NULL, as.factor = FALSE, ...){
#' Create Bins From Integer Factor
#'
#' \code{bin.windows} creates binned ranges based on the minimum factor of the integer input (\code{i}) or user-supplied value.
#'
#' @param i (integer[]) An integer scalar, vector, or n-dimensional object executed conditionally as follows:
#' \itemize{
#' \item if a vector of length = 1, a zero-based sequence up to \code{abs(i) } is used
#' \item if a vector of length = 2, a sequence is created from the values in the order given
#' \item if a vector of length >= 3, the raw values
#' \item if n-dimensional, recursion along the last dimension given by \code{dim(i)} until a vector is detected
#' }
#'
#' @param use.bin The bin size to use: when empty, the smallest prime number or integer factor in \code{i} within the range of \code{i} is used.
#' @param as.factor (logical) Should the output be converted into a factor?
#' @param ... (not used)
#'
#' @return A character (or factor) vector the length of the input, as "binned" representations.  If the input is dimensional, an array of the same dimensions is returned
#'
#' @family Data Generation
#'
#' @export

	.dir <- FALSE;

	force(use.bin)

	if (rlang::is_empty(use.bin)){
		bin_fact <- purrr::keep(unlist(book.of.utilities::factor.int(i)), `%in%`, `:`(min(i), max(i))) |> min(na.rm = TRUE)
		use.bin <- if (rlang::is_empty(bin_fact)){
				book.of.utilities::gen.primes(n = 1, domain = range(i), distinct = TRUE, random = FALSE)
			} else { bin_fact }
	}

	func <- purrr::as_mapper(~{
				orig_X <- .x
				sort_X <- sort(orig_X, decreasing = .dir);
				.breaks <- round(diff(range(sort_X)) / use.bin);
				.out <- {
					.map <- { data.table::data.table(
											win.vals = sort_X
											, label = if (.breaks == 1){ as.character(sort_X) } else {
													cut(sort_X
														, breaks = .breaks
														, dig.lab = 0
														, ordered_result = TRUE
														, include.lowest = TRUE
														, right = TRUE
														)}
											)[order(orig_X)]
									}

					data.table::setattr(.map$label, "bin.map", .map[, .(win.vals = list(range(win.vals))), by = label]) |>
						data.table::setattr("bin.size", use.bin)
				}

				if (!as.factor){
					if (is.factor(.out)){ levels(.out)[.out] } else { .out }
				} else { .out }
			})

	if (rlang::is_empty(dim(i)) | rlang::has_length(dim(i), 1)){
		func(as.vector(i))
	} else {
		.dns = dimnames(i);
		.dms = dim(i);

		.out = purrr::array_branch(i, margin = length(.dms)) |> purrr::map(func);

		if (length(.dms) == 2){ .out <- purrr::reduce(.out, cbind) }

		as.array(.out) |> structure(dim = .dms, dimnames = .dns);
	}
}
#
make.date_time <- function(add_vec = 1:7, var_start = Sys.Date(), var_form = "%Y-%m-%d 00:00:00", var_tz = "", var_interval = "days"){
#' Date-Time Sequence Generator
#'
#' \code{make.date_time} serves as a wrapper for \code{\link[stringi]{stri_datetime_add}}
#'
#' @param add_vec (numeric | c(1:7)): Vector of integer values to add
#' @param var_start (datetime | Sys.Date()): Vector of integer values used to generate datetime results
#' @param var_form (string | "\%Y-\%m-\%d 00:00:00"): Format for the datetime values
#' @param var_tz (string | ""): Timezone for the datetime values
#' @param var_interval (string | "days"): The unit of time for \code{add_vec}
#'
#' @return A temporal vector
#'
#' @family Data Generation
#'
#' @export

	stringi::stri_datetime_add(time = as.Date(var_start), value = add_vec, unit = var_interval, tz = var_tz) |> format(var_form)
}
#
make.windows <- function(series, window.size, increment = 1, post = eval, debug = FALSE, ...) {
#' Serial Window Maker (DEPRECATED)
#'
#' \code{make.windows} is a wrapper for \code{\link[slider]{slide}}
#'
#' @param series A list or vector object from which incremental subsets (windows) of a fixed size are chosen
#' @param window.size (integer) The size of the subset (window) to select
#' @param increment (integer) The number of elements by which iteration should advance
#' @param post (function) A post-processing function on the return object having class "data.table" and single column "window"
#' @param debug (logical | FALSE) When \code{TRUE}, additional information is printed to console for debugging purposes
#' @param ... Additional arguments sent to  \code{\link[slider]{slide}}
#'
#' Sets are the result of forward-moving partitioning:
#' \enumerate{
#'   \item \code{window.size}: The size of each partition (W)
#'   \item \code{increment}: The number of items to increment before selecting the next W items (W + i)
#' }
#'
#' @return A serialized collection-list of partitions (windows), each window containing a subset of size \code{window.size}
#'
#' @family Data Generation
#'
#' @export

	slider::slide(
		.x = series
		, .f = post
		, .before = ifelse(window.size < 0L, window.size - 1, 0L)
		, .after = ifelse(window.size > 0L, window.size - 1, 0L)
		, .step = increment
		, ...
		) |>
	purrr::compact()
}
#
make.quantiles <- function(x, ...){
#' Quantiles Transformation
#'
#' \code{make.quantiles} is a wrapper for \code{\link[stats]{quantile}} replacing the input with calculated values.
#'
#' @param x The input vector
#' @param ... (\code{\link[rlang]{dots_list}}): Additional arguments sent to \code{\link[stats]{quantile}}
#'
#' @return A quantile representation of the input
#' @export
  q.vec <- rlang::inject(quantile(x = x, ...));
  idx <- sapply(x, function(i){ max(which(q.vec <= i))})

  return(q.vec[idx])
}