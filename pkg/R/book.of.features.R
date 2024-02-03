bin.windows <- function(i = 1, use.bin = NULL, as.factor = FALSE, label_format = "<%s,%s>", silently = FALSE, ...){
#' Create Bins From Integer Factor
#'
#' \code{bin.windows} creates binned ranges based on the minimum factor of the integer input (\code{i}) or user-supplied value.
#'
#' @param i (integer[]) An integer scalar, vector, or n-dimensional object executed conditionally as follows:
#' \itemize{
#' \item{if a vector of length = 1, a zero-based sequence up to \code{abs(i) } is used}
#' \item{if a vector of length = 2, a sequence is created from the values in the order given if the input is non-dimensional}
#' \item{if a vector of length >= 3 or \code{i} is dimensional, the raw values coerced into a vector}
#' }
#'
#' @param use.bin The bin size to use: when empty, the smallest prime number or integer factor in \code{i} within the range of \code{i} is used.
#' @param as.factor (logical) Should the output be converted into a factor?
#' @param label_format (string) A two-argument string compatible with \code{\link[base]{sprintf}} that controls label output
#' @param silently (logical) Should the output return invisibly?
#' @param ... (not used)
#'
#' @return A character (or factor) vector the length of the input, as "binned" representations.  If the input is dimensional, an array of the same dimensions is returned
#'
#' @note Factor output is only available for heterogeneous data structures.
#'
#' @examples
#' bin.windows(40, use.bin = 3)
#' bin.windows(40, use.bin = 3, as.factor = TRUE)
#' bin.windows(40, use.bin = 3, silently = TRUE)
#'
#' .Data <- c(5, 50)
#' list(before = .Data, after = bin.windows(.Data, use.bin = 3))
#'
#' .Data <- array(1:10, dim = c(5, 2))
#' list(before = .Data, after = bin.windows(.Data, use.bin = 3, as.factor = FALSE))
#' list(before = .Data, after = bin.windows(.Data, use.bin = 3, as.factor = TRUE))
#'
#' .Data <- cbind(
#'     a = sample(70, 30)
#'     , b = sample(100, 30)
#'     , c = sample(10, 30, TRUE)
#'     )
#'
#' list(before = .Data, after = bin.windows(.Data, use.bin = 7))
#' list(before = .Data, after = as.data.frame(.Data) |> bin.windows(use.bin = 7))
#'
#' @family Data Generation
#'
#' @export

	if (rlang::has_length(i, 1)){
		i <- matrix(0:i, ncol = 1)
	} else if (rlang::has_length(i, 2) & rlang::is_empty(dim(i))){
		i <- `:`(i[1], i[2])
	}

	if (rlang::is_empty(dim(i))){ dim(i) <- c(length(i), 1) }

	output <- NULL;
	.dnames <- dimnames(i);
	.dims <- dim(i);
	.class <- class(i);

	i <- data.table::setattr(if ("data.frame" %in% .class){ unlist(i, use.names = FALSE) } else { c(i) }, "orig", i)

	force(use.bin)
	use.bin <- abs(use.bin)

	if (rlang::is_empty(use.bin)){
		bin_fact <- purrr::keep(unlist(book.of.utilities::factor.int(i)), `%in%`, `:`(min(i), max(i))) |> min(na.rm = TRUE)
		use.bin <- if (rlang::is_empty(bin_fact)){
				book.of.utilities::gen.primes(n = 1, domain = range(i), distinct = TRUE, random = FALSE)
			} else { bin_fact }
	}

	func <- purrr::as_mapper(~{
				orig_X <- .x
				orig_X.bins <- book.of.utilities::`%bin%`(orig_X, use.bin)
				sort_X <- sort(orig_X) |> unique();
				.fmt <- .y
				.map <- { data.table::data.table(
										win.vals = orig_X
										, label = cbind(orig_X.bins, orig_X.bins + use.bin - 1) |>
												apply(1, \(x) sprintf(fmt = .fmt, sort(x)[1], sort(x)[2]))
										)
								}

				attr(.map$label, "bin.map") <- .map[order(win.vals), .(win.vals = list(range(win.vals))), by = label][, purrr::map(.SD, `attributes<-`, value = NULL)];
				attr(.map$label, "bin.size") <- use.bin;

				if (as.factor){ factor(.map$label, levels = attr(.map$label, "bin.map")$label, ordered = TRUE) } else { .map$label }
			});

	output <- func(i, label_format) |> structure(dim = .dims, dimnames = .dnames)

	if (silently){ invisible(output) } else { output }
}
#
create_dims <- function(n, d){
#' Create Dimension Specs
#'
#' @param n The total number of values
#' @param d The number of dimensions
#'
#' @return A \code{d}-column row-ordered matrix of possible dimensions or (invisibly) a message if no dimensions can be found.
#'
#' @examples
#' k <- create_dims(n = 20, d = 3)
#' # Create an array with dimensions sampled from `k`:
#' array(sample(x = 20, size = prod(k[1, ]), replace = TRUE), dim = k[sample(nrow(k), 1), ])
#'
#' k <- create_dims(n = 56, d = 4)
#' # Create an array with dimensions sampled from `k`:
#' array(sample(x = 20, size = prod(k[1, ]), replace = TRUE), dim = k[sample(nrow(k), 1), ])
#'
#' @export

	# Use factors of `n` to create a search space of possible dimensions:
	x <- book.of.utilities::factor.int(n);

	# Find all combinations of `x` with length `d` whose product is `n` ...
	.tmp <- combinat::combn(x, d, simplify = FALSE) |>
		purrr::keep(\(i) prod(i) == n)

	if (rlang::is_empty(.tmp)){
		msg <- glue::glue("No dimensions of {d} factors can be found for {n} values");
		message(msg);
		return(invisible(msg))
	}

	.tmp |>
		# ... generate permutations of each combination retained ...
		purrr::map(\(z) combinat::permn(z) |> purrr::reduce(rbind)) |>
		# ... and combine into a single row-ordered array
		purrr::reduce(rbind) |>
		unique() |>
		magrittr::set_attr("dimnames", list(NULL, paste0("dim", 1:d)));
}
#
logic_map <- function(fvec, avec = 1L, bvec = NULL, logical.out = FALSE, regex = FALSE, chatty = FALSE, ...){
#' Logical Occurrence Map
#'
#' \code{logic_map} conducts an existence test of a vector or list of tuples (\code{fvec}) against a vector of unique values (\code{bvec}) optionally mapping a supplied vector of values (\code{avec}) to all \code{TRUE} results.
#'
#' @param fvec (vector) Values to be tested: may be a simple vector or a list of tuples
#' @param avec (vector) Optional vector of numeric values to project \emph{a}cross the result (must be the same length as \code{fvec} or length-1)
#' @param bvec (vector) A vector of unique values forming the basis of comparison  If given as a named list, the output will preserve the names when creating columns; otherwise, the values of \code{bvec} are used as the names.
#' @param logical.out (logical, vector) When \code{TRUE}, the output consists of logical values; when a vector of length two (2) is supplied, the first value returns on \code{FALSE}, the second on \code{TRUE}; otherwise, the values of \code{avec} are used.  If the vector form is used, only the first value at each position is used to supply the choices.
#' @param regex (logical | \code{FALSE}) When \code{TRUE}, argument \code{bvec} is interpreted as patterns against which case-sensitive matches are sought are attempted.  This forces the value of \code{test} to invoke \code{\link[data.table]{like}}.
#' @param ... See \code{\link[string]{stri_detect_regex}}: arguments \code{str} and \code{pattern} are internally passed and not needed.
#'
#' @note \code{length(fvec) == length(avec)}
#'
#' @note \code{fvec} and \code{bvec} must be of compatible types for comparison.
#'
#' @section \strong{Warning}:
#' When combining with a source object, \code{fvec} must \strong{NOT} be sorted during the function call or the values will not map correctly in the output.
#'
#' @return An array, the column names being the values of \code{bvec} or names of \code{bvec} if they exist.
#'
#' @export

	# Target classes:
	classes <- c(NA, "numeric", "integer", "factor", "character", "list");

	# Possible argument combinations:
	logic_map.options <- { expand.grid(
		fvec_class = classes
		, avec_class = classes[-length(classes)]
		, bvec_is_list = c(T,F)
		, empty_bvec = c(T,F)
		, any_empty_bvec_names = c(T,F)
		, regex = c(T,F)
		, logical.out = c(T,F)
		, stringsAsFactors = FALSE
		)
	} |> data.table::as.data.table();

	# Exclusions:
	exclude_these <- rlang::exprs(
		empty_bvec & (any_empty_bvec_names | bvec_is_list)
		, regex & fvec_class %in% c("numeric", "integer", "list")
		);

	logic_map.options <- logic_map.options[!Reduce(x = exclude_these, f = \(x, y) eval(x) | eval(y)), ] |>
		data.table::setkey();

	# Actions:
	these_actions <- list(
		bvec = \(empty_bvec = FALSE, bvec_is_list = FALSE, any_empty_bvec_names = FALSE){
			if (bvec_is_list){
				bvec <<- rlang::list2(!!!bvec[!duplicated(unlist(bvec))]) |>
					unlist() |>
					sort();

				bvec_is_list <<- is.list(bvec);

				these_actions$bvec(
					empty_bvec = empty_bvec
					, bvec_is_list = bvec_is_list
					, any_empty_bvec_names = any_empty_bvec_names
					);
			} else if (any_empty_bvec_names){
				bvec <<- sort(bvec);
				these_actions$bvec_names();
			} else if (empty_bvec){
				if (fvec_class == "factor"){
					bvec <<- levels(fvec);
				} else if (fvec_class == "character"){
					bvec <<- unique(fvec) |> sort() |> rlang::set_names();
				} else if (fvec_class == "list"){
					bvec <<- unique(unlist(fvec)) |> rlang::set_names();
				} else {
					bvec <<- unique(fvec) |> sort();
				}
			} else {
				these_actions$bvec_names();
			}

			invisible();
		}
		, bvec_names = \(){ #browser();
				.names <- if (hasName(attributes(bvec), "names")){ names(bvec) } else { unique(bvec) }
				.idx <- which(.names == "");
				.names[.idx] <- bvec[.idx] |> unlist();
				bvec <<- rlang::set_names(bvec, .names) |> unlist();
				invisible();
			}
		);

	# Argument handling:
	fvec_class <- classes[match(class(fvec), classes, nomatch = 1)];
	avec_class <- classes[match(class(avec), classes, nomatch = 1)];
	empty_bvec <- rlang::is_empty(bvec);
	any_empty_bvec_names <- any(names(bvec) == "");
	bvec_is_list <- is.list(bvec);

	# Process Arguments:
	logic_map.options[
		eval(rlang::expr(list(
			fvec_class = !!fvec_class
			, avec_class = !!avec_class
			, bvec_is_list = !!bvec_is_list
			, empty_bvec = !!empty_bvec
			, any_empty_bvec_names = !!any_empty_bvec_names
			, regex = !!regex
			, logical.out = !!logical.out
			)))
		, these_actions$bvec(empty_bvec, bvec_is_list, any_empty_bvec_names)
		];

	# Execute and return:
	out <- sapply(bvec, \(i){
		f <- if (regex){
					\(str, pattern, ...) stringi::stri_detect_regex(str = str, pattern = pattern, ...)
				} else {
					`%in%`
				}

		res <- if (fvec_class == "list"){
				# browser()
				sapply(fvec, \(x) f(x, i) |> any())
			} else { f(fvec, i) }

		if (logical.out){
			res
		} else if (is.numeric(avec)){
			res * avec
		} else {
			av <- vector(mode = "list", length = length(avec));
			av[res] <- as.vector(avec)[res];
			av
		}
	});
	array(out, dim = dim(out), dimnames = list(NULL, names(bvec)));
}
#
make.date_time <- function(add_vec = 1:7, var_start = Sys.Date(), var_form = "%Y-%m-%d 00:00:00", var_tz = "", var_interval = "days"){
#' Date-Time Sequence Generator
#'
#' \code{make.date_time} serves as a wrapper for \code{\link[stringi]{stri_datetime_add}}
#'
#' @param add_vec (numeric): Vector of integer values to add
#' @param var_start (datetime): Vector of integer values used to generate datetime results
#' @param var_form (string): Format for the datetime values
#' @param var_tz (string): Timezone for the datetime values
#' @param var_interval (string): The unit of time for \code{add_vec}
#'
#' @return A temporal vector
#'
#' @family Data Generation
#'
#' @export

	stringi::stri_datetime_add(time = as.Date(var_start), value = add_vec, unit = var_interval, tz = var_tz) |> format(var_form)
}
#
make.quantiles <- function(x, ..., as.factor = FALSE){
#' Quantiles Transformation
#'
#' \code{make.quantiles} is a wrapper for \code{\link[stats]{quantile}} replacing the input with calculated values.  Unmatched quantiles will return \code{NA} in order to match the length of the input vector.
#'
#' @param x The input vector
#' @param ... (\code{\link[rlang]{dots_list}}): Additional arguments sent to \code{\link[stats]{quantile}}.  Defaults are as follows:\cr
#' \itemize{
#' \item{\code{probs = seq(0, 1, 0.25)}}
#' \item{\code{na.rm = FALSE}}
#' \item{\code{names = TRUE}}
#' \item{\code{type = 7}}
#' \item{\code{digits = 7}}
#' \item{\code{...}}
#' }
#' @param as.factor (logical) Should the output be returned as a factor?
#'
#' @note Any indeterminate probabilities relative to the input will return \code{NA}
#'
#' @return A quantile representation of the input
#'
#' @examples
#' sample(900, 20) |> (\(x) data.frame(x = x, q = book.of.features::make.quantiles(x, seq(0, 1, .1), as.factor = TRUE)))()
#' sample(900, 20) |> (\(x) data.frame(x = x, q = book.of.features::make.quantiles(x, seq(0, 1, .1), as.factor = FALSE)))()
#'
#' @export

	args <- rlang::exprs(...);
	args$x <- rlang::expr(!!x);

	if (as.factor){ args$names <- rlang::expr(TRUE) }

  q.vec <- do.call(quantile, eval(args))

	.out <- outer(x, q.vec, `<=`)

	.out <- apply(.out, 1, \(x){
				idx <- names(q.vec)[which(x) |> min()];

				if (as.factor){ idx } else { q.vec[idx] }
			});

	if (as.factor){
		factor(.out, levels = names(q.vec), , ordered = TRUE)
	} else { .out }
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
#'   \item{\code{window.size}: The size of each partition (W)}
#'   \item{\code{increment}: The number of items to increment before selecting the next W items (W + i)}
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
sigmoid <- function(input, family = "logistic", center = mean, ...){
#' {0,N} Sigmoid Scaler
#'
#' \code{sigmoid} Scales the input to a range of {0,N} using the sigmoid function
#'
#' The input must contain positive and negative values when \code{centered == FALSE}
#'
#' Function arguments for each sigmoid family are as follows:
#' \enumerate{
#'   \item{\code{"generalized"}: \code{list(A, K, C, Q, B, v)}}
#'   \item{\code{"logistic"}: \code{list(L, K)}}
#'   \item{\code{"gompertz"}: \code{list(A, B, C)}}
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
#' @importFrom book.of.utilities %bin% factor.int %tf%
#' @importFrom stringi %s+%
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom utils str
#' @import data.table
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
