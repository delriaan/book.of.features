logic_map <- function(fvec, avec = 1L, bvec = NULL, logical.out = FALSE, regex = FALSE, chatty = FALSE, ...){
#' Logical Occurrence Map
#'
#' \code{logic_map} conducts an existence test of a vector or list of tuples (\code{fvec}) against a vector of unique values (\code{bvec}) optionally mapping a supplied vector of values (\code{avec}) to all \code{TRUE} results. The "vanilla" run mode produces at one-hot encoded version of \code{fvec}, but it can produce augmented versions of of such depending on the structure of \code{fvec}.
#'
#' @param fvec (vector) Values to be tested: may be a simple vector or a list of tuples
#' @param avec (vector) Optional vector of numeric values to project \emph{a}cross the result (must be the same length as \code{fvec} or length-1)
#' @param bvec (vector) A vector of unique values forming the basis of comparison  If given as a named vector, the output will preserve the names when creating columns; otherwise, the values of \code{bvec} are used as the names.
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
#' @examples
#' x <- LETTERS[1:10]
#' logic_map(x)
#'
#' k <- c("steelblue2", "tan3", "gray47", "gray27", "gray60")
#' x <- replicate(5, sample(k, 2) |> sort(), simplify = FALSE)
#' v <- sample(50, length(x), TRUE)
#' k
#' v
#' str(x)
#' # Vanilla:
#' # One-hot, default names:
#' logic_map(x)
#' # One-hot, custom names:
#' logic_map(x, bvec = k)
#' # One-hot, custom names, logical output:
#' logic_map(x, bvec = k, logical.out = TRUE)
#' # One-hot, custom names, regex matched, logical output:
#' logic_map(x, bvec = c(alpha = "blue", beta = 60, delta = "gray"), regex = TRUE, logical.out = TRUE)
#' # One-hot, valued, custom names, regex matched, numeric output:
#' logic_map(x, avec = v, bvec = c(alpha = "blue", beta = 60, delta = "gray"), regex = TRUE)
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
				} else { `%in%` }

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