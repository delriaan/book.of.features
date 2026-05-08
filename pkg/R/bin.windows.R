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
	#' @param use.bin The bin size to use: When empty, the smallest prime number or integer factor in \code{i} within the range of \code{i} is used.
	#' @param as.factor (logical) Should the output be converted into a factor?
	#' @param label_format (string) A two-argument string compatible with \code{\link[base]{sprintf}} that controls label output
	#' @param silently (logical) Should the output return invisibly?
	#' @param ... (not used)
	#'
	#' @importFrom book.of.utilities %bin%
	#' @return A character (or factor) vector the length of the input, as "binned" representations.  If the input is dimensional, an array of the same dimensions is returned
	#'
	#' @note Factor output is only available for heterogeneous data structures.
	#'
	#' @family Data Transformation
	#'
	#' @export

	if (rlang::has_length(i, 1)){
		i <- matrix(0:i, ncol = 1)
	} else if (rlang::has_length(i, 2) && rlang::is_empty(dim(i))){
		i <- i[1]:i[2]
	}

	`%bin%` <- book.of.utilities::`%bin%`
	.bin_fun <- \(.x){
		lb <- .x %bin% use.bin
		ub <- lb + use.bin - 1
		out <- sprintf("<%s,%s>", lb, ub)

		if (as.factor){
			out <- factor(out, unique(out[order(.x)]), ordered = TRUE)
		}

		out
	}

	vals <- i
	attributes(vals) <- NULL

	bins <- if (!is.atomic(vals)){
			purrr::modify(vals, .bin_fun)
		} else { .bin_fun(vals)	}

	attributes(bins) <- attributes(i)

	if (silently){ invisible(bins) } else { bins }
}

