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
