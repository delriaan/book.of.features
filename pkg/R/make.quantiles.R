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
