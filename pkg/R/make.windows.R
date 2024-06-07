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
