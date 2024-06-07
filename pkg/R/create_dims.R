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
