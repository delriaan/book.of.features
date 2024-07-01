formulate <- function(lhs = NULL, ...){
#' Make A Formula
#'
#' \code{formulate} takes the inputs and assembles a \code{\link[stats]{formula}} object
#'
#' @param lhs (string|symbol) The left-hand side of the formula
#' @param ... (dots-list) Elements to use on the right-hand side of the formula
#'
#' @section \code{...}:
#' \itemize{
#' \item{Length-1 elements are treated as terms}
#' \item{Vectors are reduced as individual terms}
#' \item{Refer to \code{\link[stats]{formula}} for valid syntax}
#' }
#'
#' @return A formula
#'
#' @examples
#' formulate(score)
#' formulate(score, )
#' formulate(score, -that)
#' formulate(score, this)
#' formulate(score, this:that)
#' formulate(score, this*that)
#' formulate(score, this, -that, c(that:other, high*low))
#'
#' @family Data Generation
#'
#' @export

	f <- ~1

	# Argument-handling: ----
	lhs <- substitute(lhs)
	rhs <- rlang::enexprs(...)
	rhs <- if (rlang::is_empty(rhs)){
			rlang::exprs(.)
		} else {
			rlang::enexprs(...)
		}
	if (all(grepl("[-]", as.character(rhs)))){
		rhs <- rlang::exprs(., !!!rhs)
	}

	# Update `f`: ----
	# Left-hand side:
	if (!rlang::is_empty(lhs)){
		rlang::f_lhs(f) <- lhs
	}

	# Right-hand side:
	rlang::f_rhs(f) <- purrr::reduce(rhs, \(x, y){
		op <- as.list(y)
		# `op[[1]]` will be a call (including primitives) when `op` is
		# coerced into a list:

		if (op[[1]] == rlang::expr(`-`)){
			if (rlang::is_empty(x)){ x <- rlang::expr(`.`) }
			rlang::expr(!!x - !!y[[2]])
		} else if (op[[1]] == rlang::expr(`*`)){
			rlang::expr(!!x + !!y)
		} else {
			if (is.call(y)){
				y <- formulate(, !!!(as.list(y)[-1])) |> rlang::f_rhs()
				# browser()
				str2lang(glue::glue("{deparse(x)} + {deparse(y)}"))
			} else {
				rlang::expr(!!x + !!y)
			}
		}
	})

	# Return `f`: ----
	return(f)
}

# formulate(score)
# formulate(score, )
# formulate(score, -that)
# formulate(score, this)
# formulate(score, this:that)
# formulate(score, this*that)
# formulate(score, this, -that, c(that:other, high*low))

