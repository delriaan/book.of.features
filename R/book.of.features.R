#' @title Book of Features Package
#'
#' @description
#' \code{book.of.features} provides feature-engineering helper functions.
#'
#' @importFrom book.of.utilities %bin% %::% %?% %??% factor.int
#' @importFrom data.table %like% %ilike% like %between%
#' @importFrom magrittr %>%
#' @importFrom stringi %s+%
#' @importFrom foreach %do% %dopar%
#'
#' @name Book of Features Package
NULL
# usethis::use_proprietary_license(copyright_holder = "Chionesu George")
# if (dir(pattern = "yml") |> length() == 0){ usethis::use_pkgdown() }
# pkgdown::build_site()

sigmoid <- function(input, family = "logistic", center = mean, debug = FALSE, ...){
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

	if (debug) (print(list(...)));

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
logic_map <- function(fvec, avec = rep(1, length(fvec)), bvec = sort(unique(fvec)), cmp_test = `==`, logical.out = FALSE, regex = FALSE, chatty = FALSE){
#' Logical Test Occurrence Map
#'
#' \code{logic_map} conducts a test of a vector or list of tuples against a vector of unique values. The test can be one of \code{identity}, pattern-matching, or some custom function with Boolean output. Parallelism is supported with a registered \code{\link[foreach]{foreach}} backend.  If no backend is registered, \code{\link[foreach]{registerDoSEQ}} is used as a default.
#'
#' @param fvec (vector) Values to be tested: may be a simple vector or a list of tuples
#' @param avec (vector) Optional vector of numeric values to project \emph{a}cross the basis vector
#' @param bvec (vector) A vector of unique values forming the basis of comparison  If given as a named list, the output will preserve the names when creating columns; otherwise, the values of \code{bvec} are used as the names.
#'
#' @param cmp_test (function|\code{`==`}) A two-valued function resulting in a logical vector.  The first argument \strong{must} be vectorized over \code{bvec} and able to compare each element in \code{fvec} to \code{bvec}. If \code{'test'} is provided as the argument, the same functionality as setting argument \code{regex} to \code{TRUE} is invoked.
#' @param logical.out (logical, vector) When \code{TRUE}, the output consists of logical values; when a vector of length two (2) is supplied, the first value returns on \code{FALSE}, the second on \code{TRUE}; otherwise, the values of \code{avec} are used.  If the vector form is used, only the first value at each position is used to supply the choices.
#' @param regex (logical | \code{FALSE}) When \code{TRUE}, argument \code{bvec} is interpreted as patterns against which case-sensitive matches are sought are attempted.  This forces the value of \code{test} to invoke \code{\link[data.table]{like}}.
#' @param chatty (logical | \code{FALSE}) When \code{TRUE}, additional information is printed to console
#'
#' @section \strong{Restriction}:
#' \code{length(fvec) == length(avec)}
#'
#' @section \strong{Warning}:
#' When combining with the source object, \code{fvec} must \strong{NOT} be sorted during the function call or the values will not map correctly in the output.
#'
#' @return Invisibly, a data.table object, the column names being the values of \code{bvec} or names of \code{bvec} if they exist
#'
#' @export

	if (!foreach::getDoParRegistered()){ foreach::registerDoSEQ() }
	if (regex|(identical(cmp_test, "regex"))){
		cmp_test <- if (regex){ function(bv, fv){ purrr::map_lgl(bv, ~any(fv %like% .x)) }} else { `==` }
	}
	out.names = if (is.null(names(bvec))){ as.character(bvec) } else { names(bvec) }

	bvec = purrr::reduce(bvec, c);

	if (chatty){ print(list(fvec = c(fvec), avec = c(avec), bvec = bvec, out.names = out.names)) }

	foreach::foreach(
		fv = fvec
		, av = avec
		, .combine = rbind
		, .multicombine = TRUE
		, .final = function(i){
				# The default value to use when no levels of 'bvec' are found in 'fvec'
				.dflt = ifelse(logical.out, FALSE, 0);
				if (all(i == 0)){ i <- rep.int(.dflt, length(out.names) * nrow(i)) %>% matrix(ncol = length(out.names)) }

				# data.table conversion
				if (!data.table::is.data.table(i)){ i <- data.table::as.data.table(i) }
				data.table::setnames(i, out.names)
			}
		, .packages = c("data.table", "magrittr", "purrr")
		, .export = c("logical.out", "bvec", "out.names", "cmp_test")
		) %dopar% {
			action = rlang::exprs({
				if (logical.out) { c(FALSE, TRUE) } else { c(0, av) }}[cmp_test(bvec, fv) + 1]
				, sapply(logical.out, first)[cmp_test(bvec, fv) + 1]
				, c(0, 1)[cmp_test(bvec, fv) + 1]
				)[[min(which(c(is.logical(logical.out), length(logical.out) >= 2, TRUE)))]];
			eval(action)
		}
}
#
make.windows <- function(series, window.size, increment = 1, post = eval, debug = FALSE, ...) {
#' Serial Window Maker
#'
#' \code{make.windows} takes as its first argument a series of values and produces a list of partitioned sets referred to as a "window"
#'
#' @param series A list or vector object from which incremental subsets (windows) of a fixed size are chosen
#' @param window.size (integer) The size of the subset (window) to select
#' @param increment (integer) The number of elements by which iteration should advance
#' @param post (function) A post-processing function on the return object having class "data.table" and single column "window"
#' @param debug (logical | FALSE) When \code{TRUE}, additional information is printed to console for debugging purposes
#'
#' Sets are the result of forward-moving partitioning:
#' \enumerate{
#'   \item \code{window.size}: The size of each partition (W)
#'   \item \code{increment}: The number of items to increment before selecting the next W items (W + i)
#' }
#'
#' @return A serialized collection-list of partitions (windows), each window containing a subset of size \code{window.size}
#'
#' @export

	if (length(series) == 1){
		message(sprintf("Series length is one (1): repeating %s times ...", window.size));
		return(data.table(window = list(rep.int(series, window.size))))
	} else if (window.size > length(series)) {
		message(sprintf(
			"Window size (%s) > series length (%s): setting to series length"
			, window.size
			, length(series)
			));

		window.size = length(series)
	};

	output = list();
	while(length(series) > 0){
		.svec <- series[1:window.size]
		output <- c(output, list(.svec[!is.na(.svec)]))
		series <- series[-c(1:increment)]
	}

	post(output);
}
#
bin.windows <- function(i = 1, use.bin = NULL, min.factor = 1){
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
#' @param use.bin (integer) The bin size to use: should be greater than zero (0), overrides the internal effects of argument \code{min.factor}.
#'
#' @param min.factor (integer) The minimum factor of of \code{i} allowed when \code{use.bin} is less than or equal to one (1).  This becomes the bin size.
#'
#' @return A character vector the length of the input, as "binned" representations.  If the input is dimensional, an array of the same dimensions is returned
#'
#' @export

	.dir <- FALSE;
	func = function(ii)	{
		ii <- as.integer(ii)

		j <- if (rlang::has_length(ii, 1)){ 0:abs(ii)
				} else if (rlang::has_length(ii, 2)){
					.dir <- ii[2] < ii[1]; seq(ii[1], ii[2], by = sign(ii[2]-ii[1]))
				} else { ii }

		this.bin = if (rlang::is_empty(use.bin)){
			factor.int(max(i, na.rm = TRUE)) %>% purrr::keep(~(.x >= min.factor)|(rlang::has_length(i, 2)))
		} else { unique(sort(j %bin% use.bin)) }

		.map = c(-Inf
						, min(this.bin, na.rm = TRUE) - 1
						, this.bin
						, suppressWarnings(purrr::keep(diff(this.bin) + this.bin + 1, ~.x <= max(this.bin, na.rm = TRUE)))
						, max(this.bin, na.rm = TRUE) + 1
						, Inf
						) %>%
						sort(decreasing = .dir) %>%
						make.windows(2, 2) %>%
						data.table::data.table(purrr::map(., ~{
							ifelse(
								any(is.infinite(.x))
								, ifelse(
										.x[which(is.infinite(.x))] > 0
										, c(">", "="[rlang::is_empty(use.bin)], " ") %>%
											paste(collapse = "") %s+% max(.x[!is.infinite(.x)])
										, c("<", "="[rlang::is_empty(use.bin)], " ") %>%
											paste(collapse = "") %s+% min(.x[!is.infinite(.x)])
										)
								, paste(unlist(.x), collapse = " to ")
								)
							})) %>%
						data.table::setnames(c("win.vals", "label"));

		.out = purrr::map_chr(j, ~{
			.val = .x;
			.label = .map[, label[[which(purrr::map_lgl(win.vals, ~(.val %between% as.list(sort(.x)))))]]]
			if (identical(.label, character())){ "oob"} else { .label }
		});

		attr(.out, "bin.map") <- .map
		.out
	}

	if (rlang::is_empty(dim(i)) | rlang::has_length(dim(i), 1)){
		func(as.vector(i))
	} else {
		.dns = dimnames(i);
		.dms = dim(i);

		.out = apply(X = i, MARGIN = length(.dms), FUN = bin.windows, use.bin = use.bin, min.factor = min.factor, simplify = TRUE) ;
		if (length(.dms) == 2){ .out <- t(.out) }

		as.array(.out) %>% structure(dim = .dms, dimnames = .dns);
	}
}
#
make.date_time <- function(add_vec = 1:7, var_start = Sys.Date(), var_form = "%Y-%m-%d 00:00:00", var_tz = "", var_interval = "days"){
#' Date-Time Sequence Generator
#'
#' \code{make.date_time} serves as a wrapper for \code{\link[stringi]{stringi_datetime_add}}
#'
#' @param add_vec (numeric | c(1:7)): Vector of integer values to add
#' @param var_start (datetime | Sys.Date()): Vector of integer values used to generate datetime results
#' @param var_form (string | "\%Y-\%m-\%d 00:00:00"): Format for the datetime values
#' @param var_tz (string | ""): Timezone for the datetime values
#' @param var_interval (string | "days"): The unit of time for \code{add_vec}
#'
#' @export

	stringi::stri_datetime_add(time = as.Date(var_start), value = add_vec, unit = var_interval, tz = var_tz) %>% format(var_form)
}
#
continuity <- function(srcData, mapFields, timeFields, timeout = GAP > timeout, boundaryName = "episode", archipelago = TRUE, show.all = FALSE, debug	= FALSE){
#' Continuity Creator
#'
#'  \code{continuity} is conceptually based on the \href{https://www.red-gate.com/simple-talk/sql/t-sql-programming/the-sql-of-gaps-and-islands-in-sequences/}{'islands & gaps'} concept.
#'
#' @param srcData (object): The source dataset, including all non-sessioning fields desired
#'
#' @param mapFields (string): A comma-separated string literal containing field names that will partition `data`
#'
#' @param timeFields (string): A comma-separated string literal containing field names to use as "start" and "stop" temporal indices.  If only one value is given, that value will be repeated as the "stop" index
#'
#' @param timeout: The largest allowable 'gap' in a series of time values before a new 'island' begins: can be a quoted expression that conditionally determines the value.
#'
#' @param boundaryName (string): The name root of the boundary column names (e.g., "episode" -> "episode_start_idx", "episode_end_idx")
#'
#' @param archipelago (logical | TRUE): Should the output include the islands and gaps generated?
#'
#' @param show.all (logical | FALSE): Should the output include all of the columns of the output?
#'
#' @return A data.table with columns <mapFields>, ..., timeOut, ISLAND, island_idx, where '...' is empty if `show.all` is FALSE
#'
#' @export
	# :: Helper function to split a string-literal argument
	sub_fn = function(i){ stringi::stri_split_regex(i, "[,; ]", simplify = TRUE, omit_empty = TRUE) %>% as.vector() };

	# :: Helper function to transform date types and factors into integers
	dt.check_fn	= function(i){
		cl = class(i);
		{ if (any(cl %in% c("Date", "POSIXlt"))){
			(i - as.Date("1900-01-01")) %>% as.integer()
			} else if (any(cl %in% c("POSIXct", "character"))) {
				(as.Date(i, origin = "1900-01-01") - as.Date("1900-01-01")) %>% as.integer()
				} else if (any(cl %in% c("double", "numeric"))) {
					i %>% round(0) %>% as.integer()
					} else { as.integer(i) }
		} %>% unlist()
	};

	boundaryName	= as.character(rlang::enexpr(boundaryName));
	mapFields			= sub_fn(mapFields);
	orderFields		= c(mapFields, "start_idx");
	optionalOutput= c(i	= "ISLAND"
										, lb = sprintf("%s_start_idx", boundaryName)
										, ub	= sprintf("%s_end_idx", boundaryName)
										);
	outputFields	= c(mapFields
									 , "island_idx"
									 , if (archipelago) { optionalOutput } else { "" }
									 ) %>% unique();
	timeout 			= substitute(timeout);
	timeout 			= switch(class(timeout)
										, "numeric" = rlang::expr(GAP > !!timeout)
										, "call" = timeout
										, "character" = str2lang(timeout)
										, timeout
										);

	# :: When only one value exists after parsing argument `timelineFields`, set `stop_idx` to a forward-shifted version of the same field plus the value of `eval(timeout)`
	timeFields = sub_fn(timeFields) %>% as.vector();
	data.table::setattr(timeFields, "is_single", length(timeFields) == 1);

	outData = data.table::as.data.table(srcData);

	# @note "start_idx" and "stop_idx" are essential fields
	outData[, c("start_idx", "stop_idx") := purrr::map(.SD[, c(timeFields), with = FALSE], magrittr::freduce, list(dt.check_fn, unlist, as.vector))];

	sanity.check = { c(
		is_DT = data.table::is.data.table(outData)
		, start_idx.exists = "start_idx" %in% ls(outData)
		, stop_idx.exists = "stop_idx" %in% ls(outData)
		)}

	if (!all(sanity.check)){
		paste0(message(paste(sanity.check[which(!sanity.check)], collapse = ", ")," failed sanity checks ..."));
		return("Execution failed.");
	}

	data.table::setkeyv(outData, eval(c(mapFields, timeFields %>% data.table::first())));

	outData[
	, # +{stop_idx, rec_idx, last_rec_idx} | Upper time index; record index; last record index flag
		`:=`(
			stop_idx = { if (attr(timeFields, "is_single")){
		    	.logi_vec = diff(c(0, start_idx)) %>% as.integer() < eval(timeout);
		    	.choices = c(data.table::shift(start_idx, fill = last(start_idx) + eval(timeout), type = "lead")) %::% c(start_idx + eval(timeout))
		    	# output value test
		    	ifelse(.logi_vec, .choices$true, .choices$false)
		   	} else { stop_idx }
			}
		, rec_idx = sequence(length(start_idx))
		, last_rec_idx = rep(FALSE, length(start_idx))
		)
	, by = c(mapFields)
	];

	if (debug) {
		return (
			list(
				mapFields					= mapFields
				, timeFields			= timeFields
				, is_single				= attr(timeFields, "is_single")
				, orderFields			= orderFields
				, optionalOutput	= optionalOutput
				, outputFields		= outputFields
				, outData_str			= outData %>% str
				, outData_is.data.table = data.table::is.data.table(outData)
				, outData_dim			= dim(outData)
				)
			);
		};

	# :: Create new columns holding the difference of from/to dates, and the resulting values for ISLAND and GAP
	outData[, seg := 1:length(rec_idx), by = c(mapFields)
	][, data.table::setorderv(.SD, orderFields)
	][ # Gap precursor: column-wise sequential differences within start and stop indices using `diff()`
	, c("delta_start", "delta_stop") := purrr::map(list(start_idx, stop_idx), ~diff(c(data.table::first(.x), .x)))
	, by = c(mapFields)
	][
	# Gap: From one record to the next in a partitioned, ordered set: { stop[n] - stop[n-1] } - [stop - start]
	# Need a visual for this in help file
	, GAP := (1 * (seg > 1)) * (delta_stop - (stop_idx - start_idx))
	, by = rec_idx
	][
	# Correct GAPS with overlapping boundaries { start[n] < stop[n-1] }
	(GAP < 0), `:=`(start_idx  = start_idx + GAP, stop_idx = stop_idx  + GAP)
	][(GAP < 0) | (is.na(GAP)), GAP := 0
	][# This is THE ESSENTIAL part of the routine as it labels the islands:
	, island_idx := {
		 #	 ep_idx MUST be initialized before session() (see below) is called iteratively
		 ep_idx  = 1;
		 # "session()" is a function delegate and is the ESSENTIAL part of the routine
		 #	Note how it is contained WITHIN this specific code block and is designed for conditional increment
		 #	See http://adv-r.had.co.nz/Functional-programming.html, section "Mutable State"
		 session = function(tf) {
		 		if (tf) {
		 			ep_idx <<- ep_idx + 1; ep_idx
		 		} else {
		 			ep_idx <<- ep_idx; ep_idx
		 		}
		 	}
		 sapply(eval(timeout), session)
	}
	, by = c(mapFields)
	][
	# Set the optional fields to be returned based on the value for argument `archipelago` (default `TRUE`)
	# Derive values for  ISLAND, episode_start_idx, and episode_end_idx by grouping: G ~ mapFields + island_idx
	, c(eval(optionalOutput)) := list(
			# ISLAND
			max(stop_idx %>% as.numeric(), na.rm = TRUE) - min(start_idx %>% as.numeric(), na.rm = TRUE)
			# episode_start_idx
			, min(start_idx %>% as.numeric(), na.rm = TRUE)
			# episode_end_idx
			, max(stop_idx %>% as.numeric(), na.rm = TRUE)
			)
	, by = eval(c(mapFields, "island_idx"))
	][, partition := .GRP, by = c(mapFields)
	][(ISLAND == 0), ISLAND := 1
	][, .SD[, if (show.all) { c(1:(colnames(.SD) %>% length())) } else { outputFields }, with = FALSE] %>% unique()]
}

#' @export
xform.basis_vector <- logic_map

#' @export
xform.sigmoid <- sigmoid

#' @export
make.islands <- continuity