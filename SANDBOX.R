# xform.basis_vector ----
data("WorldPhones")

fvec <- sample(colnames(WorldPhones), 500, TRUE)
avec <- rbinom(n = length(fvec), size = 4, prob = 0.7)
bvec <- sample(fvec, 5) |> rlang::set_names() |> sort()
bvec <- c(Africa = "Afr", Asia = "Asia", Europe = "Eur", Oceania = "Oc") |> sort()
# fun <- purrr::as_mapper(~factor(.x) %*% t(factor(.y)))

# outer(fvec, bvec, fun)
list(f = fvec, {
	.str_sim = stringdist::stringsimmatrix(fvec, bvec, method = "jw", useNames = "names")

	t(apply(.str_sim, 1, function(i){ ifelse(i == max(i), 1, 0) })) * avec
})

n <- 20

fvec <- list(
	ex_1 = sample(LETTERS[1:10], n, TRUE)
	, ex_2 = sample(LETTERS[seq(2,20,2)], n, TRUE)
	, ex_3 = transpose(list(sample(LETTERS[seq(2,20,2)], n, TRUE), sample(LETTERS[seq(2,20,2)], n, TRUE)))
	, ex_4 = colors() %>% sample(4) %>% sample(n, TRUE)
	, ex_5 = list(
						colors() %>% sample(4) %>% sample(n, TRUE)
						, sample(letters[seq(2,20,2)], n, TRUE)
						) %>% transpose() %>% map(unlist, recursive = FALSE)
	);

avec <- sample(seq(1, 30, 0.75), n, TRUE);
bvec <- list(
	set_1 = fvec$ex_2 %>% unique() %>% sample(5)
	, set_2 = sprintf("[%s%s]{1,3}", sample(letters, 10), sample(letters, 10))
	);

map(fvec[1:3], xform.basis_vector, avec = avec, bvec = bvec$set_1);
map(fvec[1:3], xform.basis_vector, avec = avec, bvec = bvec$set_1, logical.out = TRUE);

data.table(fvec$ex_4, xform.basis_vector(fvec$ex_4, avec = avec))
data.table(fvec$ex_4, xform.basis_vector(fvec$ex_4, avec = avec, bvec = bvec$set_2, regex = TRUE))
data.table(fvec$ex_5, xform.basis_vector(fvec$ex_5, avec = avec, bvec = bvec$set_2, regex = TRUE))
data.table(fvec$ex_5, xform.basis_vector(fvec$ex_5, avec = avec, bvec = bvec$set_2, regex = TRUE, logical.out = TRUE))

# ----
x <- sample(1000, 100)/1000
x <- sort(x)
p <- plotly::plot_ly()

purrr::map(
	c("guder", "logis", "atan", "tanh", "gen", "gom"), ~{
		y = do.call(sigmoid
					, args = list(input = seq_along(x), family = .x, k = 0.2, center = median))

		p <<- plotly::add_lines(
				p = p, x = x, y = y
				, hovertext = sprintf("x: %s\ny: %s", x, y)
				, type = "scatter", line = list(shape = "spline"), name = .x
				)
	})

data.frame(x = x, y = sigmoid(x, family = "log"))

# make.windows() ----
series <- sample(10, 97, TRUE)
output <- list()
window.size <- 10
increment <- 3
post <- function(i){ lapply(i, `*`, 3)}

while(length(series) > 0){
	output <- c(output, list(series[1:window.size] %>% .[!is.na(.)]))
	series <- series[-c(1:increment)]
}

post(output)
# continuity ----
x <- data.table(
	g = sample(LETTERS[1:4], 2000, TRUE)
	, i = sample(100, 2000, TRUE)
	)[, j := i + sample(100, 2000, TRUE)]

setorder(x, g, i, j)
