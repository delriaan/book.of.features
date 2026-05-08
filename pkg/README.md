---
engines:
- path: /home/delriaan/.positron-server/bin/ae29de6747f9f15cc38783410ec9a46a5b428393/quarto/share/extension-subtrees/julia-engine/\_extensions/julia-engine/julia-engine.js
toc-title: Table of contents
---

# ![book](book_small.png) Book of Features

`book.of.features` seeks to facilitate design-oriented targets during
the feature engineering phase for simple datasets.

## Installation

Use
`remotes::install_github("delriaan/book.of.features", subdir = "pkg")`
to install the latest version from GitHub.

## Functions

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `bin.windows()`                     Creates labeled bins from integer
                                      input, with optional ordered factor
                                      output.

  `create_dims()`                     Generates all valid $d$-dimensional
                                      factor combinations whose product
                                      equals $n$.

  `formulate()`                       Builds a formula object from
                                      supplied left- and right-hand-side
                                      terms.

  `logic_map()`                       Produces a one-hot/logical
                                      occurrence map (optionally sparse)
                                      from values or tuples.

  `make.date_time()`                  Generates date-time sequences by
                                      adding interval offsets to a start
                                      date/time.

  `make.quantiles()`                  Replaces values with their matched
                                      quantile cut values or quantile
                                      labels.

  `sigmoid()`                         Scales numeric input using
                                      selectable sigmoid-family
                                      transformations.
  -----------------------------------------------------------------------
