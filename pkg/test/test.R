library(plyr)

## a version of letters() that works for n > 26
##
# infinite_letters <- function(x, caps = "low") {
#   if !(caps %in% c('low', 'high'))
#     stop("Bad caps=... option.")
# 
#   if (x <= 26) {
#     if (caps == 'low')
#       res <- letters[1:x]
#     else
#       res <- LETTERS[1:x]
#   }
#   else {
#     n <- floor(x / 26)
#     p <- x %% 26
#     
#   }
# }

# meta parameters
n_samples <- 25
sample_ids <- letters[1:n_samples]
wl <- 350:500
n_wl <- length(wl)
n_sites <- 8
site_ids <- sample(LETTERS[1:n_sites], replace = TRUE, size = n_samples)
site_locations <- 

nir_col <- data.frame(
  wl, 
  matrix(runif(n_wl*n_samples), ncol = n_samples, dimnames=list(NULL, sample_ids))
)

nir_row <- data.frame(
  id = sample_ids,
  matrix(runif(n_wl*n_samples), nrow = n_samples, dimnames=list(NULL, as.character(wl)))
)

soil_plots <- ddply(data.frame(site_ids), 'site_ids', 
  function(x) {
    X <- rep(runif(1), length.out = nrow(x))
    Y <- rep(runif(1), length.out = nrow(x))
    top <- seq(0, 100, length.out = nrow(x))
    lag <- ifelse(length(top) > 1, top[length(top)] - top[length(top) - 1], 10)
    bottom <- top + lag 
    data.frame(x = X, y = Y, top = top, bottom = bottom)
  }
)

df <- data.frame(
  id = sample_ids,
  soil_plots,
  foo = runif(n_samples),
  bar = sample(c("Some", "Categorical", "Data"), replace = TRUE, size = n_samples)
  )

test_data <- join(df, nir_row, by = 'id')

## TESTS ##

foo <- nir_row
spectra(foo) <- id ~ ...
features(foo, safe = TRUE, key = 'id') <- df
