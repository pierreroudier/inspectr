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

nir_values <- runif(n_wl*n_samples)

nir_col <- data.frame(
  wl, 
  matrix(nir_values, ncol = n_samples, dimnames=list(NULL, sample_ids))
)

nir_row <- data.frame(
  id = sample_ids,
  matrix(nir_values, nrow = n_samples, dimnames=list(NULL, as.character(wl)), byrow = TRUE)
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

df <- df[sample(1:nrow(df), size=nrow(df)), ]

test_data <- join(df, nir_row, by = 'id')

## TESTS ##

# Spectra and SpectraDataFrame inits

# From long to wide format (in case you got data in cols)
foo <- nir_col
spectra_from_col(foo) <- wl ~ ...

# Create Spectra
bar <- nir_row
spectra(bar) <- id ~ ...
identical(foo, bar) # TRUE
# Promoting Spectra to SpectraDataFrame by adding external data
features(bar, safe = TRUE, key = 'id') <- df

# Create SpectraDataFrame directly from a data.frame
baz <- test_data
spectra(baz) <- id ~ ... ~ 350:500
identical(bar, baz) # FALSE, but just because sample orders are different

# rbind
baz_bar <- rbind(bar, baz, create_new_ids = TRUE)
# also works for Spectra
baz_bar <- rbind(as(bar, "Spectra"), as(baz, "Spectra"), create_new_ids = TRUE)

# Cut
# Select wl
test <- cut(bar, wl = 350:370)
plot(test)
# remove wl
test <- cut(bar, wl = -1*350:370)
plot(test)

# Apply function on spectra
a <- apply_spectra(bar, diff, 2)

# Spatial bindings
library(sp)
bar_sp <- bar
coordinates(bar_sp) <- ~x+y
proj4string(bar_sp) <- CRS("+init=epsg:4326")
bbox(bar_sp)
str(bar_sp)

# Soil init

