## preprocessing.R
##
## Pre-processing of Vis-NIR spectra
##

## Apply for spectra
apply_spectra <- function(obj, fun, ...) {
  nir <- aaply(spectra(obj), 1, fun, ...)

  # Managing case where only one spectra
  if (nrow(obj) == 1)
    nir <- matrix(nir, nrow = 1, dimnames = list(ids(obj), names(nir)))

  spectra(obj) <- nir
  obj
}

## SNV
## Barnes et al., 1989
##
snv <- function(x){
  (x - mean(x))/sd(x)
}

## RNV
## Guo et al, 1999
rnv <- function(x, r){
  pct <- as.numeric(quantile(x = x, probs = r, na.rm = TRUE))
  (x - pct)/(sd(x[x <= pct]))
}

## Savitzky-Golay Filter
#library(signal)
#foo <- apply_spectra(s, sgolayfilt, p = 4, n = 33, m = 1)
#
# ---- Using custom Savitzky-Golay Filter (inspired by: http://tolstoy.newcastle.edu.au/R/help/04/02/0385.html)
# sav.gol <- function(nir, fl = 33, forder=4, der=0){
#                     ## -- nir: Spectral Data (matrix) [1 spectra per row]
#                     ## -- fl: Filter length (must be odd)
#                     ## -- forder: Filter order
#                     ## -- dorder: Derivative Order
#     # -- Outpout: Processed Spectra will be stored in res matrix
#     res=NULL
#     nir=as.matrix(nir)
#     # -- Each Spectra is smoothed separatedly
#     for (row in 1:nrow(nir)){
#         T <- as.vector(nir[row,])
#         m <- length(T)
#         dorder <- der + 1
#         # -- calculate filter coefficients --
#         fc <- (fl-1)/2 # index: window left and right
#         X <- outer(-fc:fc, 0:forder, FUN="^") # polynomial terms and coefficients
#         # -- calculate X pseudoinverse
#         s <- svd(X)
#         Y <- s$v %*% diag(1/s$d) %*% t(s$u)
#         # -- filter via convolution and take care of the end points --
#         T2 <- convolve(T, rev(Y[dorder,]), type="o") # convolve(...)
#         T2 <- T2[(fc+1):(length(T2)-fc)]  
#         # -- Store smoothed spectra in res
#         res=rbind(res,T2)
#     }
#     # -- Formatting Data
#     rownames(res) <- NULL
#     names(res) <- names(nir)
#     res
# }

## Baseline using the baseline package

baseline.Spectra <- function(spectra, method = 'irls', ...) {
  object <- spectra
  new_nir <- baseline(spectra(object), method = method, ...)
  spectra(object) <- getCorrected(new_nir)
  object
}

if (!isGeneric('baseline'))
  setGeneric('baseline', function(spectra, method = 'irls', ...)
    standardGeneric('baseline')
)

setMethod('baseline', 'Spectra', baseline.Spectra)

#' Continuum removal
#'
#' Operates a continuum removal on the spectra in the collection.
#' This operation is commonly done  to normalize reflectance spectra 
#' and allow comparison of individual absorption features from a 
#' common baseline. The removal is based on the upper convex hull of
#' the spectra.
#'
continuum_removal <- function(x, wl = as.numeric(names(x))){
  ch.index <- sort(chull(x = wl, y = x))
  ch <- data.frame(wl = wl[ch.index], nir = x[ch.index])
  res <- approx(x = ch$wl, y = ch$nir, xout = wl)
#   res2 <- signal::interp1(ch$wl, ch$nir, wl, method = 'linear', extrap = TRUE)
  x - res$y
}
