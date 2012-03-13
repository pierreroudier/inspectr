## transformations.R
##
## Pre-processing of Vis-NIR spectra
##

## Apply for spectra
apply_spectra <- function(obj, fun, ...) {
  nir <- aaply(spectra(obj), 1, fun, ...)
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
  setGeneric('baseline', function(spectra, method, ...)
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
#' @param obj an object inheriting from Spectra
#' @return an object of same class with the continuum removed from its spectra.
#' @references R.N. Clark and T.L. Roush (1984).Reflectance spectroscopy: 
#' Quantitative analysis techniques for remote sensing applications, 
#' Journal of Geophysical Research 89 (B7), pp. 6329–6340.
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
continuum_removal <- function(obj){

# # using plyr
#   require(plyr)
#   require(reshape)
#   res <- aaply(.data=spectra(obj), .margins=1, .fun=function(x) {
#     ch.index <- sort(chull(x))
#     ch <- data.frame(wl=wl(obj)[ch.index], nir=x[ch.index])
#     ch <- approx(x=ch$wl, y=ch$nir, xout=wl(obj)) 
#     x - ch$y
#     })

  # using a for loop
  sp <- spectra(obj)
  res <- sp

  for (i in 1:nrow(sp)) {
    ch.index <- sort(chull(sp[i,]))
    ch <- data.frame(wl = wl(obj)[ch.index], nir = sp[i, ch.index])
    ch <- approx(x = ch$wl, y = ch$nir, xout = wl(obj))
    res[i, ] <- sp[i, ] - ch$y
  }

  obj@nir <- res
  obj
}

## from Raphael
# continuum.removal <- function(obj, wavelength=wl(obj), method="linear"){
#   require(signal)
# 
#   spc <- spectra(obj)
#   cr <- spc
# 
#   for (i in 1:nrow(spc)){
#     id <- sort(chull(wavelength, spc[i,]))
#     cr[i, ] <- spc[i,] - interp1(wavelength[id], spc[i,id], wavelength, method=method, extrap=TRUE)
#   }
#   obj@nir <- cr
#   obj
# }

## for baseline, see the baseline package

## what about derivatives? See KernSmooth package. (locpoly function)
