## transformations.R
##
## Pre-processing of Vis-NIR spectra
##

## SNV
## Barnes et al., 1989
##
# snv <- function(x){
#   .snv <- function(x){(x - mean(x))/sd(x)}
#   nspec <- nrow(x)
#   if (is.null(nspec)){
#     res <- .snv(x)
#   } 
#   else {
#     require(plyr)
#     res <- aaply(.data=x, .margins=1, .fun=.snv)
#   }
#   res
# }

## RNV
## Guo et al, 1999
# rnv <- function(x, r){
#   .rnv <- function(x, r){
#     pct <- as.numeric(quantile(x=x, probs=r, na.rm=TRUE))
#     (x - pct)/(sd(x[x <= pct]))
#   }
#   nspec <- nrow(x)
#   if (is.null(nspec)){
#     res <- .rnv(x, r)
#   } 
#   else {
#     require(plyr)
#     res <- aaply(.data=x, .margins=1, .fun=.rnv, r=r)
#   }
#   res
# }

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
    ch <- data.frame(wl=wl(obj)[ch.index], nir=sp[i, ch.index])
    ch <- approx(x=ch$wl, y=ch$nir, xout=wl(obj))
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
