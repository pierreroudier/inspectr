## preprocessing.R
##
## Pre-processing of Vis-NIR spectra
##

## Apply for spectra




#' Apply a function on the spectra of a Spectra* object
#' 
#' Aggregates spectral and data information of a \code{Spectra} object using a
#' user-defined function.
#' 
#' Apply a function and update the spectra of a \code{Spectra} object. This
#' function is particularly interesting for pre-processing, e.g to derive the
#' spectra, or apply pre-processing functions such as \code{\link{snv}}.
#' 
#' The philosophy of this function is to let the user free to use any function
#' to pre-process a spectra collection, using either functions from the stats
#' package, functions from other packages such as \code{signal}, or personal
#' functions.
#' 
#' @param obj an object inheriting from class \code{Spectra}
#' @param fun an aggregation function
#' @param ... expressions evaluated in the context of \code{fun}
#' @return An object of the same class as \code{obj}
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{aggregate_spectra}}, \code{\link{snv}},
#' \code{\link{rnv}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Second derivative
#' r <- apply_spectra(australia, diff, 2)
#' plot(r)
#' 
#' # Smoothing kernel
#' k <- kernel("daniell", 20) # define a kernel
#' r <- apply_spectra(australia, kernapply, k)
#' plot(r)
#' 
#' \dontrun{
#' # Savitzky-Golay filter (from the signal package)
#' library(signal)
#' r <- apply_spectra(australia, sgolayfilt, n = 33, p = 4)
#' plot(r)
#' }
#' 
#' @export apply_spectra
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




#' Standard and Robust Normal Variate transformations
#' 
#' Standard and Robust Normal Variate transformations are often used in
#' chemometrics to normalise a spectra collection and remove the baseline
#' effect.
#' 
#' The Standard Normal Variate transformation (SNV, Barnes et al., 1989) is a
#' common method to reduce within-class variance.
#' 
#' The Robust Normal Variate transformation (RNV, Guo et al., 1999) is a
#' modification of the SNV to make it more robust to closure problems.
#' 
#' These function are to be used in conjonction with
#' \code{\link{apply_spectra}}.
#' 
#' @aliases snv rnv
#' @param x a vector of numeric values
#' @param r the percentile to use in the RNV computation
#' @return A vector of numeric values
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{apply_spectra}}, \code{\link{baseline}}
#' @references \itemize{ \item Barnes, R.J., Dhanoa, M.S., Lister, S.J. 1989.
#' Standard normal variate transformation and detrending of near-infra-red
#' diffuse reflectance spectra. Applied Spectroscopy 43, 772--777.
#' 
#' \item Guo, Q., Wu, W., Massar, D.L. 1999. The robust normal variate
#' transform for pattern recognition with near-infrared data. Analytica Chimica
#' Acta 382:1--2, 87--103.  }
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Standard Normal Variate transform
#' s <- apply_spectra(australia, snv)
#' plot(s)
#' 
#' # The scale function in the base package is actually doing
#' # the same thing!
#' s <- apply_spectra(australia, scale, center = TRUE, scale = TRUE)
#' plot(s)
#' 
#' # Robust Normal Variate transform
#' s <- apply_spectra(australia, rnv, r = 0.25)
#' plot(s)
#' 
#' @export snv
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

if (!isGeneric('base_line'))
  setGeneric('base_line', function(object, ...)
    standardGeneric('base_line')
)

setMethod('base_line', 'Spectra', function(object, ...) {
  .try_require('baseline')
  nir <- spectra(object)
  new_nir <- baseline:::baseline(nir, ...)
  spectra(object) <- getCorrected(new_nir)
  object
})





#' Continuum removal
#' 
#' Operates a continuum removal on a vector.
#' 
#' This operation is commonly done to normalize reflectance spectra and allow
#' comparison of individual absorption features from a common baseline. The
#' removal is based on the upper convex hull of the spectra.
#' 
#' This function is working on vectors. It may applied on matrix or data.frames
#' using the \code{apply} function, or on \code{Spectra*} objects using the
#' \code{apply_spectra} function.
#' 
#' @param x a numeric vector
#' @param wl the wavelengths of the spectra
#' @param upper if TRUE, removes the upper convex hull from the spectra, if
#' FALSE, takes the lower convex hull
#' @return A numeric vector with its continuum removed.
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}, based on code from
#' Raphael Viscarra-Rossel.
#' @seealso \code{\link{baseline}}, \code{\link{snv}}, \code{\link{rnv}}
#' @references Clark, R.N., and Roush, T.L. 1984. Reflectance spectroscopy:
#' Quantitative analysis techniques for remote sensing applications. Journal of
#' Geophysical Research 89, 6329--6340.
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' s <- apply_spectra(australia, continuum_removal)
#' plot(s)
#' 
#' s <- apply_spectra(australia, continuum_removal, upper = FALSE)
#' plot(s)
#' 
#' @export continuum_removal
continuum_removal <- function(x, wl = as.numeric(names(x)), upper = TRUE){
  
  if (!upper) x <- -1*x
  
  # Compute convex hull
  ch_idx <- chull(x = wl, y = x)
  # Close the polygon
  ch_idx <- c(ch_idx, ch_idx[1])
  # Put in a data.frame
  ch <- data.frame(wl = wl[ch_idx], nir = x[ch_idx])
  
  # We don't want the polygon, just the line above the spectra
  # We will select only the bit that goes from wl min to wl max
  
  idx_min <- which(ch$wl == min(ch$wl))[1]
  idx_max <- which(ch$wl == max(ch$wl))[1]
  
  if (idx_min > idx_max) {
    idx_select <- c(idx_min:nrow(ch), 1:idx_max)
  } else {
    idx_select <- idx_min:idx_max
  }
  ch <- ch[idx_select,]
  
  # Linear interpolation
  res <- approx(x = ch$wl, y = ch$nir, xout = wl)

  # Remove continuum
  x - res$y
}
