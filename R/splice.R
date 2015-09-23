## Code for splice-correction
##
#
# x a vector
# y the wavelengths 
# wl the wavelengths at which the splice correction should take place
#
# ASD version is using cubic splines
#
# The SWIR part of the spectrum (1000-1800 nm) is taken as a reference 
# for corrections as it is stable to the instrument sensitivity drift
# (Beal & Eamon, 2010)
#
# http://support.asdi.com/Document/Documents.aspx
#

# x and y giving the coordinates of the points to be interpolated
# x: wavelengths
# y: NIR value at these wavelengths
splice.numeric <- function(x, y, wl = c(725:1020, 1801:1950), method = "spline") {
  # Getting index of WLs to be interpolated
  idx <- which(x %in% wl)
  y_cutted <- y[-1*idx]
  x_cutted <- x[-1*idx]
  # Interpolation
  r <- interp1(x = x_cutted, y = y_cutted, xi = x, method = method)
  r
}

splice.Spectra <- function(x, wl = c(725:1020, 1801:1950), method = "spline") {
  nir <- apply_spectra(x, function(spec) {
    splice.numeric(x = wl(x), y = as.vector(spec), wl = wl, method = method)
  })
}

# if (!isGeneric("splice"))
setGeneric("splice", function(x, wl, method="spline")
  standardGeneric("splice"))

#' @title Splicing of spectra
#' @name splice 
#' @description This method mimicks the "splicing" method available in the ViewSpec Pro
#' software from ASD, which aims at correcting steps in the data.
#' 
#' It removes parts of the spectra defined by the \code{wl} vector, and
#' interpolates these parts using a method chosen using the \code{method}
#' option.
#' 
#' This function is a wrapper around \code{signal::interp1}.
#' 
#' @param x a \code{Spectra} object
#' @param wl the wavelengths to cut out and interpolate
#' @param method the interpolation method. Available options are "linear",
#' "nearest", "pchip", "cubic", and "spline".
#' @return an object of same class as x
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @examples
#' 
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' oz_spliced <- splice(australia, wl = c(725:1020, 1801:1950), method = "spline")
#' plot(oz_spliced)
#' 
setMethod("splice", 
          signature(x="Spectra"),
          function(x, wl = c(725:1020, 1801:1950), method = "spline") {
            splice.Spectra(x, wl, method)
          }
)
