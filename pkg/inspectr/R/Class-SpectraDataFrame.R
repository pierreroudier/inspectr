#' Class for spectra collections
#'
#' @slot wl object of class "\code{numeric}"; the wavelengths at which the spectra has been measured
#' @slot nir object of class "\code{matrix}"; the spectra, with as many columns as wavelengths, and as many rows as samples
#' @slot id object of class "\code{data.frame}" with one attribute; the identification strings for each sample in the collection
#' @slot units object of class "\code{character}"; units in which the wavelengths are expressed
#' @slot data object of class data.frame containing the attribute data
#' @seealso \code{\link{spectra}}, \code{\link{wl}}, \code{\link{Spectra-class}}
#' @author Pierre Roudier \email{pierre.roudier@@gmail.com}
#' @rdname SpectraDataFrame-class
#' @exportClass SpectraDataFrame
setClass(
  Class='SpectraDataFrame',
  representation=representation(
    "Spectra",
    data='data.frame'
  ),
  prototype=prototype(
    wl=numeric(),
    nir=matrix(),
    id=data.frame(NA),
    units=as.character(NA),
    data=data.frame()
  ),
  validity = function(object) {
    if (ncol(object@data) == 0)
      stop("data.frame is empty: use Spectra() to create spectra-only object")
    if (nrow(object@data) != nrow(object@nir))
      stop("number of rows in data.frame and spectra don't match")
    return(TRUE)
  }
)