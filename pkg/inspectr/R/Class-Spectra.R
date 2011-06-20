#' Class for spectra collections
#'
#' @slot wl object of class "\code{numeric}"; the wavelengths at which the spectra has been measured
#' @slot nir object of class "\code{matrix}"; the spectra, with as many columns as wavelengths, and as many rows as samples
#' @slot id object of class "\code{data.frame}" with one attribute; the identification strings for each sample in the collection
#' @slot units object of class "\code{character}"; units in which the wavelengths are expressed
#' @seealso \code{\link{spectra}}, \code{\link{wl}}, \code{\link{SpectraDataFrame-class}}
#' @author Pierre Roudier \email{pierre.roudier@@gmail.com}
#' @rdname Spectra-class
#' @exportClass Spectra
setClass(
  Class="Spectra",
  representation=representation(
    wl='numeric',
    nir='matrix',
    id='data.frame',
    units="character"
  ),
  prototype=prototype(
    wl=numeric(),
    nir=matrix(),
    id=data.frame(NA),
    units=as.character(NA)
  ),
  validity = function(object) {
    # if the wl are given as an integer vector they are translated into a numeric vector
    # for clarity (only one type to manage)
    if (is(object@wl, "integer"))
      object@wl <- as.numeric(object@wl)
    if (!inherits(object@wl, "numeric"))
      stop("wl should be of class integer or numeric")
    if ((nrow(object@id) > 0) & (nrow(object@nir) != nrow(object@id)))
      stop("number of individuals and number of rows in the spectra matrix don't match")
    if ((length(object@wl > 1) & (ncol(object@nir) != length(object@wl))))
      stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
    if (length(unique(object@id[,1])) != nrow(object@id))
      stop("The ids of the samples need to be unique")
    return(TRUE)
  }
)
