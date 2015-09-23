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




#' Retrieves or sets the wavelengths of a \code{Spectra*} object.
#' 
#' Either retrieves the wavelengths from a \code{Spectra*} object, or creates a
#' \code{Spectra*} object from a \code{"data.frame"} object by setting some of
#' its columns as the wavelengths.
#' 
#' When applied to a \code{Spectra*} object, this functions simply returns the
#' wavelengths of the spectra it is storing.
#' 
#' If applied on a \code{"data.frame"} object, it is an helper function to
#' create a \code{Spectra*} object. It then needs to be indicated the
#' wavelengths at which the spectra values are measured. The assumption is that
#' each row of the \code{"data.frame"} is a spectra, and the column names of
#' the \code{"data.frame"} contain the wavelengths values.
#' 
#' If all the columns are used to create the \code{Spectra*} object, a
#' \code{Spectra} object is created. If some attributes are left, they will be
#' used to generate a \code{SpectraDataFrame} object.
#' 
#' @name wl
#' @aliases wl wl<- wl,Spectra-method wl<-,Spectra-method
#' wl<-,data.frame-method
#' @docType methods
#' @param object a \code{"data.frame"} or an object inheriting from class
#' \code{Spectra}
#' @param value the wavelengths of the \code{Spectra*} object to create
#' @param ... Ignored
#' @return If applied on a \code{"data.frame"}, either a \code{Spectra} or a
#' \code{SpectraDataFrame} object. If applied on a \code{Spectra*} object, a
#' vector.
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{spectra}}, \code{\link{Spectra-class}},
#' \code{\link{SpectraDataFrame-class}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Retrieving wavelengths from Spectra* object
#' wl(australia)
#' 
#' # Replacing wavelength values - USE WITH CAUTION!
#' wl(australia) <- 1:length(australia)
#' wl(australia)
#' 
#' # Use to initiate a Spectra* object from a data.frame
#' data(australia)
#' wl(australia) <- 350:2500
#' ids(australia) <- ~ sr_no
#' summary(australia)
#' 
#' @export wl
    wl = 'numeric',
    nir = 'matrix',
    id = 'data.frame',
    units = "character"
  ),
  prototype = prototype(
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA)
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

#' Class for spectra collections with associated attributes
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
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA),
    data = data.frame()
  ),
  validity = function(object) {
    if (ncol(object@data) == 0)
      stop("data.frame is empty: use Spectra() to create spectra-only object")
    if (nrow(object@data) != nrow(object@nir))
      stop("number of rows in data.frame and spectra don't match")
    return(TRUE)
  }
)

#' Class for spectra collections that have
#' associated spatial point locations
#'
setClass(
  Class='SpatialSpectra',
  representation=representation(
    'SpatialPoints',
    'Spectra'
  ),
  prototype=prototype(
    bbox = matrix(NA),
    proj4string = CRS(as.character(NA)),
    coords = matrix(NA),
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA)
  )
)

#' Class for spectra collections that have
#' associated attributes and spatial point locations
#'
#' @slot wl object of class "\code{numeric}"; the wavelengths at which the spectra has been measured
#' @slot nir object of class "\code{matrix}"; the spectra, with as many columns as wavelengths, and as many rows as samples
#' @slot id object of class "\code{data.frame}" with one attribute; the identification strings for each sample in the collection
#' @slot units object of class "\code{character}"; units in which the wavelengths are expressed
#' @slot data object of class data.frame containing the attribute data (may or may not contain the coordinates in its columns)
#' @slot bbox object of class "matrix"; bounding box
#' @slot proj4string object of class "CRS"; projection string
#' @slot coords object of class "matrix"; the coordinates matrix (points are rows in the matrix)
#' @slot coords.nrs object of class logical; if TRUE, when the object was created the coordinates were retrieved from the data.frame, and hence stripped from it; after coercion to data.frame, e.g. by as.data.frame(x), coordinates will again be added (as ﬁrst few columns) to the data.frame
#' @seealso \code{\link{spectra}}, \code{\link{wl}}, \code{\link{coordinates}}, \code{\link{SpectraDataFrame-class}}
#' @author Pierre Roudier \email{pierre.roudier@@gmail.com}
#' @rdname SpatialSpectraDataFrame-class
#' @exportClass SpatialSpectraDataFrame
#' @import sp
setClass(
  Class = 'SpatialSpectraDataFrame',
  representation = representation(
    'SpatialPoints',
    'SpectraDataFrame'
  ),
  prototype = prototype(
    bbox = matrix(NA),
    proj4string = CRS(as.character(NA)),
    coords = matrix(NA),
    coords.nrs = numeric(0),
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA),
    data = data.frame()
  )
)
