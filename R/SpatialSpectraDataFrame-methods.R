#' @include SpectraDataFrame-methods.R

#' @title Constructor for the SpatialSpectraDataFrame class.
#' @name SpatialSpectraDataFrame-class
#' @description Constructor for the SpatialSpectraDataFrame class. Creates a
#' SpatialSpectraDataFrame object from scratch.
#' 
#' 
#' @param wl a numeric vector giving the wavelengths at with the spectra have
#' been measured
#' @param nir a \code{"matrix"} or a \code{"data.frame"} object giving the
#' spectra values for each sample
#' @param id a vector giving the unique id of each sample in the collection
#' @param units a character giving the unit in which the wavelengths values are
#' expressed
#' @param data object of class \code{"data.frame"} containing the attribute
#' data
#' @param coords numeric matrix or data.frame with coordinates (each row is a
#' point)
#' @param proj4string projection string of class \link{CRS-class}
#' @param bbox bounding box matrix, usually NULL and constructed from the data,
#' but may be passed through for coercion purposes if clearly needed
#' @param coords.nrs numeric; if present, records the column positions where in
#' \code{data} the coordinates were taken from (used by \link{coordinates<-})
#' @param match.ID logical; if TRUE AND coords has rownames (i.e., coerced to a
#' matrix, \code{dimnames(coords)[[2]]} is not NULL), AND data has row.names
#' (i.e. is a data.frame), then the \code{SpatialSpectraDataFrame} object is
#' formed by matching the row names of both components, leaving the order of
#' the coordinates in tact. Checks are done to see whether both row names are
#' sufficiently unique, and all data are matched. If FALSE, coordinates and
#' data are simply "glued" together. If character: indicates the column in
#' \code{data} with coordinates IDs to match
#' @return a new \code{"SpatialSpectraDataFrame"} object
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{spectra}}, \code{\link{wl}},
#' \code{\link{SpatialSpectraDataFrame-class}}
#' @examples
#' 
#'   # Creating a SpatialSpectraDataFrame object from scratch
#'   my.wl <- 350:2500
#'   my.id <- LETTERS[1:4]
#'   my.nir <- matrix(runif(4*length(my.wl)), nrow=4)
#'   my.data <- data.frame(foo = runif(4), bar = LETTERS[1:4])
#'   my.coords <- matrix(runif(4), runif(4), ncol=2, nrow=4)
#' 
#'   my.ssdf <- SpatialSpectraDataFrame(wl = my.wl, nir = my.nir, id = my.id, data = my.data, coords = my.coords)
#' 
#' @export SpatialSpectraDataFrame
"SpatialSpectraDataFrame" <- function(coords, nir, data,
  coords.nrs = numeric(0), proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox=NULL,
  wl=numeric(), id=data.frame(id=NA), units="nm") {

  if (!is(coords, "SpatialPoints"))
    coords = SpatialPoints(coords, proj4string = proj4string, bbox=bbox)

  if (!is(nir, "Spectra"))
    nir = Spectra(wl=wl, nir=nir, id=id, units=units)

  new("SpatialSpectraDataFrame", coords, nir, data=data)
}

## coordinates<- setter
##
## Adapted from sp package code
##
setGeneric('coordinates<-', function(object, value) standardGeneric('coordinates<-'))

#' Sets spatial coordinates to create \code{SpatialSpectraDataFrame} objects
#' 
#' Sets spatial coordinates to create \code{SpatialSpectraDataFrame} objects.
#' Adapted from the \code{"coordinates<-"} method in the \bold{sp} package.
#' 
#' 
#' @name coordinates
#' @aliases coordinates<- coordinates<-,SpectraDataFrame-method
#' @docType methods
#' @param object A \code{SpectraDataFrame} object
#' @param value Spatial coordinates: either a matrix, list, or data frame with
#' numeric data, or column names, column number or a reference: a formula (in
#' the form of e.g. \code{~x+y}), column numbers (e.g. \code{c(1,2)}) or column
#' names (e.g. \code{c("x","y")}) specifying which columns in \code{object} are
#' the spatial coordinates. If the coordinates are part of object, giving the
#' reference does not duplicate them, giving their value does duplicate them in
#' the resulting structure.
#' @return A \code{SpatialSpectraDataFrame} object
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}, from code from the
#' \bold{sp} package.
#' @seealso \code{\link{spectra}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # The australia dataset does not have associated coordinates.
#' # To fake it is a spatial dataset, we will associate each spectra
#' # in the collection to a location in the meuse dataset provided
#' # by the sp package
#' 
#' # Loding the meuse.grid dataset from the sp package
#' library(sp)
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' 
#' # We randomly generate sampling locations on this grid
#' coords <- spsample(meuse.grid, n=nrow(australia)*2, type="random")
#' class(coords)
#' 
#' # We associate these locations to the spectra in the collection
#' #
#' 
#' # First, we add the coordinates in the data slot data
#' features(australia, safe=FALSE) <- as.data.frame(coords)[1:nrow(australia), ]
#' names(australia)
#' 
#' # Then we promote the SpectraDataFrame object to a SpatialDataFrame object
#' coordinates(australia) <- ~x+y
#' class(australia)
#' str(australia)
#' 
setReplaceMethod("coordinates", signature(object = "SpectraDataFrame", value = "ANY"),
  function(object, value) {
    coord.numbers <- NULL
    data <- features(object)

    if (inherits(value, "formula")) {
      cc = model.frame(value, data) # retrieve coords
      if (dim(cc)[2] == 2) {
        nm = as.character(as.list(value)[[2]])[2:3]
        coord.numbers = match(nm, names(data))
      }
      else if (dim(cc)[2] == 3) {
        nm = c(as.character(as.list((as.list(value)[[2]])[2])[[1]])[2:3], as.character(as.list(value)[[2]])[3])
        coord.numbers = match(nm, names(data))
      } # else: give up.
    }

    else if (is.character(value)) {
      cc = data[, value] # retrieve coords
      coord.numbers = match(value, names(data))
    }

    else if (is.null(dim(value)) && length(value) > 1) { # coord.columns?
      if (any(value != as.integer(value) || any(value < 1)))
        stop("coordinate columns should be positive integers")

      cc = data[, value] # retrieve coords
      coord.numbers = value
    }

    else  # raw coordinates given; try transform them to matrix:
      cc = coordinates(value)

    if (any(is.na(cc)))
      stop("coordinates are not allowed to contain missing values")

    if (!is.null(coord.numbers)) {
      data = data[ , -coord.numbers, drop = FALSE]
      stripped = coord.numbers
      # ... but as.data.frame(x) will merge them back in, so nothing gets lost.
      if (ncol(data) == 0)
        #stop("only coords columns present: use SpatialSpectra to create a points object without data")
        return(SpatialSpectra(cc))
    }
    else
      stripped = numeric(0)

    SpatialSpectraDataFrame(coords = cc, data = data, coords.nrs = stripped, object, match.ID = FALSE)
  }
)

## coercition methods

as.data.frame.SpatialSpectra = function(x, ...)  {
  coords <- as.data.frame(as(x, 'SpatialPoints'), check.names = FALSE)
  nir <- as.data.frame(spectra(x), check.names = FALSE)
  id <- ids(x, as.vector = FALSE)
  
  if ('data' %in% slotNames(x)) {
    df <- data.frame(id, coords, features(x), nir)
  }
  else {
    df <- data.frame(id, coords, nir)
  }
  df
}

setAs("SpatialSpectra", "data.frame", function(from)
	as.data.frame.SpatialSpectra(from))

## data manipulation
# subset.SpatialSpectraDataFrame <- function(x, subset, select, drop = FALSE, ...) {
#   # adapted from subset.data.frame
#   df <- features(x)
# 
#   if (missing(subset))
#         r <- TRUE
#   else {
#     e <- substitute(subset)
#     r <- eval(e, df, parent.frame())
# 
#     if (is.logical(r))
#       r <- r & !is.na(r)
# #     else if (inherits(r, "Spatial")) {
# #       require(rgeos)
# #       which_intersects <- gIntersects(as(x, "SpatialPoints"), subset, byid = TRUE)
# #       r <- which(which_intersects)
# #     }
# #     else stop("subset must evaluate to either logical or Spatial")
#   }
# 
#   if (missing(select))
#     vars <- TRUE
#   else {
#     nl <- as.list(seq_along(df))
#     names(nl) <- names(df)
#     vars <- eval(substitute(select), nl, parent.frame())
#   }
# 
#   df_sub <- df[r, vars, drop = drop]
#   SpPts <- as(x, 'SpatialPoints')
#   SpPts <- SpPts[r, ]
# 
#   # remove unused factors
#   df_sub <- droplevels(df_sub)
#   id_selected <- which(rownames(df) %in% rownames(df_sub))
#   x <- SpatialSpectraDataFrame(SpPts, wl = wl(x), nir = spectra(x)[id_selected, , drop = FALSE], id = ids(x)[id_selected, 1, drop = FALSE], units = wl_units(x), data = df_sub)
#   x
# }
# 
# setMethod("subset", "SpatialSpectraDataFrame", subset.SpatialSpectraDataFrame)
