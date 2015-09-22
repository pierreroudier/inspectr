#' @include SpectraDataFrame-methods.R
# roxygen()

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
