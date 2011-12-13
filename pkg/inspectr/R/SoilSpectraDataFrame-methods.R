"SoilSpectraDataFrame" <- function(
  # Spectra stuff
  nir, 
  wl = numeric(), 
  id = data.frame(id=NA), 
  units = as.character(NA),
  # Data
  data,
  # Spatial stuff (optional)
  coords, 
  coords.nrs = numeric(0), 
  proj4string = CRS(as.character(NA)), 
  match.ID = TRUE, 
  bbox = NULL,
  # Soil stuff
  depths = list(matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c('top', 'bottom'))))
  {

  ## Maybe Spatial stuff not mandatory here
  if (!is(coords, "SpatialPoints"))
    coords = SpatialPoints(coords, proj4string = proj4string, bbox = bbox)

  if (!is(nir, "Spectra"))
    nir = Spectra(wl = wl, nir = nir, id = id, units = units)

  new("SoilSpectraDataFrame", coords, nir, data = data, depths = depths)
}

## soilplot<- setter
##
## plot_id ~ top + bottom
##
setReplaceMethod("sampling_site", signature(object = "SpectraDataFrame", value = "ANY"),
  function(object, value) {

    data <- features(object)

    if (inherits(value, "formula")) {
      cc = model.frame(value, data) # retrieve plot, top and bottom cols

      # If not plot ID is given, 
      # we create one based on duplicated spatial coordinates
      if (dim(cc)[2] == 2) {
        stop('unsupported for the moment - please provide plot ID')
#         nm = as.character(as.list(value)[[2]])[2:3]
#         coord.numbers = match(nm, names(data))
      }
      else if (dim(cc)[2] == 3) {
        nm = c(as.character(as.list(as.list(value)[[3]])[2:3]) , as.character(as.list(value)[[2]]))
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

    ## Hack from here
    # given plot_col, top_col and bottom_col
    depths <- dlply(data, plot_col, function(x) subset(x, select = c(top_col, bottom_col)))

    SoilSpectraDataFrame(coords = cc, data = data, coords.nrs = stripped, object, match.ID = FALSE, depths = depths)
  }
)