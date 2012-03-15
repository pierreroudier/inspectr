"SoilSpectraDataFrame" <- function(
    # Spectra stuff
    nir, 
    coords = matrix(NA),
    wl = numeric(), 
    id = data.frame(id=NA), 
    units = as.character(NA),
    # Data
    data,
    # Spatial stuff (optional)
    # coords, 
#     coords.nrs = numeric(0), 
    proj4string = CRS(), 
    match.ID = TRUE, 
    bbox = NULL,
    # Soil stuff
    depths = list(matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c('top', 'bottom')))),
    profiles_id = data.frame(NA)
  ) {

  # Creating a SpectraDataFrame object
  if (!is(nir, "Spectra")) {
    if (!is(nir, "SpectraDataFrame")) {
      nir = Spectra(wl = wl, nir = nir, id = id, units = units, data = data)
    }
  }

  # Creating a SpatialPoints object (posibly void)
  if (!is(coords, "SpatialPoints")) {
    if (!is.na(coords)) {
      coords <- SpatialPoints(coords, proj4string = proj4string, bbox = bbox)
    }
    else { # If SpatialSpectraDataFrame
      if (is(nir, "SpatialPoints")) {
        coords <- as(nir, "SpatialPoints")
      }
      else { # void Spatial slot
        coords <- new("SpatialPoints")
      }
    }
  }

  sdf <- as(nir, "SpectraDataFrame")
  ssp <- new(".SpatialSoilProfiles", 
    new(".SoilProfiles", 
      depths = depths, 
      profiles_id = profiles_id
    ), 
    coords
  )

  new("SoilSpectraDataFrame", sdf, ssp)
}

## profiles<- setter
##
## profiles_id ~ top + bottom
##

if (!isGeneric('profiles<-'))
  setGeneric('profiles<-', function(object, value, ...)
    standardGeneric('profiles<-')
)

setReplaceMethod("profiles", signature(object = "SpectraDataFrame", value = "ANY"),
  function(object, value) {

    data <- features(object)

    if (inherits(value, "formula")) {
      cc <- model.frame(value, data) # retrieve plot, top and bottom cols

      # If not plot ID is given, 
      # we create one based on duplicated spatial coordinates
      if (dim(cc)[2] == 2) {
        stop('unsupported for the moment - please provide plot ID')
#         nm = as.character(as.list(value)[[2]])[2:3]
#         coord.numbers = match(nm, names(data))
      }
      else {
        if (dim(cc)[2] == 3) {
#           nm <- c(as.character(as.list(as.list(value)[[3]])[2:3]) , as.character(as.list(value)[[2]]))
#           coord.numbers = match(nm, names(data))
          profiles_id <- names(cc)[1]
          top <-  names(cc)[2]
          bottom <-  names(cc)[3]
        } 
        else {
          stop("Bad formula.")
        }
      }
    }
    else
      stop("Please use formula interface.")

    cc <- data.frame(ids(object), cc)
    samples_id <- names(cc)[1]
    
    soil <- SoilProfiles(cc, 
      samples_id = samples_id, 
      profiles_id = profiles_id, 
      top = top, 
      bottom = bottom)
    browser()
    new("SoilSpectraDataFrame", object, soil)
  }
)
