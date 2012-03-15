## SOIL CLASSES STUFF

# Hiden class describing soil profile support
setClass(
  Class = ".SoilProfiles",
  representation = representation(
    depths = 'list',
    profiles_id = 'list'
  ),
  prototype = prototype(
    profiles_id = list(character()),
    depths = list(
      matrix(nrow = 0, ncol = 2, 
        dimnames = list(NULL, c('top', 'bottom'))
      )
    )
  ),
  validity = function(object) {
      if (!identical(names(object@depths), names(object@profiles_id)))
        stop("Profiles ids are not consistent.")
      return(TRUE)
    }
)

# Hiden class describing soil profile support with associated spatial reference
setClass(
  Class = ".SpatialSoilProfiles",
  representation = representation(
    ".SoilProfiles",
    "SpatialPoints"
  ),
  prototype = prototype(
    profiles_id = list(),
    depths = list(matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c('top', 'bottom')))),
    bbox = matrix(NA),
    proj4string = CRS(),
    coords = matrix(NA)
  )
)

setClass(
  Class = 'SoilSpectra',
  representation = representation(
    'Spectra',
    '.SoilProfiles'
  ),
  prototype = prototype(
#     bbox = matrix(NA),
#     proj4string = CRS(),
#     coords = matrix(NA),
#     coords.nrs = numeric(0),
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA),
#     data = data.frame(),
    profiles_id = list(),
    depths = list(matrix(nrow=0, ncol=2, dimnames = list(NULL, c('top', 'bottom'))))
  )
)

setClass(
  Class = 'SpatialSoilSpectra',
  representation = representation(
    'SpatialPoints',
    'Spectra',
    '.SoilProfiles'
  ),
  prototype = prototype(
    bbox = matrix(NA),
    proj4string = CRS(),
    coords = matrix(NA),
#     coords.nrs = numeric(0),
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA),
#     data = data.frame(),
    profiles_id = list(),
    depths = list(matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c('top', 'bottom'))))
  )
)

setClass(
  Class = 'SoilSpectraDataFrame',
  representation = representation(
    'SpectraDataFrame',
    '.SoilProfiles'
  ),
  prototype = prototype(
#     bbox = matrix(NA),
#     proj4string = CRS(),
#     coords = matrix(NA),
#     coords.nrs = numeric(0),
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA),
    data = data.frame(),
    profiles_id = list(),
    depths = list(matrix(nrow=0, ncol=2, dimnames = list(NULL, c('top', 'bottom'))))
  )
)

setClass(
  Class = 'SpatialSoilSpectraDataFrame',
  representation = representation(
    'SoilSpectraDataFrame',
    'SpatialPoints'
  ),
  prototype = prototype(
    bbox = matrix(NA),
    proj4string = CRS(),
    coords = matrix(NA),
    coords.nrs = numeric(0),
    wl = numeric(),
    nir = matrix(),
    id = data.frame(NA),
    units = as.character(NA),
    data = data.frame(),
    profiles_id = list(),
    depths = list(matrix(nrow=0, ncol=2, dimnames = list(NULL, c('top', 'bottom'))))
  )
)

## Just for fun: aqp hacking

# setClass(
#   Class = ".SpatialSoilProfilesDataFrame",
#   representation = representation(
#     ".SoilProfiles",
#     "SpatialPoints",
#     id = "character",
#     horizons = 'list'
#   ),
#   prototype = prototype(
# 
#   )
# )
