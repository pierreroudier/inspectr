"SpectraDataFrame" <- function(..., wl=numeric(), nir=matrix(), id=as.character(NA), units="nm", data=data.frame()) {

  dotargs <- list(...)

  # Initialisation from Spectra object(s)
  if (any(sapply(dotargs, class) == "Spectra")) {
    id_spectra <- which(sapply(dotargs, class) == "Spectra")
    # if there's more than one Spectra object
    if (length(id_spectra) > 1) {
      ss <- dotargs[id_spectra]
      s <- ss[[1]]
      for (i in 2:length(id_spectra))
	s <- add(s, ss[[i]])
    }
    # if theres only one Spectra object
    else
      s <- dotargs[[1]]

    wl <- wl(s)
    nir <- spectra(s)
    id <- id(s)
    units <- units(s)
  }

  else {
    # if the wl are given as an integer vector they are translated into a numeric vector
    # for clarity (only one type to manage)
    if (is(wl, "integer"))
      wl <- as.numeric(wl)
    if (is(nir, 'data.frame'))
      nir <- as.matrix(nir)
#     if (!is(id, "character"))
#       id <- as.character(id)
    if (!is(id, "data.frame"))
      id <- data.frame(id = id)

    # If no id is given
    if (all(is.na(id))) {
      # If the object is void
      if (length(nir) == 1) 
        id <- data.frame(NULL)
      # if a matrix is here
      else 
        id <- data.frame(id = as.character(seq(1, nrow(nir))))
    } 
    else {
      # Test of inconsistent ids when id is specified by the user
      if (is.null(nrow(nir))) { # if theres only one spectra
        if (nrow(id) != 1)
          stop("number of individuals and number of rows in the spectra matrix don't match")
        if ((length(wl) > 1) & (length(nir) != length(wl)))
          stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
        nir <- matrix(nir, nrow=1)
      } 
      else {
        if (nrow(nir) != nrow(id))
          stop("number of individuals and number of rows in the spectra matrix don't match")
        if ((length(wl) > 1) & (ncol(nir) != length(wl)))
          stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
        colnames(nir) <- wl
        rownames(nir) <- as.vector(do.call('rbind', id))
      }
    }
  }
  if (is(data, "numeric") | is(data, "integer"))
    data <- as.data.frame(data)
  
  rownames(data) <- as.vector(do.call('rbind', id))

  new("SpectraDataFrame", wl=wl, nir=nir, id=id, units=units, data=data)
}

## coercition methods

as.data.frame.SpectraDataFrame = function(x, ...)  {
  df <- as.data.frame(spectra(x))
  data <- data(x)
  id <- id(x)
  df <- data.frame(id, data, df)
  names(df) <- c(names(id), names(data), wl(x))
  df
}

setAs("SpectraDataFrame", "data.frame", function(from)
	as.data.frame.SpectraDataFrame(from))

## Getting the data

if (!isGeneric("data"))
  setGeneric("data", function(object, ...)
    standardGeneric("data"))

setMethod("data", "SpectraDataFrame", 
  function(object)
    object@data
)

setMethod("$", "SpectraDataFrame",
  definition=function(x, name) x@data[[name]]
)

setReplaceMethod("$", "SpectraDataFrame",
  definition=function(x, name, value) {
    x@data[[name]] <- value
    x
  }
)

setMethod("[[", c("SpectraDataFrame", "ANY", "missing"), 
  function(x, i, j, ...) {
    if (!("data" %in% slotNames(x)))
      stop("no [[ method for object without attributes")
    x@data[[i]]
  }
)

setReplaceMethod("[[", c("SpectraDataFrame", "ANY", "missing", "ANY"), 
  function(x, i, j, value) {
    if (!("data" %in% slotNames(x)))
      stop("no [[ method for object without attributes")
    x@data[[i]] <- value
    x
  }
)

names.SpectraDataFrame <- function(x) names(x@data)

"names<-.SpectraDataFrame" <- function(x, value) { 
  names(x@data) <- value
  x 
}

## Subset SDF with a subset/select query

subset.SpectraDataFrame <- function(x, subset, select, drop = FALSE, ...) {
  # adapted from subset.data.frame
  df <- data(x)
  if (missing(subset)) 
        r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, df, parent.frame())
    if (!is.logical(r)) 
	stop("'subset' must evaluate to logical")
    r <- r & !is.na(r)
  }
  if (missing(select)) 
    vars <- TRUE
  else {
    nl <- as.list(seq_along(df))
    names(nl) <- names(df)
    vars <- eval(substitute(select), nl, parent.frame())
  }  
  df_sub <- df[r, vars, drop = drop]
  # remove unused factors
  df_sub <- droplevels(df_sub)
  id_selected <- which(rownames(df) %in% rownames(df_sub))
  x <- SpectraDataFrame(wl=wl(x), nir=spectra(x)[id_selected, , drop = FALSE], id=id(x)[id_selected, 1, drop = FALSE], units=units(x), data=df_sub)
  x
}

setMethod("subset", "SpectraDataFrame", subset.SpectraDataFrame)

## Separate calibration set vs validation set

if (!isGeneric("separate"))
  setGeneric("separate", function(obj, calibration, ...)
    standardGeneric("separate"))

separate.SpectraDataFrame <- function(obj, calibration){
  if (calibration < 1)
    calibration <- floor(calibration*length(obj))
  calib <- sample(x=seq_len(length(obj)), size=calibration, replace = FALSE)
  valid <- setdiff(seq_len(length(obj)), calib)
  list(calibration=obj[calib, , drop = FALSE], validation=obj[valid, , drop = FALSE])
}

setMethod("separate", "SpectraDataFrame", separate.SpectraDataFrame)

if (!isGeneric("unseparate"))
  setGeneric("unseparate", function(obj, ...)
    standardGeneric("unseparate"))

unseparate.SpectraDataFrame <- function(obj){
  #' Warning: does not recover the order of the samples
  #'
  add(obj$calibration, obj$validation)
}

setMethod("unseparate", "list", unseparate.SpectraDataFrame)