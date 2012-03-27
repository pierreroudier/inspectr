#' Constructor for the Spectra class
#'
#' @param wl a numeric vector giving the wavelengths at with the spectra have been measured
#' @param nir a \code{matrix} or a \code{data.frame} object giving the spectra values for each sample
#' @param id a vector giving the unique id of each sample in the collection
#' @param units a character giving the unit in which the wavelengths values are expressed
#' @return a new Spectra object
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
"Spectra" <- function(wl=numeric(), nir=matrix(), id=as.character(NA), units="nm") {
  # if the wl are given as an integer vector they are translated into a numeric vector
  # for clarity (only one type to manage)
  if (is(wl, "integer"))
    wl <- as.numeric(wl)

  if (is(nir, 'data.frame'))
    nir <- as.matrix(nir)

  if (!is(id, "data.frame"))
    id <- data.frame(id = id)

  # If no id is given
  if (all(is.na(id))) {
    # If the object is void
    if (length(nir) == 1)
      id <- as.character(NULL)
    # if a matrix is here
    else
      id <- data.frame(id = as.character(seq(1, nrow(nir))))
  }
  # if ids are actually given by the user
  else {
    # Test of inconsistent ids when id is specified by the user

    # if theres only one spectra
    if (is.null(nrow(nir))) {
      if (nrow(id) != 1)
	stop("number of individuals and number of rows in the spectra matrix don't match")
      if ((length(wl) > 1) & (length(nir) != length(wl)))
        stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
      nir <- matrix(nir, nrow=1)
    }

    # if theres more than one specta
    else {
      if (nrow(nir) != nrow(id))
        stop("number of individuals and number of rows in the spectra matrix don't match")
      if ((length(wl) > 1) & (ncol(nir) != length(wl)))
        stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
      colnames(nir) <- wl
      rownames(nir) <- as.vector(do.call('rbind', id))
    }
  }

  # consistency nimber of wl/number of cols in the NIR matrix
  if ((length(wl) > 1) & (ncol(nir) != length(wl)))
    stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
  rownames(nir) <- as.vector(do.call('rbind', id))
  colnames(nir) <- wl
  new("Spectra", wl = wl, nir = nir, id = id, units = units)
}

## SUMMARY

if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...)
    standardGeneric("summary"))

#' @param object an object inheriting from \code{Spectra}
#' @param ... Ignored
#' @method summary Spectra
#' @rdname Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
summary.Spectra <- function (object, ...){
    obj = list()
    obj[["class"]] = class(object)
    obj[["wl"]] = object@wl
    obj[["id"]] = object@id
    obj[["nir"]] = object@nir
    obj[["units"]] = object@units
    if ("data" %in% slotNames(object)) {
        if (ncol(object@data) > 1)
            obj[["data"]] = summary(object@data)
        else obj[["data"]] = summary(object@data[[1]])
    }
    else obj[["data"]] = NULL
    class(obj) = "summary.Spectra"
    obj
}

# setMethod("summary", "summary.Spectra", summary.Spectra)

#' @param object a summary for an object inheriting from \code{Spectra}
#' @param ... Ignored
#' @method print summary.Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
print.summary.Spectra = function(x, ...) {
    cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
    cat("Set of ", nrow(x[['id']])," spectra\n", sep = "")
    if (nrow(x[['id']]) > 0){
      cat("Wavelength range: ")
      cat(min(x[["wl"]], na.rm=TRUE), " to ", max(x[["wl"]], na.rm=TRUE)," ", x[["units"]], "\n", sep="")
      SpectralResolution <- res(x[["wl"]])
      if (length(SpectralResolution) > 1)
	cat("Spectral resolution: irregular wavelength spacing\n")
      else {
	if (length(SpectralResolution) == 0)
	  cat("Spectral resolution: NA\n")
	else
	  cat("Spectral resolution: ", SpectralResolution , " ",  x[["units"]], "\n", sep="")
      }
      if (!is.null(x$data)) {
	cat("Data attributes:\n")
	print(x$data)
      }
    }
    invisible(x)
}

# setMethod("print", "summary.Spectra", print.summary.Spectra)

## PRINT

#' @param object an object inheriting from \code{Spectra}
#' @method show Spectra
#' @rdname Spectra-methods
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod(
  f='show',
  signature='Spectra',
  definition=function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Set of ", nrow(object@id)," spectra\n", sep='')
    if (nrow(object@id) > 0){
      cat("Wavelength range: ", min(object@wl, na.rm=TRUE),"-",max(object@wl, na.rm=TRUE)," ", object@units, "\n", sep="")
      SpectralResolution <- res(object)
      if (is.null(SpectralResolution) > 1)
        cat("Spectral resolution: irregular wavelength spacing\n")
      else {
        if (length(SpectralResolution) == 0)
          cat("Spectral resolution: NA\n")
        else
          cat("Spectral resolution: ", SpectralResolution , " ", object@units, "\n", sep="")
      }
    }
    if ("data" %in% slotNames(object)) {
      cat("Data attributes:\n")
      print((object@data))
    }
  }
)

## coercition methods

#' @param x an object inheriting from \code{Spectra}
#' @param ... Ignored
#' @return a \code{data.frame} object
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
as.data.frame.Spectra <- function(x, ..., include_id = TRUE)  {
  df <- as.data.frame(spectra(x))
  names(df) <- wl(x)
  if (include_id) {
    df <- data.frame(ids(x), df)
  }
  df
}

setAs("Spectra", "data.frame", function(from)
	as.data.frame.Spectra(from))

## Accessing data

# Getting the spectra matrix
if (!isGeneric("spectra"))
  setGeneric("spectra", function(object, ...)
    standardGeneric("spectra"))

#' Returns the matrix of the spectra in the collection
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a \code{matrix} object
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("spectra", "Spectra",
  function(object) {
    res <- object@nir
    colnames(res) <- object@wl
    res
  }
)

# Getting the wavelengths
if (!isGeneric("wl"))
  setGeneric("wl", function(object, ...)
    standardGeneric("wl"))

#' Returns the wavelengths at which the spectra have been recorded
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a \code{numeric} object
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("wl", "Spectra",
  function(object)
    object@wl
)

# Getting the ids
if (!isGeneric("ids"))
  setGeneric("ids", function(object, ...)
    standardGeneric("ids"))

#' Returns the ids of each spectra in the collection
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a \code{character} object
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("ids", "Spectra",
  function(object, ..., as.vector = FALSE) {
    if (as.vector) {
      res <- object@id[[1]]
    } else {
      res <- object@id
    }
    res
  }
)

# Getting the units
if (!isGeneric("wl_units"))
  setGeneric("wl_units", function(object)
    standardGeneric("wl_units"))

#' Returns the unit in which the wavelengths values are expressed
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a \code{character}
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("wl_units", signature = "Spectra",
  function(object)
    object@units
)

if (!isGeneric('wl_units<-'))
  setGeneric('wl_units<-', function(object, value)
    standardGeneric('wl_units<-'))

setReplaceMethod("wl_units", "Spectra",
  function(object, value) {
    if (!is.character(value) | length(value) != 1)
      stop("Units have to be passed as a single character string.")
    object@units <- value
    object
  }
)

#' Returns the number of wavelengths in the object
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a vector
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
# setMethod(f='length', signature='Spectra',
#   definition=
length.Spectra <- function(x)
    ncol(x@nir)
# )

#' Returns the number of samples in the object
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a vector
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod(f='nrow', signature='Spectra',
definition = function(x)
  nrow(ids(x))
)

' Returns the number of data cols in the object
'
setMethod(f='ncol', signature='Spectra',
definition = function(x) {
  if ("data" %in% slotNames(x)) {
    n <- ncol(x@data)
  } else {
    n <- NULL
  }
  n
}
)

# setMethod(f='dim', signature='Spectra',
# definition=
dim.Spectra <- function(x) {
  r <- c(nrow(x), length(x))
  if ('data' %in% slotNames(x)) {
    r <- c(r, ncol(features(x)))
  }
  r
}
# )


## Returns spectral resolution of the wavelengths

if (!isGeneric("res"))
  setGeneric("res", function(x)
    standardGeneric("res"))

#' Returns the spectral resolution of an object
#'
#' @param object a vector
#' @param digits the number of significant digits
#' @return a vector
#'
#' @method resolution numeric
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
res.numeric <- function(x){
  unique(round(diff(x), digits = 10)) # round - otherwise diff() picks some unsignificant values
}

#' Returns the spectral resolution of an object
#'
#' @param object an object inheriting from \code{Spectra}
#' @param digits the number of significant digits
#' @return a vector
#'
#' @method resolution Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
res.Spectra <- function(x){
  r <- unique( round( diff(wl(x)), digits = 10) )
  if (length(r) > 1) r <- NULL # if resolution is not regular
  r
}

setMethod("res", "numeric", res.numeric)
setMethod("res", "integer", res.numeric)
setMethod("res", "Spectra", res.Spectra)

## overloads

#' extract parts of Spectra objects
#'
#' @name [
#' @aliases [, Spectra-method
#' @docType methods
#' @rdname extract-methods
#'
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("[", c("Spectra", "ANY", "ANY", "missing"),
  function(x, i, j, ..., drop = FALSE) {

    missing.i <- missing(i)
    missing.j <- missing(j)

    # ROWS
    if (missing.i) {
      i <- TRUE
    } 
    else {
      # throws an error if trying to index rows using NAs
      if (any(is.na(i))) {
        stop("NAs not permitted in row index")
      }
      # in the case indexing rows by ids
      if (is.character(i)) {
        i <- which(x@id %in% i)
      }
    }

    # WAVELENGTHS
    if (missing.j) {
      j <- TRUE
    } else {
      # If the indices are all negative, cols are removed
      if (all(j < 0)) {
        j <- setdiff(as.numeric(wl(x)), abs(j))
      }
      j <- which(as.numeric(wl(x)) %in% j)
    }

    Spectra(wl = x@wl[j], nir=x@nir[i, j, drop = FALSE], id = x@id[i, , drop = FALSE]) 
  }
)


## Promote a Spectra object to a SpectraDataFrame

if (!isGeneric('features<-'))
  setGeneric('features<-', function(object, ..., value)
    standardGeneric('features<-')
)

#'
setReplaceMethod("features", signature("Spectra", "ANY"),
  # safe enables id check
  # key gives the column name of the ids in the data.frame
  function(object, ..., value) {
     
#     safe = TRUE, key = NULL, remove_id = TRUE

    # hack to avoid the 'value' must be on the right side' thing at R CMD check
    dots <- list(...)
    ifelse('safe' %in% names(dots), safe <- dots$safe, safe <- TRUE)
    ifelse('key' %in% names(dots), key <- dots$key, key <- NULL)
    ifelse('remove_id' %in% names(dots), remove_id <- dots$remove_id, remove_id <- TRUE)

    if (!inherits(value, "data.frame"))
      stop('invalid initialization for SpectraDataFrame object')

    if (safe) {
      if (is.null(key))
        stop("In the safe mode, you need to provide either the column name of the sample ids to the key option.")
      if (length(key) != 1)
        stop("Please provide only ONE id column.")

      # Actual ID sanity check
      spectra_ids <- ids(object)
      if (is.numeric(key)) {
        key <- names(value)[key]
      }

      # Using the "key" name for ids
      names(spectra_ids) <- key
      
      # Put data together
      data <- join(spectra_ids, value,  by = key, type = "left", match = "first")
      
      # removing the id column      
      if (remove_id)
        data <- data[, -1*which(names(data) == key)]
    }
    else {
      warning("Sample ID check has been disabled. This mode assumes you made sure the order of the rows in your data is consistent with the order in which these samples appear in the Spectra object.")
      data <- value
    }
    SpectraDataFrame(object, data = data)
  }
)

## Adding objects together
# Maybe to be moved into the Spectra() and SpectraDataFrame() method.

# if (!isGeneric("add"))
#   setGeneric("add", function(x, y, ...)
#     standardGeneric("add"))
# 
# #' Adds two Spectra objects together
# #'
# #' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
# .add.Spectra <- function(x, y){
#   tmp <- list()
# 
#   if (identical(x@wl, y@wl))
#     tmp$wl <- x@wl
#   else
#     stop('You can not add objects with different wavelength ranges')
# 
#   if (identical(ncol(x@wl), ncol(y@wl)))
#     tmp$nir <- rbind(x@nir, y@nir)
#   else
#     stop('You can not add objects with different wavelength ranges')
# 
#   if (!any(x@id %in% y@id))
#     tmp$id <- rbind(x@id, y@id)
#   else
#     stop('You can not add objects with overlapping IDs')
# 
#   if (x@units %in% y@units)
#     tmp$units <- x@units
#   else
#     stop('You can not add objects with different wavelength units')
# 
#   if (("data" %in% slotNames(x)) & ("data" %in% slotNames(y))) {
#     tmp$data <- join(x@data, y@data, type="full")
#     res <- SpectraDataFrame(wl=tmp$wl, nir=tmp$nir, id=tmp$id, units=tmp$units, data=tmp$data)
#   }
#   else
#     res <- Spectra(wl=tmp$wl, nir=tmp$nir, id=tmp$id, units=tmp$units)
# 
#   res
# }
# 
# add.Spectra <- function(...){
#   dotargs <- list(...)
#   if ( !all(sapply(dotargs, function(x) is(x,"Spectra") )) )
#     stop('the arguments must be Spectra objects')
# 
#   res <- dotargs[[1]]
#   if (nargs() >= 2) {
#     for (i in 2:length(dotargs))
#       res <- .add.Spectra(res, dotargs[[i]])
#   }
#   res
# }
# 
# setMethod("add", signature=c("Spectra", "Spectra"),
#   function(x,y,...) add.Spectra(x, y, ...))
# 
# setMethod("add", signature=c("SpectraDataFrame", "SpectraDataFrame"),
#   function(x,y,...) add.Spectra(x, y, ...))

## rbind overload to put together Spectra* objects

rbind.Spectra <- function(..., create_new_ids = FALSE, new_ids = NULL) {
  dots <- list(...)
  names(dots) <- NULL

  # wl
  wls <- lapply(dots, wl)
  wl_test <-all(laply(wls, function(x) identical(wls[[1]], x)))
  if (!wl_test) {
    stop("To be added together, all Spectra objects must share the same wavelengths.")
  } else {
    wls <- apply(do.call('rbind', wls), 2, unique)
  }

  # units
  wl_uts <- unique(laply(dots, wl_units))
  if (length(wl_uts) > 1)
    stop("To be added together, all Spectra objects must share the same wavelength units.")

  # nir
  nir <- do.call("rbind", lapply(dots, spectra))
  
  # id
  if (is.null(new_ids)) { # We try to keep the old IDs
    ids <- do.call('c', lapply(dots, ids, as.vector = TRUE))
  } else {
    # New IDs are provided
    if (length(new_ids) == length(ids))
      ids <- new_ids
    else
      stop("new_ids must have the same length as the total number of spectra you are trying to collate together.")
  }
    
  # If ids are not unique
  if (length(unique(ids)) != length(ids)) {
    if (create_new_ids) {
      warning("Redundant IDs found. Creating new set of unique IDs.")
      ids <- 1:length(ids)
    }
    else {
      stop("Redundant IDs found. Either allow the creation of new ids using the create_new_ids option, or provide the function with a set of unique ids using the new_ids option.")
    }
  }
  
  res <- Spectra(wl = wls, nir = nir, id = ids, units = wl_uts)

  # data
  test_data <- laply(dots, function(x) "data" %in% slotNames(x))
  if (all(test_data)) {
    # Unify id colname for join
    data <- llply(dots, features, include_id = TRUE)
    data <- llply(data, function(x) {names(x)[1] <- 'id'; x})
    data <- do.call("rbind", data)
    res <- SpectraDataFrame(res, data = data)
  }
  
  res
}

rbind.SpectraDataFrame <- rbind.Spectra

## Split

setMethod("split", "Spectra", function(x, f, drop = FALSE, ...){
  
  # If length(f) <= 1, we consider f is giving the colname or index
  if (length(f) <= 1) {
    f <- features(x)[, f]
  }

  lapply(split(seq_len(nrow(x)), f, ...), function(ind) x[ind, ])
})

#`  Mutate a Spectra object by adding new or replacing existing columns.
#`
#` This function is a simple port of the \code{\link{mutate}} function in the
#' plyr package to the Spectra objects, which it wraps.
#'
#' This function is very similar to \code{\link{transform}} but it executes
#' the transformations iteratively so that later transformations can use the
#' columns created by earlier transformations. Like transform, unnamed
#' components are silently dropped.
#'
#' Mutate seems to be considerably faster than transform for large data
#' frames.
#'
#' @param obj an object inheriting from the \code{Spectra} class
#' @param ... named parameters giving definitions of new columns
#' @seealso \code{\link{mutate}}
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @export
setMethod("mutate", "Spectra", function (.data, ...){

  wls <- wl(.data)
  uns <- wl_units(.data)
  ids <- ids(.data)

  cols <- as.list(substitute(list(...))[-1])
  cols <- cols[names(cols) != ""]

  # transformations on the spectra
  if (any(names(cols) == 'nir')) {
    nir <- reshape2::melt(spectra(.data), varnames=c('id', 'wl'), value.name = "nir")
    nir[["nir"]] <- eval(cols[["nir"]], nir, parent.frame())
    nir <- reshape2::acast(nir, id ~ wl)
    # remove it from the cols list
    cols[['nir']] <- NULL
  }
  # no transformations on the spectra
  else
    nir <- spectra(.data)

  r <- Spectra(wl = wls, nir = nir, id = ids, units = uns)

  # transformations on the data - only for classes inheriting from SpectraDataFrame
  if (("data" %in% slotNames(.data)) & (length(cols) > 0)) { # testing if theres transformations left
    d <- sapply(cols, function(x) eval(x, features(.data), parent.frame()))
    r <- SpectraDataFrame(r, data = data.frame(features(.data), d))
  }

  r
})

## Separate calibration set vs validation set

if (!isGeneric("separate"))
  setGeneric("separate", function(obj, calibration, ...)
    standardGeneric("separate"))

setMethod("separate", "Spectra", function(obj, calibration){
  if (calibration < 1)
    calibration <- floor(calibration*nrow(obj))
  calib <- sample(x=seq_len(nrow(obj)), size=calibration, replace = FALSE)
  valid <- setdiff(seq_len(nrow(obj)), calib)
  list(calibration=obj[calib, ], validation=obj[valid, ])
})

#` Melting the spectra matrix
#'
#' @param obj an object inheriting from the \code{Spectra} class
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#` @import reshape2
if (!isGeneric('melt_spectra'))
  setGeneric('melt_spectra', function(obj, ...)
    standardGeneric('melt_spectra')
)

setMethod("melt_spectra", "Spectra", function(obj, ...){

  id.nm <- names(ids(obj))
  x <- data.frame(ids(obj), spectra(obj))
  names(x) <- c(id.nm, wl(obj))
  res <- reshape2:::melt.data.frame(x, id.vars = id.nm, variable.name = 'wl', value.name="nir")
  res$wl <- as.numeric(as.character(res$wl))
  res
})

## Selecting/cutting wavelengths

# Negative values will be to remove, positive to select
if (!isGeneric("cut")) {
    setGeneric("cut", function(x, ...)
        standardGeneric("cut"))
}   

setMethod("cut", "Spectra", function(x, ..., wl) {
  
  # If wl is negative, we REMOVE these
  if (any(wl < 0) & any(wl > 0) | any(wl == 0))
    stop("You can't mix positive and negative wavelengths, or use zero.")

  if (all(wl < 0)) {
    wl <- abs(wl)
    wl <- setdiff(wl(x), wl)
  } 

  # Checking that wl in available wavelengths
  if (!all(wl %in% wl(x))) {
    stop("Selected wavelengths not present in the object")
  }

  # Getting indices of the wavelengths to select
  idx <- laply(wl, function(w) which(wl(x) == w))
  # Subsetting spectra matrix
  nir <- spectra(x)[, idx]
  
  res <- Spectra(wl = wl, nir = nir, id = ids(x), units = wl_units(x))

  if ("data" %in% slotNames(x)) {
    res <- SpectraDataFrame(res, data = features(x))
  }
  
  res
})
