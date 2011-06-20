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

setMethod("summary", "summary.Spectra", summary.Spectra)

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
      SpectralResolution <- resolution(x[["wl"]])
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

setMethod("print", "summary.Spectra", print.summary.Spectra)

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
      SpectralResolution <- resolution(object)
      if (length(SpectralResolution) > 1)
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
as.data.frame.Spectra <- function(x, ...)  {
  df <- as.data.frame(spectra(x))
  names(df) <- wl(x)
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
  function(object)
    object@nir
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
if (!isGeneric("id"))
  setGeneric("id", function(object, ...)
    standardGeneric("id"))

#' Returns the ids of each spectra in the collection
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a \code{character} object
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("id", "Spectra",
  function(object)
    object@id
)

# Getting the units
if (!isGeneric("units"))
  setGeneric("units", function(object)
    standardGeneric("units"))

#' Returns the unit in which the wavelengths values are expressed
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a \code{character}
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("units", signature = "Spectra",
  function(object)
    object@units
)

if (!isGeneric('units<-'))
  setGeneric('units<-', function(object, value)
    standardGeneric('units<-'))

setReplaceMethod("units", "Spectra",
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
setMethod(f='length', signature='Spectra',
  definition=function(x)
    ncol(x@nir)
)

#' Returns the number of samples in the object
#'
#' @param object an object inheriting from \code{Spectra}
#' @return a vector
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod(f='nrow', signature='Spectra',
definition=function(x)
    nrow(id(x))
)

## Returns spectral resolution of the wavelengths

if (!isGeneric("resolution"))
  setGeneric("resolution", function(object, ...)
    standardGeneric("resolution"))

#' Returns the spectral resolution of an object
#'
#' @param object a vector
#' @param digits the number of significant digits
#' @return a vector
#'
#' @method resolution numeric
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
resolution.numeric <- function(object, digits = 10, ...){
  unique(round(diff(object), digits = digits)) # round - otherwise diff() picks some unsignificant values
}

#' Returns the spectral resolution of an object
#'
#' @param object an object inheriting from \code{Spectra}
#' @param digits the number of significant digits
#' @return a vector
#'
#' @method resolution Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
resolution.Spectra <- function(object, digits=10, ...){
  x <- wl(object)
  unique( round( diff(x), digits=digits) )
}

setMethod("resolution", "numeric", resolution.numeric)
setMethod("resolution", "integer", resolution.numeric)
setMethod("resolution", "Spectra", resolution.Spectra)

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
  function(x, i, j, ...) {

    missing.i <- missing(i)
    missing.j <- missing(j)
    nargs <- nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.

    if (missing.i)
      i <- TRUE
    else {
      # throws an error if trying to index rows using NAs
      if (any(is.na(i)))
	stop("NAs not permitted in row index")
      # in the case indexing rows by ids
      if (is.character(i))
	i <- which(x@id %in% i)
    }

    if (missing.j)
      j <- TRUE
    else
      j <- which(as.numeric(wl(x)) %in% j)

    # If there is a data slot
    if ("data" %in% slotNames(x)) {
      df <- x@data[i, , drop = FALSE]
      res <- SpectraDataFrame(wl=x@wl[j], nir=x@nir[i, j, drop = FALSE], id=x@id[i, , drop = FALSE], data=df)
    }
    # if this is a Spectra obecjt
    else
      res <- Spectra(wl=x@wl[j], nir=x@nir[i, j, drop = FALSE], id=x@id[i, , drop = FALSE])

    res
  }
)

## Upgrade a Spectra object to a SpectraDataFrame

if (!isGeneric('data<-'))
  setGeneric('data<-', function(object, value)
    standardGeneric('data<-'))

#'
setReplaceMethod("data", "Spectra",
  function(object, value) {
    if (!inherits(value, "data.frame"))
      stop('invalid initialization for SpectraDataFrame object')
    SpectraDataFrame(object, data=value)
  }
)

## Adding objects together
# Maybe to be moved into the Spectra() and SpectraDataFrame() method.

if (!isGeneric("add"))
  setGeneric("add", function(x, y, ...)
    standardGeneric("add"))

#' Adds two Spectra objects together
#'
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
.add.Spectra <- function(x, y){
  tmp <- list()

  if (identical(x@wl, y@wl))
    tmp$wl <- x@wl
  else
    stop('You can not add objects with different wavelength ranges')

  if (identical(ncol(x@wl), ncol(y@wl)))
    tmp$nir <- rbind(x@nir, y@nir)
  else
    stop('You can not add objects with different wavelength ranges')

  if (!any(x@id %in% y@id))
    tmp$id <- rbind(x@id, y@id)
  else
    stop('You can not add objects with overlapping IDs')

  if (x@units %in% y@units)
    tmp$units <- x@units
  else
    stop('You can not add objects with different wavelength units')

  if (("data" %in% slotNames(x)) & ("data" %in% slotNames(y))) {
    tmp$data <- join(x@data, y@data, type="full")
    res <- SpectraDataFrame(wl=tmp$wl, nir=tmp$nir, id=tmp$id, units=tmp$units, data=tmp$data)
  }
  else
    res <- Spectra(wl=tmp$wl, nir=tmp$nir, id=tmp$id, units=tmp$units)

  res
}

add.Spectra <- function(...){
  dotargs <- list(...)
  if ( !all(sapply(dotargs, function(x) is(x,"Spectra") )) )
    stop('the arguments must be Spectra objects')

  res <- dotargs[[1]]
  if (nargs() >= 2) {
    for (i in 2:length(dotargs))
      res <- .add.Spectra(res, dotargs[[i]])
  }
  res
}

setMethod("add", signature=c("Spectra", "Spectra"),
  function(x,y,...) add.Spectra(x, y, ...))

setMethod("add", signature=c("SpectraDataFrame", "SpectraDataFrame"),
  function(x,y,...) add.Spectra(x, y, ...))

## Split

split.Spectra <- function(x, f, drop = FALSE, ...){
  lapply(split(seq_len(nrow(x)), f, drop = drop, ...), function(ind) x[ind, , drop = FALSE])
}

setMethod("split", "Spectra", split.Spectra)

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
mutate.Spectra <- function (.data, ...){

  wls <- wl(.data)
  uns <- units(.data)
  ids <- id(.data)

  cols <- as.list(substitute(list(...))[-1])
  cols <- cols[names(cols) != ""]

  # transformations on the spectra
  if (any(names(cols) == 'nir')) {
    nir <- reshape2::melt(spectra(.data), varnames=c('id', 'wl'), value.name = "nir")
    nir[["nir"]] <- eval(cols[["nir"]], nir, parent.frame())
    nir <- acast(nir, id ~ wl)
  }
  else
    nir <- spectra(.data)

  res <- Spectra(wl = wls, nir = nir, id = ids, units = uns)

  # transformations on the data - only for classes inheriting from SpectraDataFrame
  if ("data" %in% slotNames(.data)) {

    d <- data(.data)

    if (any(names(cols) %in% names(.data))) {
      cols_data <- names(cols)[which(names(cols) %in% names(.data))]
      for (col in cols_data){
	d[[col]] <- eval(cols[[col]], d, parent.frame())
      }
    }

    res <- SpectraDataFrame(res, data = d)
  }

  res
}

setMethod("mutate", "Spectra", mutate.Spectra)

#` Melting the spectra matrix
#'
#' @param obj an object inheriting from the \code{Spectra} class
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#` @import reshape2
melt_spectra <- function(obj, ...){

  # if obj is Spectra* class
  if (inherits(obj, 'Spectra')){
    x <- spectra(obj)
  }
  # if obj is a data.frame or a matrix (ass returned by spectra)
  else {
    if ((inherits(obj, 'data.frame')) | (inherits(obj, 'matrix'))){
      x <- obj
    }
    else
      stop('The object you try to melt either be a matrix or data.frame, or a Spectra* object')
  }
  res <- reshape2:::melt.array(x, varnames=c('id', 'wl'), value.name="nir")
  names(res)[3] <- "nir" # tmp fix - waiting for fix upstream in reshape2
  res
}