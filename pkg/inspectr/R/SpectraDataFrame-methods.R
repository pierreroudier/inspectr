"SpectraDataFrame" <- function(..., wl=numeric(), nir=matrix(), id=as.character(NA), units="nm", data=data.frame()) {

  dotargs <- list(...)

  # Initialisation from Spectra object(s)
  if (any(sapply(dotargs, inherits, "Spectra"))) {
    id_spectra <- which(sapply(dotargs, inherits, "Spectra"))
    # if there's more than one Spectra object
    if (length(id_spectra) > 1) {
      ss <- dotargs[id_spectra]
      s <- ss[[1]]
      for (i in 2:length(id_spectra))
	s <- rbind(s, ss[[i]])
    }
    # if theres only one Spectra object
    else
      s <- dotargs[[1]]

    wl <- wl(s)
    nir <- spectra(s)
    id <- ids(s)
    units <- wl_units(s)
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

  rownames(data) <- id[, 1]

  new("SpectraDataFrame", wl=wl, nir=nir, id=id, units=units, data=data)
}

## coercition methods

as.data.frame.SpectraDataFrame = function(x, ..., expand = TRUE, include_id = TRUE)  {
  
  data <- features(x, include_id = include_id)

  if (expand) {
    df <- data.frame(data, spectra(x))
    names(df) <- c(names(data), wl(x))
  }
  else {
    df <- data.frame(data, NIR = I(spectra(x)))
  }

  df
}

setAs("SpectraDataFrame", "data.frame", function(from) {
	as.data.frame.SpectraDataFrame(from)
  })

## Getting the data

if (!isGeneric("features"))
  setGeneric("features", function(obj, ...)
    standardGeneric("features"))

setMethod("features", "SpectraDataFrame",
  function(obj, ..., include_id = FALSE) {
    if (include_id) {
      res <- data.frame(ids(obj), obj@data) 
    } else {
      res <- obj@data
    }
    res
  }
)

## Append or replace data

if (!isGeneric('features<-'))
  setGeneric('features<-', function(object, value, ...)
    standardGeneric('features<-')
)

setReplaceMethod("features", signature("SpectraDataFrame", "ANY"),
  # safe enables id check
  # key gives the column name of the ids in the data.frame
  function(object, value, safe = TRUE, key = NULL, remove_id = TRUE, append = TRUE) {
    
    if (!inherits(value, "data.frame"))
      stop('data must be provided as a data.frame object')

    if (safe) {
      if (is.null(key))
        stop("In the safe mode, you need to provide either the column name of the sample ids to the key option.")
      if (length(key) != 1)
        stop("Please provide only ONE id column.")

      if (is.numeric(key)) {
        key <- names(value)[key]
      }

      if (append) {
        # Actual ID sanity check
        d <- features(object, include_id = TRUE)
        # Using the "key" name for ids
        names(d[names(ids(object))]) <- key
      }
      else {
        d <- ids(object)
        # Using the "key" name for ids
        names(d) <- key
      }
      # Put data together
      data <- join(d, value,  by = key, type = "left", match = "first")
      # removing the id column      
      if (remove_id)
        data <- data[, -1*which(names(data) == key), drop = FALSE]
    }
    else {
      warning("Sample ID check has been disabled. This mode assumes you made sure the order of the rows in your data is consistent with the order in which these samples appear in the Spectra object.")
      
      if (append) data <- data.frame(features(object), value)
      else data <- value
    }
    
    SpectraDataFrame(object, data = data)
  }
)

setMethod("$", "SpectraDataFrame",
  definition=function(x, name) x@data[[name]]
)

setReplaceMethod("$", "Spectra",
  definition=function(x, name, value) {
    # For SpectraDataFrame
    if ('data' %in% slotNames(x)) {
      x@data[[name]] <- value
    }
    # Else promoting Spectra to SpectraDataFrame
    else {
      data <- data.frame(value)
      names(data) <- name
      x <- SpectraDataFrame(x, data = data)
    }
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

setReplaceMethod("[[", c("Spectra", "ANY", "missing", "ANY"),
  function(x, i, j, value) {
    # For SpectraDataFrame
    if ('data' %in% slotNames(x)) {
      x@data[[i]] <- value
    }
    # Else promoting Spectra to SpectraDataFrame
    else {
      data <- data.frame(value)
      names(data) <- i
      x <- SpectraDataFrame(x, data = data)
    }
    x
  }
)

setMethod("[", c("SpectraDataFrame", "ANY", "ANY", "missing"),
  function(x, i, j, ..., k, drop = FALSE) {

    .bracket <- function(x, i, j, k, ..., drop = FALSE) {

      missing.i <- missing(i)
      missing.j <- missing(j)
      missing.k <- missing(k)

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

      # COLS
      if (missing.j) {
        j <- TRUE
      } 
      else {
        if (is.numeric(j)) {
          # If the indices are all negative, cols are removed
          if (all(j < 0)) {
            j <- setdiff(1:ncol(x), abs(j))
          }
        }
        else {
          j <- which(names(x) %in% j)
        }
      }

      # WAVELENGTHS
      if (missing.k) {
        k <- TRUE
      } 
      else {
        # If the indices are all negative, cols are removed
        if (all(k < 0)) {
          k <- setdiff(as.numeric(wl(x)), abs(k))
        }
        k <- which(as.numeric(wl(x)) %in% k)
      }

      SpectraDataFrame(wl = x@wl[k], nir = x@nir[i, k, drop = FALSE], id = x@id[i, , drop = FALSE], data = features(x)[i, j, drop = FALSE])
    }
  
  .bracket(x, i, j, k, ..., drop = drop)
  }
)

names.SpectraDataFrame <- function(x) names(x@data)

"names<-.SpectraDataFrame" <- function(x, value) {
  names(x@data) <- value
  x
}

#` Melting the spectra matrix
#'
setMethod("melt_spectra", "SpectraDataFrame", function(obj, attr = NULL, ...){
  
  id.nm <- names(ids(obj))

  if (!is.null(attr)) {
    data <- subset(features(obj), select = attr)
    x <- data.frame(ids(obj), data, spectra(obj))
    names(x) <- c(id.nm, attr, wl(obj))
  }
  else {
    x <- data.frame(ids(obj), spectra(obj))
    names(x) <- c(id.nm, wl(obj))
  }
  
  res <- reshape2:::melt.data.frame(x, id.vars = c(id.nm, attr), variable.name = 'wl', value.name = "nir")
  res$wl <- as.numeric(as.character(res$wl))
  res
})

## Subset SDF with a subset/select query

subset.SpectraDataFrame <- function(x, subset, select, drop = FALSE, ...) {
  # adapted from subset.data.frame
  df <- features(x)
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
  SpectraDataFrame(wl = wl(x), nir = spectra(x)[id_selected, , drop = FALSE], id = ids(x)[id_selected, 1, drop = FALSE], units = wl_units(x), data = df_sub)
}

setMethod("subset", "SpectraDataFrame", subset.SpectraDataFrame)

## REMOVED - rbind is doing the same thing now
# if (!isGeneric("unseparate"))
#   setGeneric("unseparate", function(obj, ...)
#     standardGeneric("unseparate"))
# 
# unseparate.SpectraDataFrame <- function(obj){
#   #' Warning: does not recover the order of the samples
#   #'
#   add(obj$calibration, obj$validation)
# }
# 
# setMethod("unseparate", "list", unseparate.SpectraDataFrame)
