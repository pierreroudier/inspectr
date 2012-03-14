#' Aggregates the spectral information of a Spectra object using
#' an aggregation function chosen by the user.
#'
#' Aggregates the spectral information of a Spectra object using
#' an aggregation function chosen by the user.If some data is also
#' present, it is aggregated using the same function.
#'
#' @param obj an object inheriting from class Spectra
#' @param fun an aggregation function
#' @param id an aggregation identifier
#' @param ... expressions evaluated in the context of \code{fun}
#' @return An object of the same class as \code{obj}
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @import plyr

if (!isGeneric("aggregate_spectra"))
  setGeneric("aggregate_spectra", function(obj, fun = mean, ...)
    standardGeneric("aggregate_spectra"))

setMethod("aggregate_spectra", "Spectra",
  function(obj, fun = mean, ...){
    
    # making up an id name from the aggregation function
    id_fun <- as.character(substitute(fun, env = parent.frame()))[1]
    id_obj <- as.character(substitute(obj, env = parent.frame()))
    id <- paste(id_fun, id_obj, sep = '.')
  
    # applying the function to the spectra
    nir <- aaply(.data = spectra(obj), .margins = 2, .fun = fun, ...)

    # Create and return Spectra object
    Spectra(wl = wl(obj), nir = nir, id = id, units = wl_units(obj))
  }
)

# In the case of a SDF, an id can be given to split the SDF and apply fun
#
setMethod("aggregate_spectra", "SpectraDataFrame",
  function(obj, fun = mean, id = NULL, ...){

    # No split --> the whole data is aggregated together
    if (is.null(id)) {
      # making up an id name from the aggregation function
      id_fun <- as.character(substitute(fun, env = parent.frame()))[1]
      id_obj <- as.character(substitute(obj, env = parent.frame()))
      # Select and paste only alphanumeric chars
      id_obj <- paste(id_obj[grep(x = id_obj, pattern = '[[:alnum:]]')], collapse = '.')
      # Combine object name and function name into an id
      id <- paste(id_fun, id_obj, sep = '.')
  
      # applying the function to the spectra
      nir <- apply(spectra(obj), 2, fun, ...)
      
      res <- Spectra(wl = wl(obj), nir = nir, id = id, units = wl_units(obj))
      
      data <- sapply(features(obj), fun, ...)
            
      res <- SpectraDataFrame(res, data = data.frame(matrix(data, nrow = 1, dimnames = list(id, names(obj)))))
    }

    # There is a variable against which the data will be aggregated
    else {
      if (id %in% names(features(obj))) {

        # Col index of the splitting variable
        idx <- which(names(features(obj)) == id)

        # Creating spectra splits
        s <- data.frame(id = features(obj)[, idx, drop = FALSE], spectra(obj))
        s <- ddply(s, id, colwise(fun, ...))
        # Remove id used to split data.frame
        s <- s[, -1]
        
        # new data slot
        d <- ddply(features(obj), id, colwise(fun, ...))
        
        # recompose the object
        res <- SpectraDataFrame(wl = wl(obj), nir = s, units = wl_units(obj), data = d)
      }
      else
        stop('Bad aggregation identifier.')
    }

    res
  }
)