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
  function(obj, fun = mean, id = NULL, ...){
    # making up an id name from the aggregation function
    new_id <- as.character(substitute(fun))[1]

    # applying the function to the spectra
    nir <- aaply(.data = spectra(obj), .margins = 2, .fun = fun, ...)

    res <- Spectra(wl = wl(obj), nir = nir, id = new_id, units = units(obj))

    res
  }
)

setMethod("aggregate_spectra", "SpectraDataFrame",
  function(obj, fun = mean, id = NULL, ...){

    if (is.null(id)) {
      # making up an id name from the aggregation function
      new_id <- as.character(substitute(fun))[1]
      # applying the function to the spectra
      nir <- aaply(.data = spectra(obj), .margins = 2, .fun = fun, ...)
      res <- Spectra(wl = wl(obj), nir = nir, id = new_id, units = units(obj))
      data <- aaply(.data = features(obj), .margins = 2, .fun = fun, ...)
      res <- SpectraDataFrame(res, data = data.frame(matrix(data, nrow = 1, dimnames = list(id, names(data)))))
    }

    else {
      if (id %in% names(features(obj))) {

        idx <- which(names(features(obj)) == id)
        s <- data.frame(id = features(obj)[, idx, drop = FALSE], spectra(obj))
        s <- dlply(s, id, mean, ...)
        s <- do.call("rbind", s)
        s[, 1] <- rownames(s)

        # new data slot
        d <- data.frame(s[,1])
        names(d) <- id

        # recompose the object
        res <- SpectraDataFrame(wl = wl(obj), nir = s[, -1], units = units(obj), data = d)
      }
      else
        stop('Bad aggregation identifier.')
    }

    res
  }
)