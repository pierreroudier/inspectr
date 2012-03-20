#' Plots an object inheriting  from the Spectra class
#'
#' @param x an object of class Spectra or inheriting from this class
#' @param gg if TRUE, usees the \code{ggplot2} package to plot the data, if FALSE uses \code{lattice}
#' @param ... options to be passed to xyplot
#' @method plot Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @import ggplot2 lattice
plot.Spectra <- function(x, gg = FALSE, gaps = TRUE, attr = NULL, ...){

  # Show gaps in the data?
  if (gaps) {
    x <- .fillSpectra(x)
  }

  if (is.null(attr)) {
    s.melt <- melt_spectra(x)
  }
  else {
    s.melt <- melt_spectra(x, attr = attr)
  }

  # force id colname
  names(s.melt)[1] <- 'id'

  if (gg) {
    .try_require("ggplot2")
    p <- ggplot(s.melt) 

    if (is.null(attr)) {
      p <- p + geom_line(aes_string(x = 'wl', y = 'nir', group = 'id'))
    }
    else {
      p <- p + geom_line(aes_string(x = 'wl', y = 'nir', group = 'id', colour = attr))
    }
    p <- p +
      labs(x = paste("Wavelength (", wl_units(x), ")", sep = ""), y = "Reflectance") +
      theme_bw()
  }
  else {
    .try_require("lattice")
    # initiate dummy vars to pas R CMD check
    wl <- nir <- id <- NULL
    p <- xyplot(nir ~ wl, groups = id, data = s.melt, type = 'l', col.line = 'black', ...)
  }
  p
}

## TODO: plot_summary() plotting mean spectra +- sd
plot_summary.Spectra <- function(x, fun = mean, se = FALSE, ...) {

  # If sd is given as TRUE or FALSE
  if (is.logical(se)) {
    if (se) {
      plot.se <- TRUE
      fun.se <- sd
    }
    else {
      plot.se <- FALSE
    }
  }
  # If sd is being given a function
  else {
    # If the function is valid
    if (is.function(se)) {
      plot.se <- TRUE
      fun.se <- se
    }
    # Else stop
    else {
      stop('The se = ... option must evaluate to either logical or function.')
    }
  }

  s.melt <- melt_spectra(x)

#   s.summary <- ddply(s.melt, 'wl', fun, ...)
  s.summary <- ddply(s.melt, 'wl', function(x) {do.call(fun, list(x$nir))})
  names(s.summary)[2] <- 'nir'

  if (plot.se) {
    # initiate dummy vars to pas R CMD check
    wl <- nir <- nir_se <- NULL

    s.se <- ddply(s.melt, 'wl', function(x) {do.call(fun.se, list(x$nir))})
    names(s.se)[2] <- 'nir_se'
    
    s <- join(s.summary, s.se, by = 'wl')

    p <- ggplot() + 
      geom_line(data = s, aes(x = wl, y = nir - nir_se), linetype = 2) +
      geom_line(data = s, aes(x = wl, y = nir + nir_se), linetype = 2) +
      geom_line(data = s, aes(x = wl, y = nir)) +
      labs(x = paste("Wavelength (", wl_units(x), ")", sep = ""), y = "Reflectance") +
      theme_bw()
  } 
  else {
    p <- ggplot() + 
      geom_line(data = s.summary, aes(x = wl, y = nir)) +
      labs(x = paste("Wavelength (", wl_units(x), ")", sep = ""), y = "Reflectance") +
      theme_bw()
  }

  p
}

## Code for adding NAs to potentially removed WLs
##
.fillSpectra <- function(obj) {

  # Detect missing WLs
  missing_wl <- setdiff(seq(from = min(wl(obj)), to = max(wl(obj)), by = res(obj)), wl(obj))
  
  # Create matrix of NAs for the missing WLs
  new_nir <- matrix(NA, ncol = length(missing_wl), nrow = nrow(obj))
  colnames(new_nir) <- missing_wl

  # Collate the NA matrix with the rest of the spectra
  new_nir <- cbind(spectra(obj), new_nir)
  # Re-order the spectra matrix
  idx <- order(as.numeric(colnames(new_nir)))
  new_nir <- new_nir[, idx, drop = FALSE]

  spectra(obj) <- new_nir

  obj
}
