# Default colour scheme for plot
#
# It's from colorbrewer. I use this fixed thing so I
# do not need to add a dep on RColorBrewer, but users are
# encouraged to make use of it in the doc.
#
# Currently teh default scheme is defined by:
# library(RcolorBrewer)
# brewer.pal(n=5, name ="Set1")
#
.defaultSpectraColours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")

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
    x <- fill_spectra(x, fill = NA, ...)
  }

  if (gg) {
    .try_require("ggplot2")

    if (is.null(attr)) s.melt <- melt_spectra(x)
    else s.melt <- melt_spectra(x, attr = attr)

    # force id colname
    names(s.melt)[1] <- 'id'

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
    return(p)
  }
  else {
    # Fast implenmentation using matplot

    # inspect dots to check matplot args: type, lty, ylab, xlab, ylim
    dots <- list(...)
    nm_dts <- names(dots)

    # insert default values if no matplot args given by user
    if (!("type" %in% nm_dts)) dots$type <- 'l'
    if (!("lty" %in% nm_dts)) dots$lty <- 1
    if (!("ylab" %in% nm_dts)) dots$ylab <- "Reflectance"
    if (!("xlab" %in% nm_dts)) dots$xlab <- paste("Wavelength (", wl_units(x), ")", sep = "")
    if (!("xlim" %in% nm_dts)) dots$xlim <- range(wl(x))
    if (!("ylim" %in% nm_dts)) dots$ylim <- range(spectra(x), na.rm = TRUE, finite = TRUE)
    if (!("col" %in% nm_dts)) dots$col <- .defaultSpectraColours
    
    # insert x and y values 
    dots$x <- wl(x)
    dots$y <- t(spectra(x))
    
    do.call("matplot", dots)
  }
}

## TODO: plot_summary() plotting mean spectra +- sd

if (!isGeneric("plot_summary")) {
    setGeneric("plot_summary", function(x, ...)
        standardGeneric("plot_summary"))
}

setMethod("plot_summary", signature('Spectra'), 
  function(x, fun = mean, se = TRUE, ...) {

    .try_require("ggplot2")

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
)

if (!isGeneric("plot_stack")) {
    setGeneric("plot_stack", function(x, ...)
        standardGeneric("plot_stack"))
}

setMethod("plot_stack", signature('Spectra'), 
  function(x){
    .try_require("ggplot2")
    m <- melt_spectra(x)
    idnm <- names(m)[1]
    m[[idnm]] <- as.factor(m[[idnm]])
    form_grid <- as.formula(paste(idnm, '~.'))
    ggplot(m) + 
      geom_line(aes_string(x = 'wl', y = 'nir', colour = idnm)) + 
      facet_grid(form_grid) + 
      theme_bw()
  }
)

if (!isGeneric("plot_offset")) {
    setGeneric("plot_offset", function(x, offset, ...)
        standardGeneric("plot_offset"))
}

setMethod("plot_offset", signature('Spectra', 'ANY'), 
  function(x, offset = 1){
    .try_require("ggplot2")
    # offsets values
    offsets <- (seq_len(nrow(x)) - 1)*offset
    # affect spectra with offset values
    spectra(x) <- aaply(offsets, 1, function(offsets) spectra(x[offsets + 1,]) + offsets)
    m <- melt_spectra(x)
    
    idnm <- names(m)[1]
    m[[idnm]] <- as.factor(m[[idnm]])
    
    ggplot(m) + 
      geom_line(aes_string(x = 'wl', y = 'nir', colour = idnm, group = idnm)) +
      labs(y = NULL) +
      theme_bw()

  }
)

## Code for adding NAs to potentially removed WLs
##
# ref reference wavelengths
# fill value to fill missing WLs with
#
fill_spectra <- function(obj, ref = NULL, fill = NA, ...) {

  if (is.null(ref)) {
    # Trying to get the most common resolution values
    r <- as.numeric(names(which.max(table(diff(wl(obj))))))
    nb_gaps <- length(table(diff(wl(obj))))
    if (nb_gaps > 2)
      warning("Sorry, at this stage removing gaps does not work well with irreguarly spaced wavelengths. Results might be odd.")
    ref <- seq(from = min(wl(obj)), to = max(wl(obj)), by = r)
  }
  
  # Detect missing WLs
  missing_wl <- setdiff(ref, wl(obj))

  # If tehre is gaps in the data, we add these as NAs values
  if (length(missing_wl) > 0) {
    # Create matrix of NAs for the missing WLs
    new_nir <- matrix(fill, ncol = length(missing_wl), nrow = nrow(obj))
    colnames(new_nir) <- missing_wl
    
    # Collate the NA matrix with the rest of the spectra
    new_nir <- cbind(spectra(obj), new_nir)
    # Re-order the spectra matrix
    idx <- order(as.numeric(colnames(new_nir)))
    new_nir <- new_nir[, idx, drop = FALSE]
    
    spectra(obj) <- new_nir
  }

  obj
}
