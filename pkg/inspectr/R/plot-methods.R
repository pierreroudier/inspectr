#' Plots an object inheriting  from the Spectra class
#'
#' @param x an object of class Spectra or inheriting from this class
#' @param gg if TRUE, usees the \code{ggplot2} package to plot the data, if FALSE uses \code{lattice}
#' @param ... options to be passed to xyplot
#' @method plot Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @import ggplot2 lattice
plot.Spectra <- function(x, gg=FALSE, ...){
  s.melt <- melt_spectra(x)
  if (gg) {
    .try_require("ggplot2")
    p <- ggplot(s.melt) + geom_line(aes(x=wl, y=nir, group=id)) # + ylim(c(0,1))
  }
  else
    p <- xyplot(nir ~ wl, groups=id, data=s.melt, type='l', col.line='black', ...)
  p
}
