\name{plot.Spectra}

\alias{plot.Spectra}
\alias{plot_summary.Spectra}

\title{Plots an object inheriting  from the Spectra class}

\usage{\method{plot}{Spectra} (x, gg=FALSE, gaps=TRUE, attr=NULL, ...)}

\description{Plots an object inheriting  from the Spectra class}

\details{
The philosophy of this plotting routine is to provide a "quick'n'dirty" way to plot your spectra collection. For advanced visualisations, the use of \code{\link{melt_spectra}} alongside with ggplot2 or lattice is encouraged.
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\arguments{
  \item{x}{an object of class \code{"Spectra"} or inheriting from this class}
  \item{gg}{if TRUE, uses the \code{ggplot2} package to plot the data, if FALSE uses \code{lattice} (experimental)}
  \item{gaps}{if TRUE, gaps in the spectra are not plotted}
  \item{attr}{attribute against which lines are coloured}
  \item{...}{options to be passed to xyplot}
}


\examples{
# Default plotting method
#plot(foo)

# Default plot using ggplot2
#plot(foo, gg = TRUE)

# Managing gaps in the spectra
#bar <- cut(foo, c(-1*450:500, -1*1800:2050))
#plot(foo, gg = TRUE, gaps = TRUE)
#plot(foo, gg = TRUE, gaps = FALSE)

# Using an attribute to group spectra
#bar <- aggregate_spectra(foo, fun = mean, id = 'someFactor')
#plot(bar, gg = TRUE, attr = 'someFactor')

}
