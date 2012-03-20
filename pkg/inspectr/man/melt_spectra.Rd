\name{melt_spectra}
\docType{methods}
\alias{melt_spectra}
\alias{melt_spectra-methods}
\alias{melt_spectra,Spectra-method}
\alias{melt_spectra,SpectraDataFrame-method}

\title{melt_spectra}
\description{Melts the spectra data of a Spectra object and returns it as wide format.}

\usage{
melt_spectra(obj, ...)
melt_spectra(obj, attr = NULL, ...)
}

\arguments{
  \item{obj}{an object inheriting from the \code{Spectra} class}
  \item{attr}{for the \code{SpectraDataFrame} class, the name of an attribute in the object data to split the spectra against.}
  \item{...}{any option evaluated in the context of plyr::melt}
}

\details{
This function is very useful when wanting to plot spectra using the lattice or ggplot2 packages.
}

\references{
  Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data                                                                                          
  Analysis. Journal of Statistical Software, 40(1), 1-29. URL                                                                                               
  http://www.jstatsoft.org/v40/i01/.
}

\seealso{\code{\link{melt}}}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Simple melt
r <- melt_spectra(australia)
head(r)

\dontrun{
# Melt against some factor (or continuous data), and plot
# using ggplot2

# Create some factor
australia$fact <- sample(LETTERS[1:3], size = nrow(australia), replace = TRUE) # Generate some kind of factor
r <- melt_spectra(australia, attr = 'fact')

# Create plot
library(ggplot2)
p <- ggplot(r) + geom_line(aes(x=wl, y=nir, group=id, colour=fact)) + theme_bw()
print(p)
}
}