\name{melt_spectra}
\alias{melt_spectra}

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
#head(res <- melt_spectra(foo))
#head(res <- melt_spectra(foo, attr = 'somefactor'))
#p <- ggplot(res) + geom_line(aes(x=wl, y=nir, group=id, colour=somefactor)) + theme_bw()
#print(p)
}