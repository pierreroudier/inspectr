\name{continuum_removal}
\alias{continuum_removal}

\title{Continuum removal}

\usage{continuum_removal(x)}

\description{Operates a continuum removal on a vector.}

\details{
This operation is commonly done  to normalize reflectance spectra and allow comparison of individual absorption features from a common baseline. The removal is based on the upper convex hull of the spectra. 

This function is working on vectors. It may applied on matrix or data.frames using the \code{apply} function, or on \code{Spectra*} objects using the \code{apply_spectra} function.
}

\value{A numeric vector with its continuum removed.}

\arguments{
  \item{x}{a numeric vector}
}

\references{
Clark, R.N., and Roush,  T.L. 1984. Reflectance spectroscopy: Quantitative analysis techniques for remote sensing applications. Journal of Geophysical Research 89, 6329--6340.
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}, based on code from Raphael Viscarra-Rossel.}

\seealso{\code{\link{baseline}}, \code{\link{snv}}, \code{\link{rnv}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

s <- apply_spectra(australia, continuum_removal)
plot(s)
}