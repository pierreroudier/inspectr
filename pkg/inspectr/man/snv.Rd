\name{snv}
\alias{snv}
\alias{rnv}

\title{Standard and Robust Normal Variate transformations}

\usage{
snv(x)
rnv(x, r)
}

\description{Standard and Robust Normal Variate transformations are often used in chemometrics to normalise a spectra collection and remove the baseline effect.}

\details{
The Standard Normal Variate transformation (SNV, Barnes et al., 1989) is a common method to reduce within-class variance.

The Robust Normal Variate transformation (RNV, Guo et al., 1999) is a modification of the SNV to make it more robust to closure problems.

These function are to be used in conjonction with \code{\link{apply_spectra}}.
}

\value{A vector of numeric values}

\arguments{
  \item{x}{a vector of numeric values}
  \item{r}{the percentile to use in the RNV computation}
}

\references{
\itemize{
  \item Barnes, R.J., Dhanoa, M.S., Lister, S.J. 1989. Standard normal variate transformation and detrending of near-infra-red diffuse reflectance spectra. Applied Spectroscopy 43, 772--777.

  \item Guo, Q., Wu, W., Massar, D.L. 1999. The robust normal variate transform for pattern recognition with near-infrared data. Analytica Chimica Acta 382:1--2, 87--103.    
}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{apply_spectra}}, \code{\link{baseline}}}

\examples{
#bar <- apply_spectra(foo, snv)
#plot(bar)
}
