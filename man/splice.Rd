\name{"Splicing" and interpolation}
\alias{"Splicing" and interpolation}

\alias{splice,Spectra-method}

\title{
"Splicing" and interpolation of spectra
}
\description{
This method mimicks the "splicing" method available in the ViewSpec Pro software from ASD, which aims at correcting steps in the data.

It removes parts of the spectra defined by the \code{wl} vector, and interpolates these parts using a method chosen using the \code{method} option.
}
\usage{
splice(x, wl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{Spectra} object}
  \item{wl}{the wavelengths to cut out and interpolate}
  \item{method}{the interpolation method. Available options are "linear", "nearest", "pchip", "cubic", and "spline".}
}

\details{
This function is a wrapper around \code{signal::interp1}.
}

\value{
an object of same class as x
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}


\examples{
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500
oz_spliced <- splice(australia, wl = c(725:1020, 1801:1950), method = "spline")
plot(oz_spliced)
}
