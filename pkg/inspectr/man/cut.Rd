\name{cut}
\docType{methods}

\alias{cut}
\alias{cut,Spectra-method}

\title{Manipulating the wavelength range of a \code{Spectra} object}

\usage{
  \S4method{cut}{Spectra}(x, ..., wl)
}

\description{This methods allows to either select or remove a specific range of wavelengths from a \code{Spectra} object.}

\details{
The wavelengths are extracted if \code{wl > 0}, or removed if \code{wl < 0}. You can't mix negative and positive index in \code{wl}.
}

\value{An object of the same class as \code{x}.}

\arguments{
  \item{x}{an object inheriting from class \code{Spectra}}
  \item{wl}{a vector of the wavelengths to either select or remove from \code{x}}
  \item{...}{ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{[}}}

\examples{
# Extracting a specific wavelengths range
#bar <- cut(foo, wl = 450:550)
#plot(bar)

#bar <- cut(foo, wl = c(450:550, 1850:2050))
#plot(bar)

# Removing a specific wavelengths range
#bar <- cut(foo, wl = -1*450:550)
#plot(bar)

#bar <- cut(foo, wl = -1*c(450:550, 1850:2050))
#plot(bar)
}