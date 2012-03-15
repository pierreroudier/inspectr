\name{mutate}
\docType{methods}
\alias{mutate}
\alias{mutate.Spectra}
\alias{mutate,Spectra-method}

\title{Mutate a Spectra* object by transforming the spectra values, and/or adding new or replacing existing attributes.}

\usage{mutate(.data, ...)}

\description{Mutate a Spectra* object by transforming the spectra values, and/or adding new or replacing existing attributes.}

\details{

This function is very similar to \code{\link{transform}} but it executes
the transformations iteratively so that later transformations can use the
columns created by earlier transformations. Like transform, unnamed
components are silently dropped.

Either the spectra, and/or the attributes (if the \code{.data} inherits from the \code{SpectraDataFrame} class) can be affected:
\itemize{
  \item {To affect the spectra, one should use the \code{nir} placeholder, eg \code{nir = log(1/nir)}}
  \item {To affect the attributes of the object, the definitions of new columns are simply given using attributes names, \code{newAttribute = 1/sqrt(attribute)}}
  \item {Both spectra and attrbutes can be transformed in one command, eg \code{mutate(s, nir = log(1/nir), newAttribute = 1/sqrt(attribute)}}
}
}

\references{
  Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data                                                                                          
  Analysis. Journal of Statistical Software, 40(1), 1-29. URL                                                                                               
  http://www.jstatsoft.org/v40/i01/.
}


\author{Pierre Roudier \url{pierre.roudier@gmail.com}, from code from Hadley Wickham}

\arguments{
  \item{.data}{an object inheriting from the \code{Spectra} class}
  \item{...}{named parameters giving definitions of new columns}
}

\examples{
# Modifying spectra
#m <- mutate(foo, nir = log1p(1/nir))
#plot(m)

# Modifying and creating attributes
#m <- mutate(foo, octane = sqrt(octane), foo = octane + 1000, nir = log1p(1/nir))
#summary(m)
#plot(m)
}
