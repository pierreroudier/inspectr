\name{aggregate_spectra}
\alias{aggregate_spectra}

\title{Aggregates spectral and data information}

\usage{aggregate_spectra(obj, fun=mean, ...)}

\description{Aggregates spectral and data information of a \code{Spectra} object using a user-defined function.}

\details{Aggregates the spectral information of a \code{Spectra} object using 
an aggregation function chosen by the user.If some data is also
present, it is aggregated using the same function.}

\value{An object of the same class as \code{obj}}

\arguments{
  \item{obj}{an object inheriting from class \code{Spectra}}
  \item{fun}{an aggregation function}
  \item{...}{expressions evaluated in the context of \code{fun}}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}