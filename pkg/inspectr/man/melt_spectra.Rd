\name{melt_spectra}
\alias{melt_spectra}

\title{melt_spectra}
\description{Melts the spectra data of a Spectra object and returns it as wide format.}

\usage{melt_spectra(obj, ...)}

\arguments{
  \item{obj}{an object inheriting from the \code{Spectra} class}
  \item{...}{any option evaluated in the context of plyr::melt}
}

\seealso{\code{\link{melt}}}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}, from code from Hadley Wickham}