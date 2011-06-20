\name{continuum_removal}
\alias{continuum_removal}

\title{Continuum removal}

\usage{continuum_removal(obj)}

\description{Operates a continuum removal on the spectra in the collection.}

\details{
This operation is commonly done  to normalize reflectance spectra and allow comparison of individual absorption features from a common baseline. The removal is based on the upper convex hull of the spectra. 

\cite{R.N. Clark and T.L. Roush (1984). Reflectance spectroscopy: Quantitative analysis techniques for remote sensing applications, Journal of Geophysical Research 89 (B7), pp. 6329-6340.}
}

\value{An object of the same class as \code{obj} with the continuum removed from its spectra.}

\arguments{
  \item{obj}{an object inheriting from class \code{Spectra}}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}