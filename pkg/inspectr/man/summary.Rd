\name{summary}

\alias{summary.Spectra}
\alias{print.summary.Spectra}

\title{Summary}

\description{Summarize a Spectra* object.}

\value{A \code{"summary.Spectra"} object}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\examples{
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500
summary(australia)
}