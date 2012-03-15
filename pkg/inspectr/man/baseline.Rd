\name{baseline}
\docType{methods}
\alias{baseline}
\alias{baseline,Spectra-method}

\title{Baseline correction using the baseline package}

\usage{baseline(spectra, method = 'irls', ...)}

\description{Estimates baselines for the spectra in the \code{obj} object, using the algorithm named in 'method'.}

\details{
The baseline package implements various algorithms for the baseline correction. The following methods are available:

\itemize{
\item 'als': Baseline correction by 2nd derivative constrained weighted regression
\item 'fillPeaks':  An iterative algorithm using suppression of baseline by means in local windows
\item 'irls' (default): An algorithm with primary smoothing and repeated baseline suppressions and regressions with 2nd derivative constraint
\item 'lowpass': An algorithm for removing baselines based on Fast Fourier Transform filtering
\item 'medianWindow': An implementation and extention of Mark S. Friedrichs' model-free algorithm
\item 'modpolyfit': An implementation of Chad A. Lieber and Anita Mahadevan-Jansen's algorithm for polynomial fiting
\item 'peakDetection': A translation from Kevin R. Coombes et al.'s MATLAB code for detecting peaks and removing baselines
\item 'rfbaseline': Wrapper for Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field, James A. Dodd's algorithm based on LOWESS and weighted regression
\item 'rollingBall': Ideas from Rolling Ball algorithm for X-ray spectra by M.A.Kneen and H.J. Annegarn. Variable window width has been left out
}

See baseline package documentation for more information and references.

Additionally, the baseline package provides a nice GUI that helps choosing the good baseline method and the good parametrisation. This GUI can be used with the \code{inspectr} package. This is demonstrate in the Examples section.
}

\value{An object of the same class as \code{obj} with the continuum removed from its spectra.}

\arguments{
  \item{spectra}{an object inheriting from class \code{Spectra}}
  \item{method}{baseline correction method available in the baseline package}
  \item{...}{additional arguments to be passed to \code{method}}
}

\references{
Kristian Hovde Liland and Bjrn-Helge Mevik (2011). baseline: Baseline Correction of Spectra. R package version 1.0-1. http://CRAN.R-project.org/package=baseline
}

\author{Interface to the baseline package by Pierre Roudier \url{pierre.roudier@gmail.com}, baseline package authored by Kristian Hovde Liland and Bjorn-Helge Mevik}

\seealso{\code{\link{continuum_removal}}, \code{\link{snv}}, \code{\link{rnv}}}

\examples{

## Not run:
# plot(df)
# baselineGUI(spectra(df))
## When happy with a configuration, clik "Apply to all" and 
## save the results under a name, e.g. "corrected.spectra"
# spectra(df) <- getCorrected(corrected.spectra)
# plot(df)
}