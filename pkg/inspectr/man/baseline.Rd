\name{base_line}
\docType{methods}
\alias{base_line}
\alias{base_line,Spectra-method}

\title{Baseline correction using the baseline package}

\usage{base_line(object, ...)}

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
  \item{object}{an object inheriting from class \code{Spectra}}
  \item{...}{additional arguments to be passed to the \code{baseline} function in the baseline package. The main option would be \code{'method'}, to switch between the several baseline methods presented in teh details section.}
}

\references{
Kristian Hovde Liland and Bjrn-Helge Mevik (2011). baseline: Baseline Correction of Spectra. R package version 1.0-1. http://CRAN.R-project.org/package=baseline
}

\author{Interface to the baseline package by Pierre Roudier \url{pierre.roudier@gmail.com}, baseline package authored by Kristian Hovde Liland and Bjorn-Helge Mevik}

\seealso{\code{\link{continuum_removal}}, \code{\link{snv}}, \code{\link{rnv}}}

\examples{
# Correction using the default method (irls)
#plot(foo)
#bar <- base_line(foo)
#plot(bar)

# Specifying another method for baseline calculation
#plot(foo)
#bar <- base_line(foo, method = "rfbaseline")
#plot(bar)

# Using the baseline package independently (useful to plot the corrections)
#plot(foo)
#bl <- baseline(spectra(foo), method = 'irls')
#class(bl) # this is a baseline object
#plot(bl)
#spectra(foo) <- getCorrected(bl)
#plot(foo)

# Using the baselineGUI with inspectr
# plot(df)
# baselineGUI(spectra(df))
## When happy with a configuration, clik "Apply to all" and 
## save the results under a name, e.g. "corrected.spectra"
# spectra(df) <- getCorrected(corrected.spectra)
# plot(df)
}