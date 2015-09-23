#' Australia spectra library data set
#' 
#' This data set gathers 100 soil spectra, along side with their organic
#' carbon, pH and clay content values. This data set has been collected by
#' CSIRO.
#' 
#' The \code{data.frame} contains the following columns: \itemize{ \itemsr_no a
#' unique identifier for each spectrum \itemcarbon soil organic carbon values
#' \itemph soil pH values \itemclay soil clay values \itemX350, X351, \dots{},
#' X2499, X2500 reflectance in wavelengths 350 to 2500nm }
#' 
#' 
#' @name australia
#' @docType data
#' @author Data kindly contributed by Raphael Viscarra Rossel. To reduce the
#' size of the package, the original dataset has been reduced to 100 spectra
#' using the Kennard-Stone algorithm.
#' @references Viscarra Rossel, R. and Behrens, T. 2010.
#' @keywords datasets
#' @examples
#' 
#' data(australia)
#' big.head(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' summary(australia)
#' 
NULL
