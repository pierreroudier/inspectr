

#' Aggregates spectral and data information
#' 
#' Aggregates spectral and data information of a \code{Spectra} object using a
#' user-defined function.
#' 
#' Aggregates the spectral information of a \code{Spectra} object using an
#' aggregation function chosen by the user.
#' 
#' There is two distinct function for \code{Spectra} and
#' \code{SpectraDataFrame} classes. For \code{SpectraDataFrame} objects,
#' associated data is also aggregated using the function provided by the
#' \code{fun} option. Additionally, the method for \code{SpectraDataFrame} has
#' an \code{id} option that allows to specify an attribute which will be used
#' to split the object, apply sequentially the \code{fun} function, and
#' recombine the results in an unique object.
#' 
#' @name aggregate_spectra
#' @aliases aggregate_spectra,Spectra-method
#' aggregate_spectra,SpectraDataFrame-method aggregate_spectra
#' @docType methods
#' @return An object of the same class as \code{obj}
#' @section Methods: \describe{
#' 
#' \bold{x=Spectra}
#' 
#' \code{aggregate_spectra(obj, fun=mean, ...)}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{Spectra} object \cr \tab
#' \code{fun} \tab An aggregation function \cr \tab \code{...} \tab Expressions
#' evaluated in the context of \code{fun} \cr }
#' 
#' \bold{x=SpectraDataFrame}
#' 
#' \code{aggregate_spectra(obj, fun=mean, id=NULL, ...)}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{SpectraDataFrame} object \cr
#' \tab \code{fun} \tab An aggregation function \cr \tab \code{id} \tab
#' Attribute(s) to split the object (character vector) \cr \tab \code{...} \tab
#' Expressions evaluated in the context of \code{fun} \cr }
#' 
#' }
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{apply_spectra}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Aggregation on the whole collection
#' m <- aggregate_spectra(australia, fun = mean)
#' summary(m)
#' plot(m)
#' 
#' # Aggregation factor-wise
#' australia$fact <- sample(LETTERS[1:3], size = nrow(australia), replace = TRUE) # Generate some kind of factor
#' summary(australia)
#' m <- aggregate_spectra(australia, fun = mean, id = 'fact')
#' summary(m)
#' plot(m)
#' 
NULL





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





#' Baseline correction using the baseline package
#' 
#' Estimates baselines for the spectra in the \code{obj} object, using the
#' algorithm named in 'method'.
#' 
#' The baseline package implements various algorithms for the baseline
#' correction. The following methods are available:
#' 
#' \itemize{ \item 'als': Baseline correction by 2nd derivative constrained
#' weighted regression \item 'fillPeaks': An iterative algorithm using
#' suppression of baseline by means in local windows \item 'irls' (default): An
#' algorithm with primary smoothing and repeated baseline suppressions and
#' regressions with 2nd derivative constraint \item 'lowpass': An algorithm for
#' removing baselines based on Fast Fourier Transform filtering \item
#' 'medianWindow': An implementation and extention of Mark S. Friedrichs'
#' model-free algorithm \item 'modpolyfit': An implementation of Chad A. Lieber
#' and Anita Mahadevan-Jansen's algorithm for polynomial fiting \item
#' 'peakDetection': A translation from Kevin R. Coombes et al.'s MATLAB code
#' for detecting peaks and removing baselines \item 'rfbaseline': Wrapper for
#' Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field, James A. Dodd's
#' algorithm based on LOWESS and weighted regression \item 'rollingBall': Ideas
#' from Rolling Ball algorithm for X-ray spectra by M.A.Kneen and H.J.
#' Annegarn. Variable window width has been left out }
#' 
#' See baseline package documentation for more information and references.
#' 
#' Additionally, the baseline package provides a nice GUI that helps choosing
#' the good baseline method and the good parametrisation. This GUI can be used
#' with the \code{inspectr} package. This is demonstrate in the Examples
#' section.
#' 
#' @name base_line
#' @aliases base_line base_line,Spectra-method
#' @docType methods
#' @param object an object inheriting from class \code{Spectra}
#' @param ... additional arguments to be passed to the \code{baseline} function
#' in the baseline package. The main option would be \code{'method'}, to switch
#' between the several baseline methods presented in teh details section.
#' @return An object of the same class as \code{obj} with the continuum removed
#' from its spectra.
#' @author Interface to the baseline package by Pierre Roudier
#' \url{pierre.roudier@@gmail.com}, baseline package authored by Kristian Hovde
#' Liland and Bjorn-Helge Mevik
#' @seealso \code{\link{continuum_removal}}, \code{\link{snv}},
#' \code{\link{rnv}}
#' @references Kristian Hovde Liland and Bjrn-Helge Mevik (2011). baseline:
#' Baseline Correction of Spectra. R package version 1.0-1.
#' http://CRAN.R-project.org/package=baseline
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Correction using the default method (irls)
#' bl <- base_line(australia)
#' plot(bl)
#' 
#' # Specifying another method for baseline calculation
#' bl2 <- base_line(australia, method = "modpolyfit")
#' plot(bl2)
#' 
#' # Using the baseline package independently (useful to plot the corrections)
#' \dontrun{
#' library(baseline)
#' bl3 <- baseline(spectra(australia), method = 'irls')
#' class(bl3) # this is a baseline object
#' plot(bl3)
#' # Affecting the baseline-corrected spectra back
#' # to the SpectraDataFrame object
#' spectra(australia) <- getCorrected(bl3)
#' plot(australia)
#' 
#' # Using the baselineGUI with inspectr
#' baselineGUI(spectra(australia))
#' ## When happy with a configuration, clik "Apply to all" and 
#' ## save the results under a name, e.g. "corrected.spectra"
#' spectra(australia) <- getCorrected(corrected.spectra)
#' plot(australia)
#' }
#' 
NULL





#' Sets spatial coordinates to create \code{SpatialSpectraDataFrame} objects
#' 
#' Sets spatial coordinates to create \code{SpatialSpectraDataFrame} objects.
#' Adapted from the \code{"coordinates<-"} method in the \bold{sp} package.
#' 
#' 
#' @name coordinates
#' @aliases coordinates<- coordinates<-,SpectraDataFrame-method
#' @docType methods
#' @param object A \code{SpectraDataFrame} object
#' @param value Spatial coordinates: either a matrix, list, or data frame with
#' numeric data, or column names, column number or a reference: a formula (in
#' the form of e.g. \code{~x+y}), column numbers (e.g. \code{c(1,2)}) or column
#' names (e.g. \code{c("x","y")}) specifying which columns in \code{object} are
#' the spatial coordinates. If the coordinates are part of object, giving the
#' reference does not duplicate them, giving their value does duplicate them in
#' the resulting structure.
#' @return A \code{SpatialSpectraDataFrame} object
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}, from code from the
#' \bold{sp} package.
#' @seealso \code{\link{spectra}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # The australia dataset does not have associated coordinates.
#' # To fake it is a spatial dataset, we will associate each spectra
#' # in the collection to a location in the meuse dataset provided
#' # by the sp package
#' 
#' # Loding the meuse.grid dataset from the sp package
#' library(sp)
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' 
#' # We randomly generate sampling locations on this grid
#' coords <- spsample(meuse.grid, n=nrow(australia)*2, type="random")
#' class(coords)
#' 
#' # We associate these locations to the spectra in the collection
#' #
#' 
#' # First, we add the coordinates in the data slot data
#' features(australia, safe=FALSE) <- as.data.frame(coords)[1:nrow(australia), ]
#' names(australia)
#' 
#' # Then we promote the SpectraDataFrame object to a SpatialDataFrame object
#' coordinates(australia) <- ~x+y
#' class(australia)
#' str(australia)
#' 
NULL





#' Manipulating the wavelength range of a \code{Spectra} object
#' 
#' This methods allows to either select or remove a specific range of
#' wavelengths from a \code{Spectra} object.
#' 
#' The wavelengths are extracted if \code{wl > 0}, or removed if \code{wl < 0}.
#' You can't mix negative and positive index in \code{wl}.
#' 
#' @name cut
#' @aliases cut cut,Spectra-method
#' @docType methods
#' @param x an object inheriting from class \code{Spectra}
#' @param wl a vector of the wavelengths to either select or remove from
#' \code{x}
#' @param ... ignored
#' @return An object of the same class as \code{x}.
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{[}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Extracting a specific wavelengths range
#' s <- cut(australia, wl = 450:550)
#' plot(s)
#' 
#' s <- cut(australia, wl = c(450:550, 1850:2050))
#' plot(s)
#' 
#' # Removing a specific wavelengths range
#' s <- cut(australia, wl = -1*450:550)
#' plot(s)
#' 
#' s <- cut(australia, wl = -1*c(450:550, 1850:2050))
#' plot(s)
#' 
NULL





#' Extracting and replacing parts of Spectra* objects
#' 
#' These methods emulates classic base methods '[', '[[' and '$' to extract or
#' replace parts of Spectra* objects.
#' 
#' 
#' @name extraction-methods
#' @aliases [ [<- [[ [[<- $ $<- [,Spectra,ANY,ANY,missing-method
#' [,SpectraDataFrame,ANY,ANY,missing-method
#' [[,SpectraDataFrame,ANY,missing-method [[<-,Spectra,ANY,missing-method
#' [<-,SpectraDataFrame-method $<-,Spectra-method $,SpectraDataFrame-method
#' @docType methods
#' @return These methods either return an object of the same clss as \code{x},
#' or can promote a \code{Spectra} object to a \code{SpectraDataFrame} object
#' by adding data ("[[<-" and "$<-" methods).
#' @section Methods: \describe{
#' 
#' \bold{x=Spectra}
#' 
#' \code{x[i, j, ..., drop = FALSE]}
#' 
#' \code{x$name <- value}
#' 
#' \code{x[[name]] <- value}
#' 
#' \tabular{rll}{ \tab \code{x} \tab A \code{Spectra} object \cr \tab \code{i}
#' \tab Row index of the selected individuals \cr \tab \code{j} \tab Selected
#' wavelengths \cr \tab \code{name} \tab A literal character string or a name
#' \cr \tab \code{...} \tab Ignored \cr \tab \code{drop} \tab Ignored \cr }
#' 
#' \bold{x=SpectraDataFrame}
#' 
#' \code{x[i, j, k, ..., drop = FALSE]}
#' 
#' \code{x[[name]]}
#' 
#' \code{x[[name]] <- value}
#' 
#' \code{x$name}
#' 
#' \code{x$name <- value}
#' 
#' \tabular{rll}{ \tab \code{x} \tab A \code{SpectraDataFrame} object \cr \tab
#' \code{i} \tab Row index of the selected individuals \cr \tab \code{j} \tab
#' Selected wavelengths \cr \tab \code{k} \tab Selected columns in the @data
#' slot\cr \tab \code{name} \tab A literal character string or a name \cr \tab
#' \code{...} \tab Ignored \cr \tab \code{drop} \tab Ignored \cr } }
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{subset}}, \code{\link{aggregate_spectra}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Getting features information from SpectraDataFrame
#' australia$carbon
#' australia[['carbon']]
#' 
#' # Creating new features
#' australia$foo <- runif(nrow(australia))
#' australia[['bar']] <- runif(nrow(australia))
#' summary(australia)
#' 
#' # Replacing values
#' australia$foo <- sample(LETTERS[1:5], size = nrow(australia), replace = TRUE)
#' australia[['bar']] <- sample(c(TRUE, FALSE), size = nrow(australia), replace = TRUE)
#' summary(australia)
#' 
#' # Promote Spectra to SpectraDataFrame
#' s <- as(australia, 'Spectra')
#' class(s)
#' s$foo <- runif(nrow(s))
#' summary(s)
#' 
NULL





#' Retrieves or sets the data slot of a SpectraDataFrame object.
#' 
#' Either retrieves the attributes values from the data slot of a
#' SpectraDataFrame object, or upgrades a Spectra object to a SpectraDataFrame
#' object by initialising its data slot by a suitable \code{"data.frame"}
#' object.
#' 
#' 
#' @name features
#' @aliases features features<- features-methods
#' features,SpectraDataFrame-method features<-,Spectra-method
#' features<-,SpectraDataFrame-method
#' @docType methods
#' @return The \code{features} methods return a \code{data.frame} object, while
#' the \code{"features<-"} methods return a \code{SpectraDataFrame} object.
#' @section Methods: \describe{
#' 
#' \bold{x=Spectra}
#' 
#' \code{features(obj, safe=TRUE, key=NULL, exclude_id=TRUE) <- value}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{Spectra} object \cr \tab
#' \code{safe} \tab Logical. If TRUE, data is being added to the object using a
#' SQL join (using a key field given by the \code{key} option), otherwise it is
#' assumed the order of the rows is consitent with the order of the rows in
#' \code{obj} \cr \tab \code{key} \tab Character, name of the column of the
#' data.frame storing the ids for the SQL join. Ignored if \code{safe} is
#' \code{FALSE}. \cr \tab \code{exclude_id} \tab Logical, if \code{TRUE}, ids
#' used for the SQL join are removed from the data slot after the join. \cr }
#' 
#' \bold{x=SpectraDataFrame}
#' 
#' \code{features(obj, exclude_id=TRUE)}
#' 
#' \code{features(obj, safe=TRUE, key=NULL, exclude_id=TRUE, append=TRUE) <-
#' value}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{SpectraDataFrame} object \cr
#' \tab \code{safe} \tab Logical. If TRUE, data is being added to the object
#' using a SQL join (using a key field given by the \code{key} option),
#' otherwise it is assumed the order of the rows is consitent with the order of
#' the rows in \code{obj} \cr \tab \code{key} \tab Character, name of the
#' column of the data.frame storing the ids for the SQL join. Ignored if
#' \code{safe} is \code{FALSE}. \cr \tab \code{exclude_id} \tab Logical. For
#' the \code{features} method, if \code{TRUE}, the spectra ids are added to the
#' \code{data.frame} that is returned. For the \code{"features<-"} method, If
#' \code{TRUE}, ids used for the SQL join are removed from the data slot after
#' the join. \cr \tab \code{append} \tab Logical, if \code{TRUE}, the data is
#' appended to any existing data. if FALSE, the data provided is erasing any
#' existing data. \cr }
#' 
#' }
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{spectra}}, \code{\link{wl}},
#' \code{\link{SpectraDataFrame-class}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Printing available data
#' features(australia)
#' 
#' # Promoting a Spectra to a SpectraDataFrame object
#' s <- as(australia, "Spectra")
#' # Generating dummy data
#' d <- data.frame(id = ids(australia), foo = runif(nrow(australia)), bar = sample(LETTERS[1:5], size = nrow(australia), replace = TRUE))
#' head(d)
#' # Affecting data to Spectra object
#' features(s, key = 'id') <- d
#' summary(s)
#' 
#' # Adding data to an existing SpectraDataFrame object
#' features(australia, key = 'id') <- d
#' features(australia)
#' 
#' # Replacing data of an existing SpectraDataFrame object
#' features(australia, key = 'id', append = FALSE) <- d
#' features(australia)
#' 
NULL





#' Kennard-Stone algorithm for optimal calibration set selection.
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~ An
#' implemnentation of the Kennard-Stone algorithm for calibration set
#' selection.
#' 
#' 
#' @aliases kenstone kenstone,Spectra-method
#' @param x a \code{Spectra} object
#' @param size a positive number, the number of items to choose from
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} A vector of length size giving the indices of the selected
#' individuals in x. %% ...
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @references %% ~put references to the literature/web site here ~ R.W.
#' Kennard, L.A. Stone, Technometrics 11 (1969) 137.
NULL





#' melt_spectra
#' 
#' Melts the spectra data of a Spectra object and returns it as wide format.
#' 
#' This function is very useful when wanting to plot spectra using the lattice
#' or ggplot2 packages.
#' 
#' @name melt_spectra
#' @aliases melt_spectra melt_spectra-methods melt_spectra,Spectra-method
#' melt_spectra,SpectraDataFrame-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \bold{x=Spectra}
#' 
#' \code{melt_spectra(obj, ...)}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{Spectra} object \cr \tab
#' \code{...} \tab Ignored \cr }
#' 
#' \bold{x=SpectraDataFrame}
#' 
#' \code{melt_spectra(obj, attr=NULL, ...)}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{SpectraDataFrame} object \cr
#' \tab \code{attr} \tab Character, the name of an attribute in the object data
#' to split the spectra against. \cr \tab \code{...} \tab Ignored \cr }
#' 
#' }
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{melt}}
#' @references Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data
#' Analysis. Journal of Statistical Software, 40(1), 1-29. URL
#' http://www.jstatsoft.org/v40/i01/.
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Simple melt
#' r <- melt_spectra(australia)
#' head(r)
#' 
#' \dontrun{
#' # Melt against some factor (or continuous data), and plot
#' # using ggplot2
#' 
#' # Create some factor
#' australia$fact <- sample(LETTERS[1:3], size = nrow(australia), replace = TRUE) # Generate some kind of factor
#' r <- melt_spectra(australia, attr = 'fact')
#' 
#' # Create plot
#' library(ggplot2)
#' p <- ggplot(r) + geom_line(aes(x=wl, y=nir, group=id, colour=fact)) + theme_bw()
#' print(p)
#' }
#' 
NULL





#' Mutate a Spectra* object by transforming the spectra values, and/or adding
#' new or replacing existing attributes.
#' 
#' Mutate a Spectra* object by transforming the spectra values, and/or adding
#' new or replacing existing attributes.
#' 
#' 
#' This function is very similar to \code{\link{transform}} but it executes the
#' transformations iteratively so that later transformations can use the
#' columns created by earlier transformations. Like transform, unnamed
#' components are silently dropped.
#' 
#' Either the spectra, and/or the attributes (if the \code{.data} inherits from
#' the \code{SpectraDataFrame} class) can be affected: \itemize{ \item To
#' affect the spectra, one should use the \code{nir} placeholder, eg \code{nir
#' = log(1/nir)} \item To affect the attributes of the object, the definitions
#' of new columns are simply given using attributes names, \code{newAttribute =
#' 1/sqrt(attribute)} \item Both spectra and attrbutes can be transformed in
#' one command, eg \code{mutate(s, nir = log(1/nir), newAttribute =
#' 1/sqrt(attribute)} }
#' 
#' @name mutate
#' @aliases mutate mutate.Spectra mutate,Spectra-method
#' @docType methods
#' @param .data an object inheriting from the \code{Spectra} class
#' @param ... named parameters giving definitions of new columns
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}, from code from
#' Hadley Wickham
#' @references Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data
#' Analysis. Journal of Statistical Software, 40(1), 1-29. URL
#' http://www.jstatsoft.org/v40/i01/.
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Modifying spectra
#' m <- mutate(australia, nir = log1p(1/nir))
#' plot(m)
#' 
#' # Modifying and creating attributes
#' m <- mutate(australia, sqrt_carbon = sqrt(carbon), foo = clay + ph, nir = log1p(1/nir))
#' summary(m)
#' plot(m)
#' 
NULL





#' Retrieve dimensions of Spectra* objects
#' 
#' Retrieves the wavelengths units and the spectral resolution from
#' \code{Spectra*} objects.
#' 
#' * Methods for \code{Spectra} objects
#' 
#' \code{nrow} returns the number of individuals in the collection
#' \code{length} returns the number of wavelengths in the collection
#' \code{ncol} returns NULL \code{dim} returns a vector containing (1) the
#' number of individuals and (2) in the number of wavelengths in the collection
#' 
#' * Methods for \code{Spectra} objects
#' 
#' \code{nrow} returns the number of individuals in the collection
#' \code{length} returns the number of wavelengths in the collection
#' \code{ncol} returns the number of attributes in the collection \code{dim}
#' returns a vector containing (1) the number of individuals, (2) in the number
#' of wavelengths, and (3) the number of attributes in the collection
#' 
#' @name nrow
#' @aliases nrow,Spectra-method ncol,Spectra-method dim length dim.Spectra
#' length.Spectra
#' @docType methods
#' @param x For \code{nrow}, \code{length}, \code{dim}, a \code{Spectra}
#' object. For \code{ncol}, a \code{SpectraDataFrame} object.
#' @return \code{nrow}, \code{ncol}, \code{nwl}, and \code{length}, return an
#' \code{integer}.
#' 
#' \code{dim} returns a vector of length 2 or 3 (see Details section).
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' nrow(australia)
#' ncol(australia)
#' length(australia)
#' dim(australia)
#' 
NULL





#' Stacking \code{Spectra} objects together
#' 
#' This method stacks two or more \code{Spectra*} objects together.
#' 
#' 
#' @aliases rbind.Spectra rbind.SpectraDataFrame
#' @param \dots The \code{Spectra} objects to be combined.
#' @param create_new_ids allows creation of new ids if the ids of the
#' \code{Spectra*} objects you are trying to stack are redondant
#' @param new_ids vector of new ids to be given to the new object
#' @return a \code{Spectra*} object.
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' s <- rbind(australia, australia, create_new_ids = TRUE)
#' summary(s)
#' 
#' l <- separate(australia, calibration = 0.6)
#' s <- rbind(l$validation, l$calibration)
#' summary(s)
#' 
NULL





#' Separates a \code{Spectra*} object into a calibration and a validation set.
#' 
#' Separates a \code{Spectra*} object into a calibration and a validation set.
#' 
#' 
#' @name separate
#' @aliases separate separate.Spectra separate,Spectra-method
#' @docType methods
#' @param obj an object inheriting from class \code{SpectraDataFrame}
#' @param calibration The fraction of the dataset to be put in the calibration
#' set
#' @param ... Ignored
#' @return An list with two \code{SpectraDataFrame} objects, one for the
#' calibration, and the other for the validation.
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{split}}, \code{\link{melt_spectra}},
#' \code{\link{lapply}}, and the \code{l*pply} family of function inthe plyr
#' package.
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' l <- separate(australia, calibration=0.7)
#' # The result is a list of two Spectra* objects
#' str(l)
#' lapply(l, nrow)
#' 
#' summary(l$calibration)
#' summary(l$validation)
#' 
NULL





#' Spectra* classes
#' 
#' The inspectr package provides the user with S4 classes that have been
#' developped to store and manipulate spectroscopy data.
#' 
#' The \code{Spectra} class is storing the spectra matrix, along with the
#' wavelengths at which those have been measured, the units in whioch those
#' wavelengths are expressed, and a unique id for each sample in the
#' collection.
#' 
#' The \code{SpectraDataFrame} class is extending the \code{Spectra} class by
#' giving the opportunity to store attribute data along with the spectra - this
#' is mostly the case when we want to predict physical or chemical properties
#' from the spectra set.
#' 
#' The \code{SpatialSpectra} and \code{SpatialSpectraDataFrame} classes are
#' extending the \code{Spectra} and \code{SpectraDataFrame} classes using the
#' \code{SpatialPoints} class from package sp. This allows to store spatial
#' information on the dataset: coordinates, coordinate reference system,
#' bounding box, etc.
#' 
#' Common generic methods implemented for these classes include:
#' 
#' \code{summary}, \code{show}, \code{nrow}, \code{length}, \code{plot},
#' \code{[}, \code{[[}, \code{$}.
#' 
#' \code{SpatialPoints} methods from the sp package can be applied to
#' \code{SpatialSpectra} and \code{SpatialSpectraDataFrame} objects are they
#' inherit from this class.
#' 
#' 
#' @name Spectra-class
#' @aliases inspectr Spectra-class SpectraDataFrame-class SpatialSpectra-class
#' SpatialSpectraDataFrame-class as.data.frame.Spectra
#' as.data.frame.SpectraDataFrame as.data.frame.SpatialSpectra
#' names.SpectraDataFrame names<-.SpectraDataFrame print,Spectra-method
#' show,Spectra-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Spectra", ...)}, with the constructor functions like
#' \code{Spectra(...)}, or with the helper functions such as \code{wl} and
#' \code{spectra}.
#' @author Pierre Roudier \email{pierre.roudier@@gmail.com}
#' @examples
#' 
#' showClass("Spectra")
#' showClass("SpectraDataFrame")
#' 
NULL





#' Retrieves or sets the spectra of a \code{Spectra*} objects.
#' 
#' Either retrieves the spectra matrix from a \code{Spectra*} object, or
#' creates a \code{Spectra*} object from a \code{"data.frame"} object different
#' interfaces detailed below.
#' 
#' When applied to a \code{Spectra*} object, this functions simply returns the
#' spectra it is storing.
#' 
#' If applied on a \code{"data.frame"} object, it is an helper function to
#' create a \code{Spectra*} object. Two kind of interfaces are then available.
#' \code{value} can be: \describe{ \item{a vector:}{Similarly to
#' \code{\link{wl}}, the wavelengths of the spectra can be passed by a
#' \code{"numeric"} vector. Alternatively, the names of the columns that
#' contain the spectra information can be passed as a \code{"character"}
#' vector.}
#' 
#' \item{a formula:}{This interface is specific to inspectr. It follows a
#' scheme where differents parts can be observed, the id column, the attributes
#' columns, and the spectra columns, described by the wavelengths at which it
#' has been measured:}
#' 
#' \itemize{ \item \bold{Placeholders:} \itemize{ \item\code{...}:placeholder
#' for all the columns of your data.frame object except those that have been
#' already used in other parts of the formula. This can lead to errors. E.g. if
#' \code{object} has data one every wavelength between 350 and 2500 nm,
#' \code{spectra(object) <- id_field ~ ... ~ 500:2500} will stores the columns
#' corresponding to the wavelengths 350-499 nm in its data slot!
#' \item\code{id}:For the creation of a \code{SpectraDataFrame}, it is
#' important to always specify an id field in the formula. If no id column is
#' present, the \code{id} placeholder will create one for you.
#' 
#' }
#' 
#' \item \code{spectra(object) <- ~ 350:2500} will build a \code{Spectra}
#' object from the wavelengths between 350 and 2500, based on the column names.
#' 
#' \item \code{spectra(object) <- ~ 350:2:2500} will build a \code{Spectra}
#' object from the wavelengths in \code{seq(350, 2500, by = 2)}.
#' 
#' \item \code{spectra(object) <- ~ 500:2350} will build a \code{Spectra}
#' object from the wavelengths between 500 and 2350, even though other
#' wavelengths are present (they will be dropped).
#' 
#' In the three later cases, the id field has been dropped (it will be
#' automatically created). If you want to use a column of \code{"data.frame"}
#' as an id filed, you can still use the first part of the formula:
#' 
#' \item \code{spectra(object) <- id_field ~ 350:2500} \item
#' \code{spectra(object) <- id_field ~ 350:5:2500}
#' 
#' Some data can also be added to the object, which will then be of
#' \code{SpectraDataFrame} class:
#' 
#' \item \code{spectra(object) <- id_field ~ property1 ~ 500:2300} will create
#' a \code{SpectraDataFrame} with ids from the id_field column, data from the
#' property1 column, and spectral information between 500 and 2300 nm. That
#' means that data property2, and all spectral information from bands < 500 nm
#' or > 2300 nm will be dropped.
#' 
#' You can also combine the placeholders:
#' 
#' \item \code{spectra(object) <- id_field ~ ... ~ 350:2500} will create a
#' \code{SpectraDataFrame} object with ids from the id_field column, all
#' spectral bands between 350 and 2500 nm. The data slot is given all the
#' remaining columns.  } }
#' 
#' @name spectra
#' @aliases spectra spectra<- spectra,Spectra-method
#' spectra<-,data.frame-method spectra<-,Spectra-method
#' spectra<-,SpectraDataFrame-method
#' @docType methods
#' @return If applied on a \code{"data.frame"}, either a \code{Spectra} or a
#' \code{SpectraDataFrame} object. If applied on a \code{Spectra*} object, a
#' matrix.
#' @section Methods: \describe{
#' 
#' \bold{obj=data.frame}
#' 
#' \code{spectra(obj, ..., mode="rowwise") <- value}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{"data.frame"} object \cr \tab
#' \code{mode} \tab A character describing the data representation of
#' \code{object} ; it can be either \code{'rowwise'} (default value) or
#' \code{'colwise'}. See Details section. \cr \tab \code{...} \tab Ignored \cr
#' \tab \code{value} \tab A representation of the wavelengths of the
#' \code{Spectra*} object to create. See details section. \cr }
#' 
#' \bold{obj=Spectra}
#' 
#' \code{spectra(obj)}
#' 
#' \tabular{rll}{ \tab \code{obj} \tab A \code{Spectra} object \cr \tab
#' \code{...} \tab Ignored \cr }
#' 
#' }
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{wl}}, \code{\link{Spectra-class}},
#' \code{\link{SpectraDataFrame-class}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' class(australia) # this is a simple data.frame
#' # structure of the data.frame: it is rowwise-formatted
#' big.head(australia) 
#' 
#' ## CREATING Spectra OBJECTS
#' ##
#' 
#' # Using spectra() to initiate a Spectra from 
#' # the data.frame
#' spectra(australia) <- sr_no ~ 350:2500
#' summary(australia)
#' 
#' # It is possible to select wavelengths using the formula interface
#' data(australia)
#' spectra(australia) <- sr_no ~ 350:5:2500
#' summary(australia)
#' 
#' data(australia)
#' spectra(australia) <- sr_no ~ 500:1800
#' summary(australia)
#' 
#' ## CREATING SpectraDataFrame OBJECTS
#' ##
#' 
#' # Using spectra() to initiate a SpectraDataFrame from 
#' # the data.frame
#' data(australia)
#' spectra(australia) <- sr_no ~ carbon + ph + clay ~ 350:2500
#' summary(australia)
#' 
#' # Selecting data to be included in the SpectradataFrame object
#' data(australia)
#' spectra(australia) <- sr_no ~ carbon ~ 350:2500
#' summary(australia)
#' 
#' # Forcing the creation of new ids using the id keyword in the 
#' # formula interface
#' data(australia)
#' spectra(australia) <- id ~ carbon ~ 350:2500
#' summary(australia)
#' ids(australia, as.vector = TRUE)
#' 
#' # Using the "..." short-hand to select all the remaining columns
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' summary(australia)
#' 
#' ## CREATING Spectra OBJECTS FROM
#' ## BY-COLS-FORMATTED DATA
#' ##
#' 
#' # For data formatted in the colwise format, 
#' # use the "colwise" mode
#' 
#' # Transforming data into colwise format
#' # for demonstration's sake
#' #
#' m <- melt_spectra(australia)
#' australia_by_col <- reshape2::acast(m, ... ~ sr_no)
#' australia_by_col <- data.frame(wl = rownames(australia_by_col), australia_by_col, check.names = FALSE)
#' 
#' # Here's colwise-formatted data 
#' big.head(australia_by_col)
#' 
#' # Convert it into Spectra object
#' spectra(australia_by_col, mode = "colwise") <- wl ~ ...
#' summary(australia_by_col)
#' 
#' # Then data can be added to promote it as a SpectraDataFrame
#' my.data <- features(australia, exclude_id = FALSE)
#' features(australia_by_col, key = 'sr_no') <- my.data
#' summary(australia_by_col)
#' 
NULL





#' "Splicing" and interpolation of spectra
#' 
#' This method mimicks the "splicing" method available in the ViewSpec Pro
#' software from ASD, which aims at correcting steps in the data.
#' 
#' It removes parts of the spectra defined by the \code{wl} vector, and
#' interpolates these parts using a method chosen using the \code{method}
#' option.
#' 
#' This function is a wrapper around \code{signal::interp1}.
#' 
#' @aliases "Splicing" and interpolation splice,Spectra-method
#' @param x a \code{Spectra} object
#' @param wl the wavelengths to cut out and interpolate
#' @param method the interpolation method. Available options are "linear",
#' "nearest", "pchip", "cubic", and "spline".
#' @return an object of same class as x
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @examples
#' 
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' oz_spliced <- splice(australia, wl = c(725:1020, 1801:1950), method = "spline")
#' plot(oz_spliced)
#' 
NULL





#' Split a Spectra* object using factors
#' 
#' Splits a Spectra* object into groups using a factor, either a provided as a
#' vector or as an attribute in the features of the object.
#' 
#' This is an adaptation of the \code{split} function in the base package.
#' 
#' @name split
#' @aliases split split.Spectra split,Spectra-method
#' @docType methods
#' @param x Spectra object
#' @param f either a vector of factors (for objects inheriting from
#' \code{Spectra}), or the name or indice of an attribute in the data slot of
#' an object inheriting from \code{SpectraDataFrame}
#' @param drop ignored
#' @param ... further potential arguments passed to methods.
#' @return A list of objects of same class as \code{x}.
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{separate}}, \code{\link{melt_spectra}},
#' \code{\link{lapply}}, and the \code{l*pply} family of function inthe plyr
#' package.
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # On a Spectra object, we need to provide a vector of factors
#' # to split the object
#' s <- as(australia, 'Spectra')
#' # We make up some kind of factor to split the data. 
#' idx <- sample(letters[1:5], replace = TRUE, size = nrow(s)) # This is a vector
#' r <- split(s, idx)
#' str(r)
#' 
#' # On a SpectradataFrame object, we can also provide the name or index 
#' # of an attribute
#' australia$fact <- sample(LETTERS[1:3], size = nrow(australia), replace = TRUE) # Generate some kind of factor
#' summary(australia)
#' r <- split(australia, 'fact')
#' str(r)
#' 
#' # A list is returned, and is thus ready for use with lapply, or any
#' # of the l*ply functions from the plyr package
#' lapply(r, nrow)
#' 
NULL





#' Subset SpectraDataFrame object
#' 
#' Returns subsets of a SpectraDataFrame object.
#' 
#' 
#' @name subset
#' @aliases subset subset.SpectraDataFrame subset,SpectraDataFrame-method
#' @docType methods
#' @param x SpectraDataFrame object
#' @param ... Additional arguments: \itemize{ \itemsubsetlogical expression
#' indicating elements or rows to keep: missing values are taken as false.
#' \itemselectexpression, indicating columns to select from the data slot.
#' \itemdroppassed on to "[" indexing operator.  }
#' @return SpectraDataFrame object
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @seealso \code{\link{mutate}}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Subset on attributes
#' s <- subset(australia, carbon < 5)
#' summary(s)
#' 
#' # Subset and selection of attributes
#' s <- subset(australia, carbon < 5, select = 1)
#' summary(s)
#' 
#' 
NULL





#' Summary
#' 
#' Summarize a Spectra* object.
#' 
#' 
#' @aliases summary.Spectra print.summary.Spectra
#' @return A \code{"summary.Spectra"} object
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @examples
#' 
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' summary(australia)
#' 
NULL





#' Wavelengths of Spectra* objects
#' 
#' Retrieves the wavelengths units and the spectral resolution from
#' \code{Spectra*} objects.
#' 
#' 
#' @name wl_units
#' @aliases wl_units wl_units<- res res.numeric res.integer res.Spectra
#' wl_units,Spectra-method wl_units<-,Spectra-method res,numeric-method
#' res,integer-method res,Spectra-method
#' @docType methods
#' @param object,x an object inheriting from class \code{Spectra}, or a
#' \code{"numeric"} vector for \code{resolution}
#' @param value a character string
#' @return A vector. \code{res} is giving NULL for irregularly spaced
#' wavelengths (eg when there is parts of the spectra data have been removed).
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @examples
#' 
#' # Loading example data
#' data(australia)
#' spectra(australia) <- sr_no ~ ... ~ 350:2500
#' 
#' # Print wavelength information
#' wl(australia)
#' range(wl(australia))
#' res(australia)
#' 
#' # Manipulate wavelength information
#' wl(australia) <- wl(australia) + 1000
#' wl(australia)
#' 
NULL



