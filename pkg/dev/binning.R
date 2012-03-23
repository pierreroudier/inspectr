## Binning of the spectra

bin_spectra <- function(object, bins = NULL, reduction = NULL, intervals = NULL, fun = mean, parallel = FALSE, ...) {
  
  if (is.null(intervals) & is.null(bins) & is.null(reduction))
    stop("Please provide either wavelength intervals, compression factor or number of bins")
  
  if (length(which(laply(list(bins, reduction, intervals), is.null))) < 2)
    stop("Please provide EITHER wavelength intervals, compression factor or number of bins")

  if (!is.null(reduction))
    bins <- length(object)/reduction
  
  # Create bins based on a number of bins (regular binning)
  bins <- cut(wl(object), breaks = bins, labels = FALSE)
  
  # Affect each existing WL to a bin
  mat <- data.frame(wl = wl(object), bins = bins, t(spectra(object)), check.names = FALSE)
  mat$bins <- factor(mat$bins)
  
  levels(mat$bins) <- daply(mat, .(bins), function(x) mean(x$wl), .parallel = parallel)

  mat$bins <- as.numeric(as.character(mat$bins))

  agg_nir <- ddply(mat, .(bins), function(x) apply(x[,3:ncol(x)], 2, fun, ...), .parallel = parallel)
  
  nir <- t(agg_nir[, 2:ncol(agg_nir)])
  colnames(nir) <- agg_nir$bins
  
  spectra(object) <- nir
  
  object
}
