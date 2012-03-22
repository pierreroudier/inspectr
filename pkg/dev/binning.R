## Binning of the spectra

# Create bins on WLs (or accept WLS as is?)
# Affect each existing WL to a bin
# For each spectra, for each bin, apply some kind of integrating fun (mean)
# Reassemble spectra matrix
# Update WLs (take central value?)
# IDs, data,units unchanged
# Throw the object back to the user

# r <- ddply(data.frame(group = as.numeric(cut(wl(australia), breaks = 250)), nir = spectra(australia)[1,]), .(group), function(x) median(x$nir))

