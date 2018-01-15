.onAttach <- function(lib, pkg)  {
    pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package = pkg), fields = c("Version", "Date")))
    
    packageStartupMessage(
      '
The inspectr package is now deprecated, and has been renamed as "spectacles". 
      
Please install the "spectacles" package from CRAN by running:

> install.packages("spectacles")

You can also visit the project page for more information at https://github.com/pierreroudier/spectacles

      '
    )

    return(invisible(0))
}
