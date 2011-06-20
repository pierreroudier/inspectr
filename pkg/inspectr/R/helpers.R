# Very same function as in ggplot2 - all credit goes to Hadley Wickham
# Just here to avoid a dependency on ggplot2
#
.try_require <- function(package){
  available <- suppressMessages(suppressWarnings(sapply(package, 
      require, quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)))
  missing <- package[!available]
  if (length(missing) > 0) 
    stop(paste(package, collapse = ", "), " package required for this functionality.  Please install and try again.", call. = FALSE)
}
