'SoilProfiles' <- function(
  data,
  samples_id = "id",
  profiles_id,
  top = "top",
  bottom = "bottom"
  ) {
  data <- data[, c(samples_id, profiles_id, top, bottom)]

  depths <- dlply(
    data, 
    profiles_id, 
    function(x) 
      subset(x, select = c(top, bottom))
  )

  profiles <- dlply(
    data,
    profiles_id,
    function(x)
      as.character(x[, samples_id, drop = TRUE])
  )

  new(".SoilProfiles", depths = depths, profiles_id = profiles)
  
}
