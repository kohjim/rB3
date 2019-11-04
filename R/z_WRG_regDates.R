## make regular dates for a data frame (to pass to aggregation)

z_WRG_regDates <- function(dt_in, timestep, pullAgg) {

### make timestamp series based on desired pull direction
if (pullAgg == "left") {

  newTS =
    as.numeric(
      lapply(
        as.numeric(dt_in$DateTime) / timestep,
        floor
      )
    ) * timestep

} else if (pullAgg == "right") {

  newTS =
    as.numeric(
      lapply(
        as.numeric(dt_in$DateTime) / timestep,
        ceiling
      )
    ) * timestep

} else {
  newTS =
    as.numeric(
      lapply(
        as.numeric(dt_in$DateTime) / timestep,
        round
      )
    ) * timestep

  }

  return(newTS)

}
