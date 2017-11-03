LagUnits <- function(units){

  units.table <- data.table::fread(
    header = T,
    input = paste(
      sep = '\n',
      'Key, Value',
      'months,months',
      'quarters,quarters',
      'semesters,semesters',
      'years,years',
      'halfyears,semesters',
      'semiannums,semesters'
    ))

  temp <- units.table[Key == units, Value]

  if (length(temp) != 1){
    valid.units <- units.table[, paste(unique(Value), collapse = ", ")]
    msg <- paste0("'", units, "' is not a valid time unit in `rIBNP` and ",
                  'cannot be interpretted as one. Valid units are ',
                  valid.units, ".")
    stop(msg)
  }

  if (temp != units){
    msg <- paste0("'", units, "' is being intepretted as '", temp, "'.")
    warning(msg)
  }

  return(temp)

}


SortCode <- function(x, units = c('months', 'quarters', 'semesters', 'years')){

  units <- LagUnits(units)

  PositionInYear <- switch(
    units,
    months = lubridate::month,
    quarters = lubridate::quarter,
    semesters = lubridate::semester,
    years = function(x) 0)

  units.per.year <- switch(
    units,
    months = 12L, quarters = 4L, semesters = 2L, years = 1L)

  year <- lubridate::year(x)

  return(year * units.per.year + PositionInYear(x))

}
