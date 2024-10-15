#' Get Affiliation Tenures
#'
#' This function processes affiliation data to extract tenure information for individuals,
#' identifying periods of association with affiliations or geographical locations and creating
#' unique event IDs.
#'
#' The function organizes the data by person and geography, identifying the start and end
#' years of affiliations. It does not return tenures for people with only one affiliation over time.
#'
#' Unique event IDs are created according to the start date of tenures, the only point at which
#' there can be tenure events. The event ID is a combination of the start date of tenures, as well
#' as the tenure IDs of any tenures that started at that point in time.
#'
#' @param aff_data A data.frame or data.table containing affiliation data with columns for
#' individual identifiers, time indicators, geographical identifiers, and a backfilled status.
#' @param person_var A string specifying the name of the column representing
#' individual identifiers in `aff_data`.
#' @param time_var A string specifying the name of the column representing
#' time indicators in `aff_data`.
#' @param geo_var A string specifying the name of the column representing
#' geographical identifiers in `aff_data`.
#'
#' @return A data.table with columns for person ID, affiliation or geography ID,
#' estimated start and end years of tenures, and a boolean indicating whether the
#' tenure estimate relied on data that was backfilled.
#'
#' @export

getAffiliationTenures <- function(
    aff_data,
    person_var,
    time_var,
    geo_var
    ){

  # Adapted from https://stackoverflow.com/questions/7077710/sequence-length-encoding-using-r
  getSequences <- function(year_sequence){

    if(!is.numeric(year_sequence)){
      year_sequence <- unique(as.numeric(year_sequence))
    }
    n <- length(year_sequence)
    boolean_sequence <- year_sequence[-1L] != year_sequence[-n] + 1L
    switches <- c(which(boolean_sequence|is.na(boolean_sequence)),n)
    switch_lengths <- diff(c(0L,switches))

    counter <- 1
    sequence_vector <- vector()
    for(switch_length in switch_lengths){
      sequence_vector <- append(sequence_vector, rep(counter, times=switch_length))
      counter <- counter + 1
    }

    return(sequence_vector)
  }

  createEventID <- function(tenure_data){
    tenure_data[, 'new_tenures' := paste(unique(tenure_id), collapse='/'), by=.(person_id, est_start_year)]
    tenure_data[, 'tenure_event_id' := paste0(person_id, '_', new_tenures, '_', est_start_year)]
    tenure_data[, new_tenures := NULL]
    return(tenure_data)
  }

  temp_data <- copy(as.data.table(aff_data))
  setnames(temp_data, person_var, 'person_id')
  setnames(temp_data, time_var, 'time_id')
  setnames(temp_data, geo_var, 'geo_id')
  temp_data <- temp_data[, .(person_id, time_id, geo_id, backfilled)]
  temp_data <- temp_data[!duplicated(temp_data)]

  temp_data <- temp_data[
    person_id %in%
      temp_data[, .N, by=c('person_id', 'geo_id')][, .N, by='person_id'][N > 1, person_id]
    ]

  setorder(temp_data, person_id, geo_id, time_id)

  temp_data[, tenure_id := getSequences(time_id), by=c('person_id', 'geo_id')]
  temp_data[, tenure_id := paste(geo_id, tenure_id, sep='.')]

  temp_data <- temp_data[
    ,
    list(
      'est_start_year' = min(time_id),
      'est_end_year' = max(time_id),
      'backfilled' = any(backfilled == TRUE)
    ),
    by=c('person_id', 'geo_id', 'tenure_id')
  ]

  temp_data <- createEventID(temp_data)
  setnames(temp_data, 'person_id', person_var)
  setnames(temp_data, 'geo_id', geo_var)
  return(temp_data)
}
