#' Get Multiple Affiliations
#'
#' This function identifies individuals with multiple affiliations within a specified
#' range of years based on their tenure data. It checks for ambiguous simultaneous
#' starts (potentially censored migrants) and determines periods of multiple affiliations.
#' See Herman, A. (Forthcoming) "Reconstructing bibliometric methods for studying mobility"
#' for more information.
#'
#' @param tenure_data A data.frame or data.table containing tenure information with columns for
#' individual identifiers, geographical identifiers, start and end years of tenures,
#' and additional related fields.
#'
#' @param min_year_of_interest An integer representing the minimum year to
#' consider for identifying multiple affiliations.
#'
#' @param max_year_of_interest An integer representing the maximum year to
#' consider for identifying multiple affiliations.
#'
#' @return A list containing:
#' \describe{
#'   \item{ambiguous_simultaneous_starts}{A data frame of ambiguous simultaneous
#'   starts in affiliations.}
#'   \item{multi_tenures}{A data frame of tenure data for individuals with multiple
#'   affiliations.}
#'   \item{multi_tenures_periods}{A data frame of periods when individuals had
#'   multiple affiliations.}
#' }
#'
#' @export


getMultipleAffiliations <- function(tenure_data, min_year_of_interest, max_year_of_interest){
  temp_data <- copy(as.data.table(tenure_data))

  # Get ambiguous because the event involves two affils starting at the same time, but which do not end at the same time
  temp_data[, 'n_at_start_year' := .N, by=.(tenure_event_id, est_start_year)]
  temp_data[, 'n_endings' := length(unique(est_end_year)), by=tenure_event_id]
  ambiguous_simultaneous_starts <- temp_data[n_at_start_year > 1 & n_endings > 1]
  ambiguous_simultaneous_starts[, 'n_at_start_year' := NULL]
  ambiguous_simultaneous_starts[, 'n_endings' := NULL]
  ambiguous_simultaneous_starts

  # Removing ambiguous simultaneous starts creates a situation where stray affiliation tenures survive in the data.table
  # because its partners have been removed. As a result, they need to be identified and removed
  temp_data <- temp_data[!(tenure_event_id %in% ambiguous_simultaneous_starts$tenure_event_id)]
  temp_data[, 'only_one' := .N == 1, by=person_id]
  temp_data <- temp_data[only_one == FALSE]
  temp_data

  getSequences <- function(year_sequence){

    if(!is.numeric(year_sequence)){
      year_sequence <- as.numeric(year_sequence)
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

  for(t in min_year_of_interest:max_year_of_interest){
    temp_data[, 'in_focal' := (t >= est_start_year & t <= est_end_year)]

    if(t != (min_year_of_interest:max_year_of_interest)[1]){
      multi_count <- rbind(
        multi_count,
        temp_data[in_focal==TRUE, list('N' = .N, 'focal_year' = t), by=person_id]
      )
    } else {
      multi_count <- temp_data[in_focal==TRUE, list('N' = .N, 'focal_year' = t), by=person_id]
    }
  }

  multi_count[N > 1, twomore_id := getSequences(focal_year), by=person_id]
  multi_count[N > 2, threemore_id := getSequences(focal_year), by=person_id]
  multi_count[N > 3, fourmore_id := getSequences(focal_year), by=person_id]

  multi_tenures <- rbind(
    multi_count[
      is.na(twomore_id) == FALSE,
      list(
        'est_start_year' = min(focal_year),
        'est_end_year' = max(focal_year),
        'min_affs' = 2
      ),
      by=c('person_id', 'twomore_id')
    ][, .(person_id, min_affs, est_start_year, est_end_year)],
    multi_count[
      is.na(threemore_id) == FALSE,
      list(
        'est_start_year' = min(focal_year),
        'est_end_year' = max(focal_year),
        'min_affs' = 3
      ),
      by=c('person_id', 'threemore_id')
    ][, .(person_id, min_affs, est_start_year, est_end_year)],
    multi_count[
      is.na(fourmore_id) == FALSE,
      list(
        'est_start_year' = min(focal_year),
        'est_end_year' = max(focal_year),
        'min_affs' = 4
      ),
      by=c('person_id', 'threemore_id')
    ][, .(person_id, min_affs, est_start_year, est_end_year)]
  )

  # Add in tenure_event_id
  multi_tenures <- merge(multi_tenures, tenure_data[, .(person_id, est_start_year, tenure_event_id)], by=c('person_id', 'est_start_year'))
  multi_tenures <- multi_tenures[!duplicated(multi_tenures)]
  return(
    list(
      'ambiguous_simultaneous_starts' = ambiguous_simultaneous_starts,
      'multi_tenures' = temp_data,
      'multi_tenures_periods' = multi_tenures
    )
  )
}
