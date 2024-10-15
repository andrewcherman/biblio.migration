#' Identify Potential Migrations
#'
#' This function identifies individuals who may have migrated based on their
#' affiliations in a specified focal year. It looks for individuals who have a new
#' position in the focal year while having at least one pre-existing affiliation.
#'
#' @param tenure_data A data frame containing tenure information with columns
#' `person_id`, `est_start_year`, and `est_end_year`, among others.
#'
#' @param focal_year An integer representing the year in which new affiliations
#' are to be assessed for potential migration.
#'
#' @return A data.table containing potential migration events, including details
#' about both pre-existing and new affiliations, as well as the career start and end
#' for each individual identified as a potential mover.
#'
#' @export


identifyPotentialMigrations <- function(tenure_data, focal_year){
  temp_data <- as.data.table(copy(tenure_data))

  # Find people who have a new position in the focal year, but who also
  # have at least one pre-existing affiliation
  potential_movers <- temp_data[
    person_id %in% temp_data[est_start_year == focal_year, person_id] &
      est_start_year < focal_year,
    unique(person_id)
  ]

  # Get career start and end for each author
  start_and_end <- temp_data[
    person_id %in% potential_movers &
      is.na(est_start_year) == FALSE &
      is.na(est_end_year) == FALSE,
    list(
      'career_start' = min(est_start_year, na.rm=TRUE),
      'career_end' = max(est_end_year, na.rm=TRUE)
    ),
    by=person_id
  ]

  # Get pre-existing affiliations, new affiliations
  preexisting_affs <- temp_data[
    person_id %in% potential_movers & est_start_year < focal_year,
    .(person_id, geo_id, tenure_id, est_start_year, est_end_year)
    ]
  setnames(preexisting_affs, 'tenure_id', 'old_tenure_id')
  setnames(preexisting_affs, 'geo_id', 'old_geo')
  setnames(preexisting_affs, 'est_start_year', 'old_start_year')
  setnames(preexisting_affs, 'est_end_year', 'old_end_year')

  new_affs <- temp_data[
    person_id %in% potential_movers & est_start_year == focal_year,
    .(person_id, geo_id, tenure_id, tenure_event_id, est_start_year, est_end_year)
    ]
  setnames(new_affs, 'tenure_id', 'new_tenure_id')
  setnames(new_affs, 'geo_id', 'new_geo')
  setnames(new_affs, 'est_start_year', 'new_start_year')
  setnames(new_affs, 'est_end_year', 'new_end_year')

  # Merge pre-existing and new affiliation data to create the core data for inferring migration
  potential_events <- merge(preexisting_affs, new_affs, by='person_id')
  potential_events <- merge(potential_events, start_and_end, by='person_id')

  return(
    potential_events
  )
}

#' Classify Migration Events
#'
#' This function classifies migration events based on the presence of
#' ruptures between new and old affiliations. A rupture is defined
#' by the overlap of years, while a gap is defined by the time between
#' old and new affiliations. For more details see Herman, A. (Forthcoming)
#' "Reconstructing bibliometric methods for studying mobility."
#'
#' This function is meant for internal use, as it estimates migration events
#' for a single focal year.
#'
#' @param temp_data A data.frame or data.table containing potential migration events, including
#' columns such as `new_start_year`, `new_end_year`, `old_start_year`,
#' `old_end_year`, `old_geo`, and `new_geo`.
#'
#' @param focal_year An integer representing the year in which migration
#' classification is to be assessed.
#'
#' @param allowable_overlap An integer representing the maximum allowable
#' overlap between the new and old end years for a migration to be classified
#' as a rupture.
#'
#' @return A data.table containing classified migration events, with an indicator
#' for migration and the year of migration.
#'
#' @export


classifyMigrationEvents <- function(temp_data, focal_year, allowable_overlap){
  potential_events <- copy(temp_data)

  ruptures <- potential_events[
    ,
    list(
      'rupture' = all(
          (
            # Check if there is a rupture as defined by the overlap
            new_start_year <= old_end_year &
            new_end_year > old_end_year &
            old_end_year - new_start_year <= allowable_overlap + 1
          )
        )
      ),
    by=person_id
    ][
      rupture == TRUE, person_id
      ]

  potential_events[, 'migration' := ifelse(person_id %in% ruptures, 1, 0)]
  potential_events[migration == 1, 'migration_year' := focal_year]

  # Remove rows created by "gaps" where people appear to migrate to and from the exact same region
  potential_events <- potential_events[old_geo != new_geo]

  return(
    potential_events[migration==1]
  )
}

#' Classify Temporary Events
#'
#' This function classifies temporary events based on the relationship
#' between new and old affiliations, as defined in Herman, A. (Forthcoming)
#' "Reconstructing bibliometric methods for studying mobility.". A temporary event is
#' defined as a situation where the new affiliation ends before the old
#' affiliation ends.
#'
#' @param temp_data A data.frame or data.table containing potential temporary events, including
#' columns such as `new_start_year`, `new_end_year`, and `old_end_year`.
#'
#' @param focal_year An integer representing the year in which temporary
#' event classification is to be assessed.
#'
#' @param allowable_temporary_overlap An integer representing the maximum
#' allowable duration for a temporary event to be classified as such.
#'
#' @return A data frame containing classified temporary events, with an indicator
#' for temporary status, the start year of the temporary event, and its length.
#'
#' @export


classifyTemporaryEvents <- function(temp_data, focal_year, allowable_temporary_overlap){
  potential_events <- as.data.table(copy(temp_data))

  potential_events[, 'temporary' := 0]
  potential_events[
    ,
    temporary := ifelse(
      all(new_end_year < old_end_year),
      1,
      0
      ),
    by=person_id
    ]
  potential_events[temporary == 1, 'start_temporary' := focal_year]
  potential_events[temporary == 1, 'length_temporary' := new_end_year - new_start_year]
  potential_events[length_temporary > allowable_temporary_overlap, temporary := 0]

  return(potential_events[temporary == 1])
}



