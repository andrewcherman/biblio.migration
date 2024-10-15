#' Get Mobility Events
#'
#' The main function for the package, providing one wrapper function to identify all
#' forms of mobility that exist within a full dataset of researcher tenures. It implements
#' the tenure events framework. See Herman, A. (Forthcoming) "Reconstructing bibliometric
#' methods for studying mobility."
#'
#' Given a specific set of parameters, chosen by the user,
#' the function returns: all migration events, all temporary migration events, all
#' return migration events, all potential migrations that are ambiguous due
#' to having two or more affiliations start simultaneously, all tenures and periods of
#' multiple affiliation, as well as all events that cannot be unambiguously assigned to another
#' category due to censoring at the end of a person's publication career, or due to
#' censoring at the end of the observable dataset.
#'
#' Note that backfilling is handled by detectAffiliations() at an earlier point in the
#' standard workflow, after which one would run getAffiliationTenures(), before finally
#' running getMobilityEvents().
#'
#' @param tenure_data A data frame containing tenure information with columns for
#' individual identifiers, geographical identifiers, start and end years of tenures,
#' and additional related fields.
#'
#' @param person_var A string specifying the name of the column that contains
#' person IDs in the input data.
#'
#' @param geo_var A string specifying the name of the column that contains
#' geographical IDs in the input data.
#'
#' @param min_year_of_interest An integer representing the minimum year in which the user
#' is interested in identifying mobility events.
#'
#' @param max_year_of_interest An integer representing the maximum year in which the user
#' is interested in identifying mobility events.
#'
#' @param allowable_duration_temporary_positions An integer representing the
#' maximum duration that qualifies as a temporary position.
#'
#' @param allowable_overlap_migration An integer representing the maximum
#' allowable overlap to classify an event as a migration.
#'
#' @return A list containing:
#' \describe{
#'   \item{migration_events}{A data frame of migration events, containing columns for *person_id*
#'   (Person ID), *old_geo* (sending affiliation/geographical unit), *old_tenure_id* (tenure_id for
#'   the sending affiliation/geographical unit), *old_start_year* (start year or other unit of time
#'   for the tenure that is ending), *old_end_year* (end year or other unit of time for the tenure
#'   that is ending), *new_geo* (receiving affiliation/geographical unit), *new_tenure_id* (tenure_id
#'   for the receiving affiliation/geographical unit), *new_start_year* (start year or other unit
#'   of time for the tenure that is starting), *new_end_year* (end year or other unit of time for the
#'   tenure that is starting), *tenure_event_id* (ID for the tenure event), *migration* (indicator
#'   variable that equals 1 for all migration events), *migration_year* (the year or other unit of
#'   time when the migration event is taken to have occurred, defined as the start year for the new
#'   tenure), *return_migrant* (an indicator variable that equals TRUE when the migrant in question ever
#'   returns to their starting location), *ambiguous_senders* (an indicator variable that equals TRUE
#'   when there is more than one sending country), and *ambiguous_receivers* (an indicator variable
#'   that equals TRUE when there us more than one receiving country. Rows are events.)}
#'   \item{temporary_events}{A data frame of temporary migration events, containing columns for *person_id*
#'   (Person ID), *old_geo* (sending affiliation/geographical unit), *old_tenure_id* (tenure_id for
#'   the sending affiliation/geographical unit), *old_start_year* (start year or other unit of time
#'   for the tenure that is ending), *old_end_year* (end year or other unit of time for the tenure
#'   that is ending), *new_geo* (receiving affiliation/geographical unit), *new_tenure_id* (tenure_id
#'   for the receiving affiliation/geographical unit), *new_start_year* (start year or other unit
#'   of time for the tenure that is starting), *new_end_year* (end year or other unit of time for the
#'   tenure that is starting), *tenure_event_id* (ID for the tenure event), *temporary* (indicator
#'   variable that equals 1 for all temporary migration events), *start_temporary* (the start year for
#'   the temporary position), and *length_temporary* (the length of the temporary position in years, or
#'   whichever unit of time was supplied in the data by the user). Rows are events.}
#'   \item{return_events}{A data frame of return migration events, containing columns for *person_id*
#'   (Person ID), *original_old_geo* (the affiliation or geographical unit that the researcher was originally
#'   connected to, *departure_year* (the year the migrant left that affiliation or geographical unit), and
#'   *return_year* (the year the migrant first returned to that affiliation or geographical unit).
#'   Rows are events.}
#'   \item{ambiguous_simultaneous_starts}{A data frame of tenures that are part of events that are
#'   ambiguous due to having two or more tenures start simultaneously. It contains columns for *person_id*,
#'   (Person ID), *geo_id* (the affiliation or geographical unit that is associated with the tenure),
#'   *tenure_id* (the ID for the tenure in question), *est_start_year* (the start year for the tenure in
#'   question, *est_end_year* (the end year for the tenure in question), and *tenure_event_id* (the
#'   tenure event id for the event that is ambiguous). Rows are tenures.}
#'   \item{multiple_affiliation_tenures}{A data frame of tenures associated with multiple affiliations
#'   that cannot be associated with another classification. It contains columns for *person_id*,
#'   (Person ID), *geo_id* (the affiliation or geographical unit that is associated with the tenure),
#'   *tenure_id* (the ID for the tenure in question), *est_start_year* (the start year for the tenure in
#'   question, *est_end_year* (the end year for the tenure in question), and *tenure_event_id* (the
#'   tenure event id for the event that is associated with multiple affiliation). Rows are tenures.}
#'   \item{multiple_affiliation_periods}{A data frame of periods during which there were multiple affiliations
#'   that could not be resolved into one or another of the other classifications. It contains columns for
#'   *person_id* (Person ID), *min_affs* (the minimum number of affiliations that the focal person held
#'   during the period), *est_start_year* (the starting year for the period of multiple affiliations),
#'   *est_end_year* (the final year for the period of multiple affiliations), and *tenure_event_id* (the
#'   evend ID for the tenure event that started the period of multiple affiliations). Rows are person-periods.}
#'   \item{censored_career_end}{A data frame of tenures associated with potential mobility events
#'   that have been censored by the end of a person's publication record in the data. It contains
#'   columns for *person_id*, (Person ID), *geo_id* (the affiliation or geographical unit that is
#'   associated with the tenure), *tenure_id* (the ID for the tenure in question), *est_start_year*
#'   (the start year for the tenure in question, *est_end_year* (the end year for the tenure in question), *tenure_event_id* (the
#'   tenure event id for the event that is ambiguous), and *censored_event* (an indicator variable that is TRUE when the tenure
#'   in question represents a potentially censored mobility event). Rows are tenures.}
#'   \item{censored_last_obs}{A data frame of tenures associated with potential mobility events
#'   that have been censored by the end of the data's temporal coverage. It contains
#'   columns for *person_id*, (Person ID), *geo_id* (the affiliation or geographical unit that is
#'   associated with the tenure), *tenure_id* (the ID for the tenure in question), *est_start_year*
#'   (the start year for the tenure in question, *est_end_year* (the end year for the tenure in question), *tenure_event_id* (the
#'   tenure event id for the event that is ambiguous), and *censored_event* (an indicator variable that is TRUE when the tenure
#'   in question represents a potentially censored mobility event). Rows are tenures.}
#' }
#'
#' @export


getMobilityEvents <- function(
  tenure_data,
  person_var,
  geo_var,
  min_year_of_interest,
  max_year_of_interest,
  allowable_duration_temporary_positions,
  allowable_overlap_migration
  ){

  if(max_year_of_interest < min_year_of_interest){
    stop('*max_year_of_interest* must not be smaller than *min_year_of_interest*')
  }

  setnames(tenure_data, person_var, 'person_id')
  setnames(tenure_data, geo_var, 'geo_id')

  temp_censored <- getCensoredEvents(tenure_data, backfill_range = 2, final_t = max(tenure_data$est_end_year, na.rm=TRUE))
  censored_career_end <- temp_censored$censored_by_last_obs
  censored_last_obs <- temp_censored$censored_by_time_series

  year_sequence = min_year_of_interest:max_year_of_interest
  for(t in year_sequence){
    potential_events <- identifyPotentialMigrations(tenure_data, t)
    potential_events <- potential_events[new_start_year - old_end_year <= 1]

    if(t == year_sequence[1]){
      temporary_events <- classifyTemporaryEvents(
        potential_events,
        t,
        allowable_duration_temporary_positions
        )
      migration_events <- classifyMigrationEvents(
        potential_events,
        t,
        allowable_overlap_migration)
    } else {
      temporary_events <- rbind(
        temporary_events,
        classifyTemporaryEvents(
          potential_events,
          t,
          allowable_duration_temporary_positions
        )
      )
      migration_events <- rbind(
        migration_events,
        classifyMigrationEvents(
          potential_events,
          t,
          allowable_overlap_migration)
      )
    }
  }

  # The code sequence below for identifying return migrants was generated with the assistance of ChatGPT
  # Step 1: Create a cumulative list of all old_geos for each person_id
  setorder(migration_events, person_id, old_start_year)
  migration_events[
    ,
    `:=` (old_geo_list = lapply(.SD, unique)),
    by = .(person_id),
    .SDcols = "old_geo"
  ]

  return_migrants <- migration_events[
    is.na(new_start_year) == FALSE ,
    .(
      original_old_geo = first(old_geo),
      return_migrant = any(new_geo == first(old_geo) & new_start_year > min(new_start_year))
    ),
    by = .(person_id)
  ][return_migrant == TRUE]

  migration_events <- left_join(
    migration_events,
    return_migrants,
    by=c('person_id')
  )

  return_events <- migration_events[
    is.na(new_start_year) == FALSE &
    return_migrant == TRUE & original_old_geo == new_geo,
    list(
      'return_year' = min(new_start_year)
    ),
    by=.(person_id, original_old_geo)]

  return_events <- merge(
    migration_events[
      return_migrant == TRUE & original_old_geo == old_geo,
      list(
        'departure_year' = first(old_end_year)
      ),
      by=.(person_id, original_old_geo)],
    return_events,
    by=c('person_id', 'original_old_geo')
  )

  # Get multiple affiliations, removing all tenure events that were identified in any of the previous steps
  multiple_affiliation_tenures <- getMultipleAffiliations(
    tenure_data[
      !(
        tenure_event_id %in% migration_events$tenure_event_id |
        tenure_event_id %in% temporary_events$tenure_event_id |
        tenure_event_id %in% censored_career_end$tenure_event_id
      )
    ],
    min_year_of_interest,
    max_year_of_interest
    )

  # Get ambiguous migration events
  migration_events <- getAmbiguousEvents(migration_events)

  # Prepare migration events dataframe for output
  migration_events <- subset(migration_events, select=c(-old_geo_list, -original_old_geo))
  migration_events[is.na(return_migrant) == TRUE, return_migrant := FALSE]

  return(
    list(
      'migration_events' = subset(migration_events, select=c(-career_start, -career_end)),
      'temporary_events' = subset(temporary_events, select=c(-career_start, -career_end)),
      'return_events' = return_events,
      'ambiguous_simultaneous_starts' = subset(multiple_affiliation_tenures$ambiguous_simultaneous_starts, select=c(-backfilled)),
      'multiple_affiliation_tenures' = subset(
        multiple_affiliation_tenures$multi_tenures,
        select=c(-backfilled, -n_at_start_year, -n_endings, -only_one, -in_focal)
        ),
      'multiple_affiliation_periods' = multiple_affiliation_tenures$multi_tenures_periods,
      'censored_career_end' = subset(censored_career_end, select=c(-backfilled)),
      'censored_last_obs' = subset(censored_last_obs, select=c(-backfilled))
      )
  )
}
