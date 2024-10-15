#' Get Censored Events
#'
#' This function identifies censored events in tenure data based on specified criteria,
#' as defined in Herman, A. (Forthcoming) "Reconstructing bibliometric methods for studying mobility."
#' It returns two event subsets: those censored by the last observation of a person's publication career
#' and those censored by the final observation in the overall time series.
#'
#' @param tenure_data A data.frame or data.table containing tenure information with columns `person_id`,
#' `est_start_year`, `est_end_year`, and `backfilled`.
#'
#' @param backfill_range An integer indicating the backfill range. This is needed to determine the final
#' observation year.
#'
#' @param final_t An integer representing the final year to assess for ongoing affiliations.
#'
#' @return A list containing two data.tables:
#' \item{censored_by_last_obs}{A data.table of events that were censored based on the last observation.}
#' \item{censored_by_time_series}{A data.table of events that were censored based on the time series, specifically those
#' that have their `est_end_year` equal to `final_t`.}
#'
#' @export

getCensoredEvents <- function(
  tenure_data,
  backfill_range,
  final_t
  ){

  raw <- as.data.table(copy(tenure_data))
  raw[, 'career_start' := min(est_start_year), by=person_id]
  raw[, 'career_end' := max(est_end_year), by=person_id]
  raw[
    ,
    'has_new_aff_final_year' := any(
      est_start_year == career_end |
        (est_start_year + backfill_range == career_end & backfilled == TRUE)
      ),
    by=person_id
    ]
  raw[
    ,
    'has_cont_aff_final_year' := any(
      est_end_year == career_end &
        est_start_year < career_end
      ),
    by=person_id
    ]
  raw[
    (
      est_start_year == career_end |
      (est_start_year + backfill_range == career_end & backfilled == TRUE)
    ) | (
      est_end_year == career_end &
      est_start_year < career_end
    ),
    'n_relevant_end' := .N,
    by=person_id]
  censored_by_last_obs <- raw[
    has_new_aff_final_year == TRUE &
      has_cont_aff_final_year == TRUE &
      is.na(n_relevant_end) == FALSE
    ]
  censored_by_last_obs[, 'censored_event' := FALSE]
  censored_by_last_obs[, 'is_earliest' := est_start_year == min(est_start_year), by=person_id]
  censored_by_last_obs[is_earliest != TRUE, censored_event := TRUE]
  censored_by_last_obs[, is_earliest := NULL]

  censored_by_time_series <- censored_by_last_obs[
    est_end_year == final_t
  ]

  return(
    list(
        'censored_by_last_obs' = subset(
          censored_by_last_obs,
          select=c(
            -career_start,
            -career_end,
            -has_new_aff_final_year,
            -has_cont_aff_final_year,
            -n_relevant_end
            )
          ),
        'censored_by_time_series' = subset(
          censored_by_time_series,
          select=c(
            -career_start,
            -career_end,
            -has_new_aff_final_year,
            -has_cont_aff_final_year,
            -n_relevant_end
          )
        )
      )
  )
}
