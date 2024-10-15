#' Get Ambiguous Events
#'
#' This function analyzes migration data to identify ambiguous senders and receivers
#' based on tenure event identifiers, as defined in Herman, A. (Forthcoming) "Reconstructing
#' bibliometric methods for studying mobility." It determines whether there are multiple unique
#' tenure IDs associated with each sender and receiver for a given migration event.
#'
#' @param migration_data A data.frame or data.table containing migration data with at least
#' `tenure_event_id`, `old_tenure_id`, and `new_tenure_id` columns.
#'
#' @return A data.table that includes the original migration data along with two new
#' logical columns: `ambiguous_senders` indicating if a tenure event has multiple senders,
#' and `ambiguous_receivers` indicating if a tenure event has multiple unique
#' receivers.
#'
#' @export

getAmbiguousEvents <- function(migration_data){
  temp_data <- copy(as.data.table(migration_data))
  temp_data[, 'ambiguous_senders' := length(unique(old_tenure_id)) > 1, by=tenure_event_id]
  temp_data[, 'ambiguous_receivers' := length(unique(new_tenure_id)) > 1, by=tenure_event_id]
  return(temp_data)
}
