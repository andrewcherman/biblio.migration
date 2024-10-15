#' Get Non-Mobile Tenures
#'
#' This function identifies non-mobile scientists based on their tenure data.
#' Non-mobile scientists are defined as those with only one unique start year
#' and one unique end year across their tenures. The function returns all
#' tenures associated with these scientists.
#'
#' @param tenure_data A data.frame or data.table containing tenure information with columns for
#' individual identifiers, geographical identifiers, start and end years of tenures.
#'
#' @return A data frame of tenures for non-mobile scientists, containing the following columns:
#' \describe{
#'   \item{person_id}{Identifier for the individual.}
#'   \item{geo_id}{Identifier for the geographical location.}
#'   \item{est_start_year}{The estimated start year of the tenure.}
#'   \item{est_end_year}{The estimated end year of the tenure.}
#' }
#'
#' @export


getNonMobileTenures <- function(tenure_data){
  temp_data <- copy(as.data.table(tenure_data))

  # Identify non-mobile scientists
  temp_data[
    , 'is_nonmobile' := length(unique(est_start_year)) == 1 & length(unique(est_end_year)) == 1,
    by=person_id
    ]

  # Return all tenures associated with those scientists
  return(temp_data[is_nonmobile == TRUE, .(person_id, geo_id, est_start_year, est_end_year)])
}

#' Get Non-Mobile People
#'
#' This function identifies non-mobile scientists from their tenure data.
#' Non-mobile scientists are defined as those who have only one unique start
#' year and one unique end year across their tenures. The function returns
#' a list of identifiers for these individuals.
#'
#' @param tenure_data A data.frame or data.table containing tenure information with columns for
#' individual identifiers, start years, and end years of tenures.
#'
#' @return A vector of identifiers for non-mobile scientists.
#'
#' @export


getNonMobilePeople <- function(tenure_data){
  temp_data <- copy(as.data.table(tenure_data))
  return(
    temp_data[
      ,
      length(unique(est_start_year)) == 1 & length(unique(est_end_year)) == 1,
      by=person_id
      ][
        V1 == TRUE,
        person_id
        ]
  )
}
