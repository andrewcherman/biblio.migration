#' Detect Affiliations
#'
#' This function identifies affiliations of individuals based on their publication
#' data, creating a record of their geographical and temporal associations. It
#' also backfills missing data (reporting gaps) at the affiliation level.
#'
#' The function takes a data frame of publication-affiliation data and generates
#' new records for years that are missing, based on the specified backfill range.
#' It ensures that data is not duplicated in the backfilling process.
#'
#' Backfilling is defined here as it is in Herman, A. (Forthcoming) "Reconstructing bibliometric
#' methods for studying mobility." That is, when there are gaps in someone's publication
#' record with respect to a specific affiliation (i.e. they published in 2007 and 2010
#' with that University of Alabama, but not in 2008 or 2009), backfilling creates additional records
#' to replace the missing data. Assuming years as the unit of time, with a *backfill range*
#' of 1, our scientist would be given an additional row showing that they were affiliated
#' with the University of Alabama in 2009 as well. With a *backfill range* of 2 they would
#' also be given a row indicated the affiliation for 2008.
#'
#' Note that this function never backfills entries before the first appearance of an affiliation
#' in a person's publication history.
#'
#' @param df_publication_person_aff A data.frame or data.table containing publication
#' data with rows for papers and columns for persons, time, and affiliation/geography.
#' @param person_var A string specifying the name of the column representing
#' person IDs in `df_publication_person_aff`.
#' @param time_var A string specifying the name of the column representing
#' time IDs in `df_publication_person_aff`.
#' @param geo_var A string specifying the name of the column representing
#' geographical IDs in `df_publication_person_aff`.
#' @param backfill_range An integer specifying the range of years to backfill
#' for each individual's missing data.
#'
#' @return A data.table with columns for person ID, time ID, affiliation/geography ID,
#' career start year, and a boolean indicating whether the row of data was created
#' through backfilling.
#'
#' @export


detectAffiliations <- function(
    df_publication_person_aff,
    person_var,
    time_var,
    geo_var,
    backfill_range
){

  df_publication_person_aff <- as.data.table(df_publication_person_aff)

  # Save old column names for later recovery
  old_person <- person_var
  old_time <- time_var
  old_geo <- geo_var

  min_year <- df_publication_person_aff[, min(eval(parse(text=time_var)), na.rm=TRUE)]
  max_year <- df_publication_person_aff[, max(eval(parse(text=time_var)), na.rm=TRUE)]
  relevant_years <- seq(from=min_year, to=max_year)
  temp_data <- subset(
    df_publication_person_aff[
      ,
      .N,
      by=c(person_var, geo_var, time_var)],
    select=-N,
  )
  setnames(temp_data, person_var, 'person_id')
  setnames(temp_data, time_var, 'time_id')
  setnames(temp_data, geo_var, 'geo_id')

  temp_data[, 'career_start' := min(time_id), by=person_id]
  temp_data[, 'backfilled' := FALSE]

  for(relevant_year in relevant_years){
    # temp_data has no entry for any author/geo/time tuple that is an NA
    # Exclude entries that happen in the first year of a person's career
    # Look for author/geo pairs that fall within the backfill range, but with no entry for the focal year
    iterated_data <- temp_data[
      time_id %in% relevant_year:(relevant_year + backfill_range) &
      career_start < relevant_year
      ]
    iterated_data[, 'needs_backfill' := all(time_id != relevant_year), by=.(person_id, geo_id)]
    iterated_data <- iterated_data[needs_backfill == TRUE]

    # Generate new backfilled rows to rbind with the original data
    end_data <- iterated_data[rep(seq_len(nrow(iterated_data)), backfill_range), .(person_id, geo_id, career_start)]
    end_data[, 'time_id' := rep(relevant_year:(relevant_year + backfill_range - 1), each=nrow(iterated_data))]
    end_data[, 'backfilled' := TRUE]

    temp_data <- rbind(temp_data, end_data)
  }

  # This process can create duplicated rows by backfilling the same missing year more than once
  temp_data <- temp_data[!duplicated(temp_data)]

  # And then it can also create duplicates by "backfilling" a year that already had data
  temp_data[, 'more_than_one' := .N > 1, by=.(person_id, geo_id, time_id, career_start)]
  temp_data[more_than_one == TRUE, backfilled := FALSE]
  temp_data <- temp_data[!duplicated(temp_data)]
  temp_data[, more_than_one := NULL]

  # Return column names to original
  setnames(temp_data, 'person_id', person_var)
  setnames(temp_data, 'time_id', time_var)
  setnames(temp_data, 'geo_id', geo_var)

  return(temp_data)
}
