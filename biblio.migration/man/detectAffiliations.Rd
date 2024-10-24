% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detectAffiliations.R
\name{detectAffiliations}
\alias{detectAffiliations}
\title{Detect Affiliations}
\usage{
detectAffiliations(
  df_publication_person_aff,
  person_var,
  time_var,
  geo_var,
  backfill_range
)
}
\arguments{
\item{df_publication_person_aff}{A data.frame or data.table containing publication
data with rows for papers and columns for persons, time, and affiliation/geography.}

\item{person_var}{A string specifying the name of the column representing
person IDs in \code{df_publication_person_aff}.}

\item{time_var}{A string specifying the name of the column representing
time IDs in \code{df_publication_person_aff}.}

\item{geo_var}{A string specifying the name of the column representing
geographical IDs in \code{df_publication_person_aff}.}

\item{backfill_range}{An integer specifying the range of years to backfill
for each individual's missing data.}
}
\value{
A data.table with columns for person ID, time ID, affiliation/geography ID,
career start year, and a boolean indicating whether the row of data was created
through backfilling.
}
\description{
This function identifies affiliations of individuals based on their publication
data, creating a record of their geographical and temporal associations. It
also backfills missing data (reporting gaps) at the affiliation level.
}
\details{
The function takes a data frame of publication-affiliation data and generates
new records for years that are missing, based on the specified backfill range.
It ensures that data is not duplicated in the backfilling process.

Backfilling is defined here as it is in Herman, A. (Forthcoming) "Reconstructing bibliometric
methods for studying mobility." That is, when there are gaps in someone's publication
record with respect to a specific affiliation (i.e. they published in 2007 and 2010
with that University of Alabama, but not in 2008 or 2009), backfilling creates additional records
to replace the missing data. Assuming years as the unit of time, with a \emph{backfill range}
of 1, our scientist would be given an additional row showing that they were affiliated
with the University of Alabama in 2009 as well. With a \emph{backfill range} of 2 they would
also be given a row indicated the affiliation for 2008.

Note that this function never backfills entries before the first appearance of an affiliation
in a person's publication history.
}
