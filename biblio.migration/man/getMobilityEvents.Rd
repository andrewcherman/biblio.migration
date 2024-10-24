% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMobilityEvents.R
\name{getMobilityEvents}
\alias{getMobilityEvents}
\title{Get Mobility Events}
\usage{
getMobilityEvents(
  tenure_data,
  person_var,
  geo_var,
  min_year_of_interest,
  max_year_of_interest,
  allowable_duration_temporary_positions,
  allowable_overlap_migration
)
}
\arguments{
\item{tenure_data}{A data frame containing tenure information with columns for
individual identifiers, geographical identifiers, start and end years of tenures,
and additional related fields.}

\item{person_var}{A string specifying the name of the column that contains
person IDs in the input data.}

\item{geo_var}{A string specifying the name of the column that contains
geographical IDs in the input data.}

\item{min_year_of_interest}{An integer representing the minimum year in which the user
is interested in identifying mobility events.}

\item{max_year_of_interest}{An integer representing the maximum year in which the user
is interested in identifying mobility events.}

\item{allowable_duration_temporary_positions}{An integer representing the
maximum duration that qualifies as a temporary position.}

\item{allowable_overlap_migration}{An integer representing the maximum
allowable overlap to classify an event as a migration.}
}
\value{
A list containing:
\describe{
\item{migration_events}{A data frame of migration events, containing columns for \emph{person_id}
(Person ID), \emph{old_geo} (sending affiliation/geographical unit), \emph{old_tenure_id} (tenure_id for
the sending affiliation/geographical unit), \emph{old_start_year} (start year or other unit of time
for the tenure that is ending), \emph{old_end_year} (end year or other unit of time for the tenure
that is ending), \emph{new_geo} (receiving affiliation/geographical unit), \emph{new_tenure_id} (tenure_id
for the receiving affiliation/geographical unit), \emph{new_start_year} (start year or other unit
of time for the tenure that is starting), \emph{new_end_year} (end year or other unit of time for the
tenure that is starting), \emph{tenure_event_id} (ID for the tenure event), \emph{migration} (indicator
variable that equals 1 for all migration events), \emph{migration_year} (the year or other unit of
time when the migration event is taken to have occurred, defined as the start year for the new
tenure), \emph{return_migrant} (an indicator variable that equals TRUE when the migrant in question ever
returns to their starting location), \emph{ambiguous_senders} (an indicator variable that equals TRUE
when there is more than one sending country), and \emph{ambiguous_receivers} (an indicator variable
that equals TRUE when there us more than one receiving country. Rows are events.)}
\item{temporary_events}{A data frame of temporary migration events, containing columns for \emph{person_id}
(Person ID), \emph{old_geo} (sending affiliation/geographical unit), \emph{old_tenure_id} (tenure_id for
the sending affiliation/geographical unit), \emph{old_start_year} (start year or other unit of time
for the tenure that is ending), \emph{old_end_year} (end year or other unit of time for the tenure
that is ending), \emph{new_geo} (receiving affiliation/geographical unit), \emph{new_tenure_id} (tenure_id
for the receiving affiliation/geographical unit), \emph{new_start_year} (start year or other unit
of time for the tenure that is starting), \emph{new_end_year} (end year or other unit of time for the
tenure that is starting), \emph{tenure_event_id} (ID for the tenure event), \emph{temporary} (indicator
variable that equals 1 for all temporary migration events), \emph{start_temporary} (the start year for
the temporary position), and \emph{length_temporary} (the length of the temporary position in years, or
whichever unit of time was supplied in the data by the user). Rows are events.}
\item{return_events}{A data frame of return migration events, containing columns for \emph{person_id}
(Person ID), \emph{original_old_geo} (the affiliation or geographical unit that the researcher was originally
connected to, \emph{departure_year} (the year the migrant left that affiliation or geographical unit), and
\emph{return_year} (the year the migrant first returned to that affiliation or geographical unit).
Rows are events.}
\item{ambiguous_simultaneous_starts}{A data frame of tenures that are part of events that are
ambiguous due to having two or more tenures start simultaneously. It contains columns for \emph{person_id},
(Person ID), \emph{geo_id} (the affiliation or geographical unit that is associated with the tenure),
\emph{tenure_id} (the ID for the tenure in question), \emph{est_start_year} (the start year for the tenure in
question, \emph{est_end_year} (the end year for the tenure in question), and \emph{tenure_event_id} (the
tenure event id for the event that is ambiguous). Rows are tenures.}
\item{multiple_affiliation_tenures}{A data frame of tenures associated with multiple affiliations
that cannot be associated with another classification. It contains columns for \emph{person_id},
(Person ID), \emph{geo_id} (the affiliation or geographical unit that is associated with the tenure),
\emph{tenure_id} (the ID for the tenure in question), \emph{est_start_year} (the start year for the tenure in
question, \emph{est_end_year} (the end year for the tenure in question), and \emph{tenure_event_id} (the
tenure event id for the event that is associated with multiple affiliation). Rows are tenures.}
\item{multiple_affiliation_periods}{A data frame of periods during which there were multiple affiliations
that could not be resolved into one or another of the other classifications. It contains columns for
\emph{person_id} (Person ID), \emph{min_affs} (the minimum number of affiliations that the focal person held
during the period), \emph{est_start_year} (the starting year for the period of multiple affiliations),
\emph{est_end_year} (the final year for the period of multiple affiliations), and \emph{tenure_event_id} (the
evend ID for the tenure event that started the period of multiple affiliations). Rows are person-periods.}
\item{censored_career_end}{A data frame of tenures associated with potential mobility events
that have been censored by the end of a person's publication record in the data. It contains
columns for \emph{person_id}, (Person ID), \emph{geo_id} (the affiliation or geographical unit that is
associated with the tenure), \emph{tenure_id} (the ID for the tenure in question), \emph{est_start_year}
(the start year for the tenure in question, \emph{est_end_year} (the end year for the tenure in question), \emph{tenure_event_id} (the
tenure event id for the event that is ambiguous), and \emph{censored_event} (an indicator variable that is TRUE when the tenure
in question represents a potentially censored mobility event). Rows are tenures.}
\item{censored_last_obs}{A data frame of tenures associated with potential mobility events
that have been censored by the end of the data's temporal coverage. It contains
columns for \emph{person_id}, (Person ID), \emph{geo_id} (the affiliation or geographical unit that is
associated with the tenure), \emph{tenure_id} (the ID for the tenure in question), \emph{est_start_year}
(the start year for the tenure in question, \emph{est_end_year} (the end year for the tenure in question), \emph{tenure_event_id} (the
tenure event id for the event that is ambiguous), and \emph{censored_event} (an indicator variable that is TRUE when the tenure
in question represents a potentially censored mobility event). Rows are tenures.}
}
}
\description{
The main function for the package, providing one wrapper function to identify all
forms of mobility that exist within a full dataset of researcher tenures. It implements
the tenure events framework. See Herman, A. (Forthcoming) "Reconstructing bibliometric
methods for studying mobility."
}
\details{
Given a specific set of parameters, chosen by the user,
the function returns: all migration events, all temporary migration events, all
return migration events, all potential migrations that are ambiguous due
to having two or more affiliations start simultaneously, all tenures and periods of
multiple affiliation, as well as all events that cannot be unambiguously assigned to another
category due to censoring at the end of a person's publication career, or due to
censoring at the end of the observable dataset.

Note that backfilling is handled by detectAffiliations() at an earlier point in the
standard workflow, after which one would run getAffiliationTenures(), before finally
running getMobilityEvents().
}
