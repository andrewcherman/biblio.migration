# biblio.migration
Infers mobility events from bibliometric data, implementing the tenure events method from Herman, A., (Forthcoming), "Reconstructing bibliometric methods for studying mobility." Classifies peoples' changes in affiliations as being migration or temporary migration, according to model parameters for backfilling missing affiliation data, tenure overlap between affiliations, and the duration of temporary positions. The package also identifies events that may have been mobility events, but which are ambiguous because of censoring or affiliation reporting.

## Framework
The tenure events method relies on converting paper-author-affiliation-time tuples into "tenures," uninterrupted periods wherein people were connected to a given affiliation, and using the patterns between those tenures to infer mobility. As described in Herman (Unpublished) ([Pre-print](#)), the method identifies migration, temporary migration, alongside several types of censoring and ambiguity, including multiple affiliations. The method is agnostic to the user's choice of units for geography and time.

The method itself comes with three main parameters which can be modified for easy sensitivity checking:

- The **backfill range**: the number of years (or some other unit of time) of missing data that ought to be filled in for each affiliation that a person has
- The **maximum overlap**: the maximum number of years (or some other unit of time) that two affiliations are allowed to overlap in order for the transition between them to be counted as a migration event
- The **maximum duration** of temporary positions: the maximum number of years (or some other unit of time) that temporary positions are allowed to exist for in order for them to be considered an instance of temporary migration

Iterating over specifications allows for easy robustness and sensitivity checking, in line with current norms around researcher degrees of freedom and multiversal analysis.

## Setup
The simplest way to install the package is to use install.github() from the devtools package.

```
install.packages('devtools')
install.github('andrewcherman/biblio.migration/biblio.migration')
```

## General workflow
Three functions make up the core workflow in implementing the tenure events method. They assume that users have data in the following 4-column format, though it is agnostic on what those columns are called.

*Table 1*

| paper_id | person_id | affiliation_id | time_id |
| -------- | --------- | -------------- | ------- |
| 10024    | 10381     | U of Bath      | 2016    |
| ...      | ...       | ...            | ...     |
| ...      | ...       | ...            | ...     |
| ...      | ...       | ...            | ...     |

*detectAffiliations()* transforms data of this type into simple links over time between people and affiliations. Users can specify the amount of backfilling (e.g. number of years) they would like to have done.

*Table 2*

| person_id | affiliation_id | time_id |
| --------- | -------------- | ------- |
| 10381     | U of Bath      | 2016    |
| ...       | ...            | ...     |
| ...       | ...            | ...     |
| ...       | ...            | ...     |

That accomplished, *getAffiliationTenures()* transforms the data into affiliation tenure data, providing estimates for how long a person was linked to each of the affiliations in their publication record. These data look as they do in the table below.

*Table 3*

| person_id | affiliation_id | est_start_year | est_end_year |
| --------  | ---------      | -------------- | -------      |
| 10381     | U of Bath      | 2014           |      2016    |
| ...      | ...       | ...            | ...     |
| ...      | ...       | ...            | ...     |
| ...      | ...       | ...            | ...     |

At this point users can call *getMobilityEvents()*, specifying their desired max overlap and max duration, in order to classify tenure events (moments where people start new tenures) as migration, temporary migration, or as suffering from various types of censoring and ambiguity, including multiple affiliations.

All together it may look something like the code block below. This assumes that the existence of a data.frame (or data.table) called *raw_data* that has a similar 4-column structure as appears in Table 1. Here I assume that *raw_data* stores its person IDs in a column called *researcher_id*, its units of time in *year*, and its affiliation data in *university_id*, and that we want to do 2 years of backfilling, that migration events can have overlapping tenures of up to 3 years, and that temporary positions can last up to 4 years. The code block below also assumes that we are interested in finding any mobility events that happened in the data between 2014 and 2023.

Note that this only processes a single specification for the tenure events method. Comparing the results across multiple specifications requires iterating over specifications.

```
my_person_var = 'researcher_id'
my_time_var = 'year'
my_geo_var = 'university_id'

estimated_affiliations <- detectAffiliations(
  df_publication_person_aff = raw_data,
  person_var = my_person_var,
  time_var = my_time_var,
  geo_var = my_geo_var,
  backfill_range = 2
  )
    
tenure_data <- getAffiliationTenures(
  aff_data = estimated_affiliations,
  person_var = my_person_var,
  time_var = my_time_var,
  geo_var = my_geo_var,
  )

mobility_events <- getMobilityEvents(
  tenure_data = tenure_data,
  person_var = my_person_var,
  geo_var = my_geo_var,
  min_year_of_interest = 2014,
  max_year_of_interest = 2023,
  allowable_duration_temporary_positions = 4,
  allowable_overlap_migration = 3
)
```

## Citation
Please use the following citation if you use *biblio.migration* in your work:

Herman, A., (Forthcoming), "Reconstructing bibliometric methods for studying mobility."

