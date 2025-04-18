% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_listing_to_report.R
\name{add_listing_to_report}
\alias{add_listing_to_report}
\title{Add each listing to the initialized report}
\usage{
add_listing_to_report(deletion_report, listing, ...)
}
\arguments{
\item{deletion_report}{Name of initialized deletion report made with \code{\link[=build_report]{build_report()}}}

\item{listing}{Data to be used in the listing. Subset to variables that best portray why the data were deleted.
Selected variables should be source variables if possible}

\item{...}{additional arguments passed to \code{\link[reporter:create_table]{reporter::create_table()}}}
}
\value{
A deletion report object that includes all added listings and their titles
}
\description{
\code{add_listing_to_report()}
is used to build listings one deletion flag at a time
}
\examples{
\dontrun{

del_report <- del_report \%>\%
 add_listing_to_report(
   listing_table(
     pk_delimp_mod  \%>\%
       filter(DELFN == 5, DVID == 2) \%>\%
       arrange(STUDYID, USUBJID, DTTM) \%>\%
       select(ROW,USUBJID, VISIT, PCTPT, PCTESTCD, PCSTAT),
     "Compound A (ng/mL), Sample Not Done / Not Recorded"
   ) \%>\%
 add_listing_to_report(
   listing_table(
     pk_delimp_mod \%>\%
       filter(DELFN == 5, DVID == 3) \%>\%
       arrange(STUDYID, USUBJID, DTTM) \%>\%
       select(ROW,USUBJID, VISIT, PCTPT, PCTESTCD, PCSTAT),
     "Compound B (ng/mL), Sample Not Done / Not Recorded"
   ))

}




}
