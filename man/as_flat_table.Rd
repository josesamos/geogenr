% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acs_5yr_rolap.R
\name{as_flat_table}
\alias{as_flat_table}
\alias{as_flat_table.acs_5yr_topic}
\title{As \code{rolap::flat_table} object}
\usage{
as_flat_table(act, attributes)

\method{as_flat_table}{acs_5yr_topic}(act, attributes = NULL)
}
\arguments{
\item{act}{An \code{acs_5yr_topic} object.}

\item{attributes}{A string vector.}
}
\value{
A \code{flat_table} object.
}
\description{
Obtain an \code{rolap::flat_table} object to be able to modify the data or integrate
it with other data.
}
\details{
We can indicate the attributes of the geographic layer to include in the export.
Otherwise, the default attributes are included (not area, perimeter or location
attributes).
}
\examples{

ft <- anrc_2021_x01 |>
  as_flat_table()

}
\seealso{
Other data exploitation and export functions: 
\code{\link{as_GeoPackage}()},
\code{\link{as_acs_5yr_geo}()},
\code{\link{as_geomultistar}()},
\code{\link{as_star_database}()},
\code{\link{get_metadata}()},
\code{\link{set_metadata}()}
}
\concept{data exploitation and export functions}
