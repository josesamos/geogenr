% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acs_5yr_topic.R
\name{select_topic}
\alias{select_topic}
\alias{select_topic.acs_5yr_topic}
\title{Select topic (report group)}
\usage{
select_topic(act, topic)

\method{select_topic}{acs_5yr_topic}(act, topic = NULL)
}
\arguments{
\item{act}{An \code{acs_5yr_topic} object.}

\item{topic}{A string, topic name.}
}
\value{
An \code{acs_5yr_topic} object.
}
\description{
Select a topic. If no topic is given, the first one that appears in the area
is taken.
}
\examples{

dir <- tempdir()
source_dir <- system.file("extdata/acs_5yr", package = "geogenr")
files <- list.files(source_dir, "*.zip", full.names = TRUE)
file.copy(from = files, to = dir, overwrite = TRUE)
ac <- acs_5yr(dir)

files <- ac |>
  unzip_files()

act <- ac |>
  as_acs_5yr_topic("Alaska Native Regional Corporation",
                   2021,
                   "X01 Age And Sex")

act <- act |>
  select_topic(topic = "X03 Hispanic Or Latino Origin")

}
\seealso{
Other data selection functions: 
\code{\link{as_acs_5yr_topic}()},
\code{\link{get_available_area_topics}()},
\code{\link{get_available_area_years}()},
\code{\link{get_available_areas}()},
\code{\link{get_geo_attribute_names}()},
\code{\link{get_geo_layer.acs_5yr_geo}()},
\code{\link{get_names_of_other_topics}()},
\code{\link{get_report_names}()},
\code{\link{get_subreport_names}()},
\code{\link{get_topic_name}()},
\code{\link{select_report}()},
\code{\link{select_subreport}()}
}
\concept{data selection functions}
