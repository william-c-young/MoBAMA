% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breadth_scores.r
\name{breadth_scores}
\alias{breadth_scores}
\title{Gets fab and FcR breadth scores per subject from
a MoBAMAResult object}
\usage{
breadth_scores(result, tps = unique(result$data$tp), agClasses = NULL,
  reClasses = NULL)
}
\arguments{
\item{result}{The MoBAMAResult object.}

\item{agClasses}{A named list of antigen classes, each item being
a vector of antigen names used in the data to filter by before computing scores.
Defaults to \code{NULL}, indicating no filtering.}

\item{reClasses}{A named list of Fc variable classes, each item being
a vector of Fc variable names used in the data to filter by before computing scores.
Defaults to \code{NULL}, indicating no filtering.}

\item{includeMagBreadth}{A boolean indicating whether to include
magnitude-weighted breadth scores. Defaults to \code{FALSE}.}
}
\value{
A \code{data.frame} containing breadth scores for each subject,
for all antigens as well as for each antigen class included in agClasses.
}
\description{
Gets fab and FcR breadth scores per subject from
a MoBAMAResult object
}
