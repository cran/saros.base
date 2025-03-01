% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert_chunk.R
\name{insert_chunk}
\alias{insert_chunk}
\title{Mass Create Elements of A Certain Type}
\usage{
insert_chunk(
  chapter_structure_section,
  grouping_structure,
  template_variable_name = ".template"
)
}
\arguments{
\item{chapter_structure_section}{\emph{Overview of chapter section}

\verb{obj:<data.frame>|obj:<tbl_df>} // Required

Data frame (or tibble, possibly grouped). Must contain column 'dep'
with similar items. See \code{draft_report()}.}

\item{grouping_structure}{\emph{Vector of groups}

\verb{vector<character>} // \emph{default:} \code{NULL} (\code{Optional})

Internal usage.}
}
\value{
Named list of elements, where each element can UNFINISHED.
}
\description{
Mass Create Elements of A Certain Type
}
\keyword{internal}
