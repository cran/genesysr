% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{mcpd_filter}
\alias{mcpd_filter}
\title{Make or adjust filter using MCPD terminology}
\usage{
mcpd_filter(
  filter = list(),
  INSTCODE = NULL,
  DOI = NULL,
  ORIGCTY = NULL,
  SAMPSTAT = NULL,
  GENUS = NULL,
  SPECIES = NULL
)
}
\arguments{
\item{filter}{Existing filters (or blank list if not provided)}

\item{INSTCODE}{WIEWS Institute Code of the holding institute}

\item{DOI}{Accession DOI}

\item{ORIGCTY}{Country of origin}

\item{SAMPSTAT}{Biological status of sample}

\item{GENUS}{List of genera}

\item{SPECIES}{List of specific epithets (within specified genera)}
}
\description{
See FAO/Bioversity Multi-Crop Passport Descriptors.
}
\examples{
 # Filter accessions from Mexico and Slovenia
 mcpd_filter(ORIGCTY = c("MEX", "SVN"))


}
