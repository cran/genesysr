# Copyright 2018 Global Crop Diversity Trust
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Make or adjust filter using MCPD terminology
#'
#' See FAO/Bioversity Multi-Crop Passport Descriptors.
#'
#' @param filter Existing filters (or blank list if not provided)
#' @param DOI Accession DOI
#' @param ORIGCTY Country of origin
#' @param SAMPSTAT Biological status of sample
#'
#' @examples
#'  # Filter accessions from Mexico and Slovenia
#'  mcpd_filter(ORIGCTY = c("MEX", "SVN"))
#'
#'
#' @export
mcpd_filter <- function(filter = list(), DOI = NULL, ORIGCTY = NULL, SAMPSTAT = NULL) {
  f <- c(filter)

  f <- filter_DOI(f, DOI)
  f <- filter_ORIGCTY(f, ORIGCTY)
  f <- filter_SAMPSTAT(f, SAMPSTAT)

  f
}

#' Add filter on accession DOI
#' @param filter Existing filters (or blank list if not provided)
#' @param DOI Accession DOI
#' @export
filter_DOI <- function(filter = list(), DOI) {
  f <- c(filter)
  if (!is.null(DOI)) {
    f$doi = c(f$doi, DOI)
  }
  f
}

#' Add filter on Country of origin of material
#' @param filter Existing filters (or blank list if not provided)
#' @param ORIGCTY Country of origin
#' @export
filter_ORIGCTY <- function(filter = list(), ORIGCTY) {
  f <- c(filter)
  if (!is.null(ORIGCTY)) {
    f$orgCty.iso3 = c(f$orgCty.iso3, ORIGCTY)
  }
  f
}

#' Add filter on Biological status of sample
#' @param filter Existing filters (or blank list if not provided)
#' @param SAMPSTAT Biological status of sample
#' @export
filter_SAMPSTAT <- function(filter = list(), SAMPSTAT) {
  f <- c(filter)
  if (!is.null(SAMPSTAT)) {
    f$sampStat = c(f$sampStat, SAMPSTAT)
  }
  f
}
