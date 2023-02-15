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


MCPD <- list(
  SAMPSTAT = list("Wild" = list(100, "Natural" = 110, "Semi-natural/wild"=120, "Semi-natural/sown"=130),
                  "Weedy" = 200, 
                  "Traditional cultivar/landrace" = 300,
                  "Breeding/research material"= list(400, "Breeder's line"=410,"Synthetic population"=411,"Hybrid"=412,"Founder stock/base population"=413,"Inbred line (parent of hybrid cultivar)"=414,"Segregating population"=415,"Clonal selection"=416,"Genetic stock"=420,"Mutant (e.g. induced/insertion mutant, tilling population)"=421,"Cytogenetic stock (e.g. chromosome addition/substitution, aneuploids, amphiploids)"=422,"Other genetic stocks (e.g. mapping populations)"=423),
                  "Advanced or improved cultivar (conventional breeding methods)"=500,
                  "GMO (by genetic engineering)"=600,
                  "Other (elaborate in REMARKS field)"=999),
  STORAGE = list("Seed collection"=list(10,"Short term"=11,"Medium term"=12,"Long term"=13),
                 "Field collection"=20,
                 "In vitro collection"=30,
                 "Cryopreserved collection"=40,
                 "DNA collection"=50,
                 "Other (elaborate in REMARKS field)"=99),
  COLLSRC = list("Wild habitat"=list(10,"Forest or woodland"=11,"Shrubland"=12,"Grassland"=13,"Desert or tundra"=14,"Aquatic habitat"=15),
                 "Farm or cultivated habitat"=list(20,"Field"=21,"Orchard"=22,"Backyard, kitchen or home garden (urban, peri-urban or rural)"=23,"Fallow land"=24,"Pasture"=25,"Farm store"=26,"Threshing floor"=27,"Park"=28),
                 "Market or shop"=30,
                 "Institute, Experimental station, Research organization, Genebank"=40,
                 "Seed company"=50,
                 "Weedy, disturbed or ruderal habitat"=list(60,"Roadside"=61,"Field margin"=62),
                 "Other (Elaborate in REMARKS field)"=99)
)

#' Make or adjust filter using MCPD terminology
#'
#' See FAO/Bioversity Multi-Crop Passport Descriptors.
#'
#' @param filter Existing filters (or blank list if not provided)
#' @param INSTCODE WIEWS Institute Code of the holding institute
#' @param DOI Accession DOI
#' @param ORIGCTY Country of origin
#' @param SAMPSTAT Biological status of sample
#' @param GENUS List of genera
#' @param SPECIES List of specific epithets (within specified genera)
#'
#' @examples
#'  # Filter accessions from Mexico and Slovenia
#'  mcpd_filter(ORIGCTY = c("MEX", "SVN"))
#'
#'
#' @export
mcpd_filter <- function(filter = list(), INSTCODE = NULL, DOI = NULL, ORIGCTY = NULL, SAMPSTAT = NULL, GENUS = NULL, SPECIES = NULL) {
  f <- c(filter)

  f <- filter_DOI(f, DOI)
  f <- filter_ORIGCTY(f, ORIGCTY)
  f <- filter_SAMPSTAT(f, SAMPSTAT)
  f <- filter_GENUS(f, GENUS)
  f <- filter_SPECIES(f, SPECIES)
  f <- filter_INSTCODE(f, INSTCODE)
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
    f$countryOfOrigin$iso3 = c(f$countryOfOrigin$iso3, ORIGCTY)
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


#' Add filter by genus
#' @param filter Existing filters (or blank list if not provided)
#' @param GENUS List of genera
#' @export
filter_GENUS <- function(filter = list(), GENUS) {
  f <- c(filter)
  if (!is.null(GENUS)) {
    f$taxonomy$genus = c(f$taxonomy$genus, GENUS)
  }
  f
}


#' Add filter on specific epithet
#' @param filter Existing filters (or blank list if not provided)
#' @param SPECIES List of specific epithets
#' @export
filter_SPECIES <- function(filter = list(), SPECIES) {
  f <- c(filter)
  if (!is.null(SPECIES)) {
    f$taxonomy$species = c(f$taxonomy$species, SPECIES)
  }
  f
}


#' Add filter by genus
#' @param filter Existing filters (or blank list if not provided)
#' @param INSTCODE List of WIEWS institute codes
#' @export
filter_INSTCODE <- function(filter = list(), INSTCODE) {
  f <- c(filter)
  if (!is.null(INSTCODE)) {
    f$institute$code = c(f$institute$code, INSTCODE)
  }
  f
}
