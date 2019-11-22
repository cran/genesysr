
#' Check MCPD taxonomic data (GENUS, SPECIES, SPAUTHOR, SUBTAXA, SUBTAUTHOR)
#' using https://validator.genesys-pgr.org.
#' 
#' Duplicate input rows are removed using dplyr::distinct() and results are
#' returned for unique rows.
#' 
#' @param mcpd Accession passport data in MCPD format
#' @param toCurrentTaxa Should obsoleted names be reported?
#' 
#' @examples
#' \dontrun{
#'   taxaCheck <- genesysr::check_taxonomy(mcpd)
#' }
#' 
#' @return Results from valdator
#' @export
#' @importFrom utils read.csv write.table
check_taxonomy <- function(mcpd, toCurrentTaxa = FALSE) {
  DT <- dplyr::distinct(mcpd)
  # print(DT)

  CSV <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = "")
  write.table(DT, file = CSV, row.names = FALSE, dec=".", sep = "\t", quote = FALSE, na = "")
  
  TMP <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = "")

  # Requires toCurrentTaxa
  response <- httr::POST("https://validator.genesys-pgr.org/process", body = list(
    toCurrentTaxa = toCurrentTaxa,
    separator = "\t", decimalMark = ".", 
    encoding = "UTF-8", csvText = readChar(CSV, file.info(CSV)$size)
  ), encode = "multipart", httr::accept("text/csv"), httr::write_disk(TMP)) # , httr::verbose())
  
  R <- read.csv(TMP, fileEncoding = "UTF-8", sep = "\t")
  file.remove(CSV, TMP)
  invisible(R)
}

#' Run Land-or-Sea check on MCPD data using https://validator.genesys-pgr.org.
#' Uploads only rows where DECLATITUDE and DECLONGITUDE are provided. 
#' In practice it is better to use `check_country` if ORIGCTY data exists.
#' 
#' @param mcpd Accession passport data in MCPD format
#' 
#' @examples
#' \dontrun{
#'   waterCheck <- genesysr::check_landorsea(mcpd)
#' }
#' 
#' @return Results from valdator
#' @export
#' @importFrom utils read.csv write.table
check_landorsea <- function(mcpd) {
  GEO <- dplyr::filter(mcpd, ! is.na(mcpd$DECLATITUDE) & ! is.na(mcpd$DECLONGITUDE))
  # print(GEO)

  CSV <- tempfile(pattern = "landorsea", tmpdir = tempdir(), fileext = "")
  write.table(GEO, file = CSV, row.names = FALSE, dec=".", sep = "\t", quote = FALSE, na = "")
  readChar(CSV, file.info(CSV)$size)

  TMP <- tempfile(pattern = "landorsea", tmpdir = tempdir(), fileext = "")
  
  response <- httr::POST("https://validator.genesys-pgr.org/process", body = list(
    validateType = "landorsea",
    separator = "\t", decimalMark = ".", 
    encoding = "UTF-8", csvText = readChar(CSV, file.info(CSV)$size)
  ), encode = "multipart", httr::accept("text/csv"), httr::write_disk(TMP)) # , httr::verbose())
  
  R <- read.csv(TMP, fileEncoding = "UTF-8", sep = "\t")
  # print(R)
  file.remove(CSV, TMP)
  invisible(R)
}

#' Run Land-or-Sea check on MCPD data. Uploads only rows where ORIGCTY,
#' DECLATITUDE and DECLONGITUDE are provided.
#' 
#' @param mcpd Accession passport data in MCPD format
#' 
#' @examples
#' \dontrun{
#'   geoCheck <- genesysr::check_country(mcpd)
#' }
#' 
#' @return Results from valdator
#' @export
#' @importFrom utils read.csv write.table
check_country <- function(mcpd) {
  GEO <- dplyr::filter(mcpd, is.character(mcpd$ORIGCTY) & ! is.na(mcpd$DECLATITUDE) & ! is.na(mcpd$DECLONGITUDE))
  # print(GEO)

  CSV <- tempfile(pattern = "landorsea", tmpdir = tempdir(), fileext = "")
  write.table(GEO, file = CSV, row.names = FALSE, dec=".", sep = "\t", quote = FALSE, na = "")
  readChar(CSV, file.info(CSV)$size)

  TMP <- tempfile(pattern = "landorsea", tmpdir = tempdir(), fileext = "")
  
  response <- httr::POST("https://validator.genesys-pgr.org/process", body = list(
    validateType = "country",
    separator = "\t", decimalMark = ".", 
    encoding = "UTF-8", csvText = readChar(CSV, file.info(CSV)$size)
  ), encode = "multipart", httr::accept("text/csv"), httr::write_disk(TMP)) # , httr::verbose())
  
  R <- read.csv(TMP, fileEncoding = "UTF-8", sep = "\t")
  # print(R)
  file.remove(CSV, TMP)
  invisible(R)
}
