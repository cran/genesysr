
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
#' @return Results from validator
#' @export
#' @importFrom utils write.table
#' @importFrom readr read_delim
check_taxonomy <- function(mcpd, toCurrentTaxa = FALSE) {
  DT <- dplyr::distinct(mcpd)
  # print(DT)
  
  CSV <- tempfile(pattern = "taxa", tmpdir = tempdir(), fileext = ".csv")
  utils::write.table(DT, file = CSV, row.names = FALSE, dec=".", sep = "\t", quote = TRUE, na = "")
  
  TMP <- tempfile(pattern = "taxa-res", tmpdir = tempdir(), fileext = ".csv")
  
  if (file.exists(TMP)) {
    stop(paste("Target file", TMP, "exists. Refusing to overwrite."))
  }
  
  outputFile <- file(description = TMP, blocking = T, raw = T, open = "wb")
  write_bytes <- function(x) {
    cat(".")
    writeBin(x, outputFile)
    TRUE
  }
  
  req <- httr2::request("https://validator.genesys-pgr.org/process") %>%
    httr2::req_headers(
      Accept = "text/csv",
    ) %>%
    httr2::req_method(method = "post") %>%
    httr2::req_user_agent(paste("genesysr", .VERSION, "(https://cran.r-project.org/package=genesysr)")) %>%
    httr2::req_body_multipart(
      toCurrentTaxa = ifelse(toCurrentTaxa, "true", "false") , # Requires toCurrentTaxa
      separator = "\t",
      decimalMark = ".",
      escapeChar = "\\",
      encoding = "UTF-8",
      csvText = readChar(CSV, file.info(CSV)$size)
    );
  
  req %>% httr2::req_stream(write_bytes, buffer_kb = 32)
  
  close(outputFile)
  message("Done.")
  R <- readr::read_delim(TMP, delim='\t', quote='"', escape_double=FALSE, escape_backslash=TRUE, show_col_types = FALSE)
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
#' @return Results from validator
#' @export
#' @importFrom utils write.table
#' @importFrom readr read_delim
check_landorsea <- function(mcpd) {
  GEO <- dplyr::filter(mcpd, ! is.na(mcpd$DECLATITUDE) & ! is.na(mcpd$DECLONGITUDE))
  # print(GEO)
  CSV <- tempfile(pattern = "landorsea", tmpdir = tempdir(), fileext = ".csv")
  utils::write.table(GEO, file = CSV, row.names = FALSE, dec=".", sep = "\t", quote = TRUE, na = "")

  TMP <- tempfile(pattern = "landorsea-res", tmpdir = tempdir(), fileext = ".csv")

  if (file.exists(TMP)) {
    stop(paste("Target file", TMP, "exists. Refusing to overwrite."))
  }
  
  outputFile <- file(description = TMP, blocking = T, raw = T, open = "wb")
  write_bytes <- function(x) {
    cat(".")
    writeBin(x, outputFile)
    TRUE
  }
  
  req <- httr2::request("https://validator.genesys-pgr.org/process") %>%
    httr2::req_headers(
      Accept = "text/csv",
    ) %>%
    httr2::req_method(method = "post") %>%
    httr2::req_user_agent(paste("genesysr", .VERSION, "(https://cran.r-project.org/package=genesysr)")) %>%
    httr2::req_body_multipart(
      validateType = "landorsea",
      separator = "\t",
      decimalMark = ".",
      escapeChar = "\\",
      encoding = "UTF-8",
      csvText = readChar(CSV, file.info(CSV)$size)
    );
  
  req %>% httr2::req_stream(write_bytes, buffer_kb = 32)
  
  close(outputFile)
  message("Done.")
  R <- readr::read_delim(TMP, delim='\t', quote='"', escape_double=FALSE, escape_backslash=TRUE, show_col_types = FALSE)
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
#' @return Results from validator
#' @export
#' @importFrom utils write.table
#' @importFrom readr read_delim
check_country <- function(mcpd) {
  GEO <- dplyr::filter(mcpd, is.character(mcpd$ORIGCTY) & ! is.na(mcpd$DECLATITUDE) & ! is.na(mcpd$DECLONGITUDE))
  # print(GEO)

  CSV <- tempfile(pattern = "country", tmpdir = tempdir(), fileext = ".csv")
  utils::write.table(GEO, file = CSV, row.names = FALSE, dec=".", sep = "\t", quote = TRUE, na = "")
  # readChar(CSV, file.info(CSV)$size)

  TMP <- tempfile(pattern = "country-res", tmpdir = tempdir(), fileext = ".csv")
  
  if (file.exists(TMP)) {
    stop(paste("Target file", TMP, "exists. Refusing to overwrite."))
  }
  
  outputFile <- file(description = TMP, blocking = T, raw = T, open = "wb")
  write_bytes <- function(x) {
    cat(".")
    writeBin(x, outputFile)
    TRUE
  }
  
  req <- httr2::request("https://validator.genesys-pgr.org/process") %>%
    httr2::req_headers(
      Accept = "text/csv",
    ) %>%
    httr2::req_method(method = "post") %>%
    httr2::req_user_agent(paste("genesysr", .VERSION, "(https://cran.r-project.org/package=genesysr)")) %>%
    httr2::req_body_multipart(
      validateType = "country",
      separator = "\t",
      decimalMark = ".",
      escapeChar = "\\",
      encoding = "UTF-8",
      csvText = readChar(CSV, file.info(CSV)$size)
    );
  
  req %>% httr2::req_stream(write_bytes, buffer_kb = 32)
  
  close(outputFile)
  message("Done.")

  R <- readr::read_delim(TMP, delim='\t', quote='"', escape_double=FALSE, escape_backslash=TRUE, show_col_types = FALSE)
  # print(R)
  file.remove(CSV, TMP)
  invisible(R)
}
