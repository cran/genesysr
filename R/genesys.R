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


#' Max pages to retrieve
#' @keywords internal
.MAX_ALLOWED_PAGES <- 500

#' Who am i? Loads and prints the user profile from Genesys as JSON.
#' You need to be logged in.
#'
#' @examples
#' \dontrun{
#'   # Login
#'   setup_production()
#'   user_login()
#'   me()
#' }
#' 
#' @export
me <- function() {
  resp <- .api_call(api1_url("/me/profile"))
  message(jsonlite::toJSON(jsonlite::fromJSON(resp), pretty = TRUE))
  invisible(resp)
}


#' Fetch accession passport data page.
#' @return Paged data structure
#' @keywords internal
.fetch_accessions_page <- function(filters = list(), page = 0, size = 1000, selector = NULL) {
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .post(path = api1_url("/acn/query"), query=list(p = page, l = size, select = c("institute.code", "accessionNumber", "countryOfOrigin.code3")), body = filters)
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  paged <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
  message(paste("Retrieved page", page + 1, "of", paged$totalPages, "with", paged$numberOfElements, "rows in", end_time - start_time, "ms."))
  
  # Apply selector
  if (is.function(selector)) {
    paged$content <- lapply(paged$content, selector)
  }
  paged
}

#' Fetch accession passport data and return the paged data structure for further processing.
#' \code{\link{get_accessions}} might be more useful as it returns a data table.
#' 
#' @seealso \code{\link{get_accessions}}
#'
#' @param filters an R \code{structure} with Genesys filters
#' @param size number of records to load per page (page size)
#' @param page the page index (0-based)
#' @param selector NULL or a function to "select" variables of interest
#' @param at.least stop fetching when at.least records are received from Genesys
#'
#' @examples
#' \dontrun{
#'   # Retrieve all accession data by country of origin
#'   accessions <- genesysr::fetch_accessions(mcpd_filter(ORIGCTY = c("DEU", "SVN")))
#'
#'   # Fetch Musa
#'   musa <- genesysr::fetch_accessions(list(taxonomy.genus = c('Musa')))
#'
#'   # Apply selector function
#'   accessions <- genesysr::fetch_accessions(
#'     mcpd_filter(ORIGCTY = c("DEU", "SVN")),
#'     selector = function(x) {
#'       list(id = x$id, acceNumb = x$acceNumb, instCode = x$institute$code)
#'     }
#'   )
#' }
#' 
#' @export
#' @return Paged data structure
fetch_accessions <- function(filters = list(), page = NULL, size = 1000, selector = NULL, at.least = NULL) {
  if (! is.null(page)) {
    # Fetch page
    return(.fetch_accessions_page(filters, page, size, selector));
  }
  
  # Fetch first page to determine number of records
  paged <- .fetch_accessions_page(filters, page = 0, size = size, selector = selector)
  pages <- paged$totalPages
  
  for (page in 1:pages) {
    if (page > .MAX_ALLOWED_PAGES) {
      # Break if over max pages
      message(paste("Not requesting data after page", .MAX_ALLOWED_PAGES, "Stopping."))
      break
    }
    p <- .fetch_accessions_page(filters, page, size, selector)
    
    paged$content <- c(paged$content, p$content)
    paged$last <- p$last
    paged$numberOfElements <- paged$numberOfElements + p$numberOfElements
    
    if (p$last) {
      # print("Got last page")
      break
    }
    if (! is.null(at.least) && at.least <= paged$numberOfElements) {
      message(paste("Received", paged$numberOfElements, "of", at.least, "requested. Stopping."))
      break
    }
  }
  
  paged
}


#' List accession passport data (paginated)
#' 
#' @return table
#' @keywords internal
#' @importFrom utils read.csv
.list_accessions_page <- function(filters = list(), page = 0, size = 1000, fields = NULL, exclude = NULL, selector = NULL) {
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  query <- list(p = page, l = size)
  if (is.vector(fields)) {
    selected_fields <- stats::setNames(as.list(fields), rep('select', length(fields)))
    query <- c(query, selected_fields)
  }
  if (is.vector(exclude)) {
    excluded_fields <- stats::setNames(as.list(exclude), rep('exclude', length(exclude)))
    query <- c(query, excluded_fields)
  }
  resp <- .post(path = api1_url("/acn/list"), query = query, body = filters, accept = "text/csv")
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved page", page + 1, "in", end_time - start_time, "ms."))
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- utils::read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
    
    # Apply selector
    if (is.function(selector)) {
      data <- lapply(data, selector)
    }
  }
  data
}


#' Get accession passport data as a data table.
#'
#' @param filters an R \code{structure} with Genesys filters
#' @param size number of records to load per page (page size)
#' @param page the page index (0-based)
#' @param fields list of fields to fetch from Genesys
#' @param exclude list of field prefixes to exclude from the Genesys response
#' @param selector NULL or a function to "select" variables of interest
#' @param at.least stop fetching when at.least records are received from Genesys
#'
#' @seealso \code{\link{mcpd_filter}}
#' 
#' @examples
#' \dontrun{
#'   # Retrieve all accession data by country of origin (Slovenia, Ivory Coast)
#'   accessions <- genesysr::get_accessions(list(countryOfOrigin = list(code3 = c('SVN', 'CIV'))))
#'
#'   # Fetch Musa, but only geographic data and accessionNumber
#'   musa <- genesysr::get_accessions(list(taxonomy = list(genus = c('Musa'))),
#'     fields = c("accessionNumber", "geo"))
#'
#'   # Apply selector function
#'   accessions <- genesysr::get_accessions(mcpd_filter(ORIGCTY = c('DEU', 'SVN')),
#'     selector = function(x) {
#'       list(id = x$id, acceNumb = x$accessionNumber, instCode = x$instituteCode)
#'     }, at.least = 100)
#' }
#' 
#' @export
#' @return Data table
get_accessions <- function(filters = list(), page = 0, size = 1000, fields = NULL, exclude = NULL, selector = NULL, at.least = NULL) {

  # Fetch first page to determine number of records
  data <- .list_accessions_page(filters, page, size, fields, exclude, selector)

  while (page < .MAX_ALLOWED_PAGES && !(! is.null(at.least) && at.least <= nrow(data))) {
    page <- page + 1
    if (page >= .MAX_ALLOWED_PAGES) {
      # Break if over max pages
      message(paste("Not requesting data after page", .MAX_ALLOWED_PAGES, "Stopping."))
      break
    }
    p <- .list_accessions_page(filters, page, size, fields, exclude, selector)
    
    if (nrow(p) == 0) {
      # print("Got last page")
      break
    }
    
    data[setdiff(names(p), names(data))] <- NA
    p[setdiff(names(data), names(p))] <- NA
    data <- rbind(data, p)
    
    if (! is.null(at.least) && at.least <= nrow(data)) {
      message(paste("Receved", nrow(data), "of", at.least, "requested. Stopping."))
      break
    }
  }
  
  data
}


#' Download passport data for one genebank in Excel format and save it to disk
#'
#' @param instituteCode FAO WIEWS institute code
#' @param file Target file name. Defaults to Genesys-provided file name in the current working directory.
#'
#' @examples
#' \dontrun{
#'   # Download MCPD passport data for NGA039
#'   excelFile <- download_mcpd("NGA039")
#' }
#' 
#' @export
#' @return The downloaded MCPD file name
download_mcpd <- function(instituteCode, file = NULL) {
  if (is.na(instituteCode)) {
    stop("instituteCode parameter is required")
  }
  if (is.null(file)) {
    file <- paste0("genesys-accessions-", instituteCode, ".xlsx")
  }
  if (file.exists(file)) {
    stop(paste("Target file", file, "exists. Refusing to overwrite."))
  }

  outputFile <- file(description = file, blocking = T, raw = T, open = "wb")
  write_bytes <- function(x) {
    cat(".")
    writeBin(x, outputFile)
    TRUE
  }

  req <- .api_request(
    method = "post",
    path = api1_url(paste0("/wiews/", instituteCode, "/download")),
  ) %>%
    httr2::req_body_form(mcpd = "mcpd");

  req %>% httr2::req_stream(write_bytes, buffer_kb = 32)

  close(outputFile)
  message("Done.")
  invisible(file)
}


#' Download PDCI data for one genebank in Excel format and save it to disk.
#'
#' @param instituteCode FAO WIEWS institute code
#' @param file Target file name. Defaults to Genesys-provided file name in the current working directory.
#'
#' @examples
#' \dontrun{
#'   # Download PDCI  data for NGA039
#'   excelData <- download_pdci("NGA039")
#' }
#' 
#' @export
#' @return The downloaded PDCI file name
download_pdci <- function(instituteCode, file = NULL) {
  if (is.na(instituteCode)) {
    stop("instituteCode parameter is required")
  }
  if (is.null(file)) {
    file <- paste0("genesys-pdci-", instituteCode, ".xlsx")
  }
  if (file.exists(file)) {
    stop(paste("Target file", file, "exists. Refusing to overwrite."))
  }
  
  outputFile <- file(description = file, blocking = T, raw = T, open = "wb")
  write_bytes <- function(x) {
    cat(".")
    writeBin(x, outputFile)
    TRUE
  }
  
  req <- .api_request(
    method = "post",
    path = api1_url(paste0("/wiews/", instituteCode, "/download")),
  ) %>%
    httr2::req_body_form(pdci = "pdci");
  
  
  req %>% httr2::req_stream(write_bytes, buffer_kb = 32)
  
  close(outputFile)
  message("Done.")
  invisible(file)
}



#' Fetch Genesys crops. Note that the list of Genesys crops does not fully
#' correspond with various CROPNAME in MCPD provided by genebanks.
#'
#' @examples
#' \dontrun{
#'   # Retrieve all Genesys crops
#'   crops <- genesysr::list_crops()
#' }
#' 
#' @export
#' @return Genesys crops
list_crops <- function() {
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .api_call(path = api1_url("/crops/list"), query = list(l=1000), accept = "text/csv")
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved crops in", end_time - start_time, "ms."))
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
  }
  data
}



#' Fetch taxonomic data of selected accessions.
#'
#' @param filters an R \code{structure} with Genesys filters
#'
#' @examples
#' \dontrun{
#'   # Retrieve taxa of selected accessions
#'   taxa <- genesysr::list_species(mcpd_filter(INSTCODE = c("LBN002", "MEX002")))
#' }
#' 
#' @seealso \code{\link{mcpd_filter}}
#'
#' @export
#' @return Taxonomic records of selected accessions
list_species <- function(filters = list()) {
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .post(path = api1_url("/acn/species"), body = filters, accept = "text/csv")
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved taxonomic data in", end_time - start_time, "ms."))
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
  }
  data
}

#' Fetch a page of data from Genesys
#' 
#' @param path API path
#' @param filters Filters
#' @param accept Accepted content type
#' @param page Page to request
#' @param size Size of page
#' 
#' @keywords internal
.fetch_csv_page <- function(path, filters = list(), accept = "text/csv", page = 0, size = 1000) {
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .post(path, query = list(l = size, p = page), body = filters, accept = accept)
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved institute data in", end_time - start_time, "ms."))
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
  }
  data
}

#' Fetch a page of data from Genesys
#' 
#' @param path API path
#' @param filters Filters
#' @param accept Accepted content type
#' @param page Page to request
#' @param size Size of page
#' 
#' @keywords internal
#' @import magrittr
#' @importFrom dplyr select
#' @importFrom tidyselect contains one_of
.fetch_json_page <- function(path, filters = list(), accept = "application/json", page = 0, size = 1000) {
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .post(path, query = list(l = size, p = page), body = filters, accept = accept)
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved JSON page in", end_time - start_time, "ms."))
  
  .result <- jsonlite::fromJSON(resp, flatten = T)
  if (is.list(.result$content)) {
    .result <- .result$content # Ignore the rest
  }
  .result <- .result %>% select(-contains('_permissions'), -one_of('createdBy', 'lastModifiedBy', '_class')) # Remove Genesys stuff
  return(.result)
}

#' List FAO WIEWS institutes.
#'
#' Institute filters:
#' - code: list of WIEWS institute codes
#' - accessions: boolean, TRUE list only institutes with accessions in Genesys, FALSE without accessions
#' - country$code3: list of ISO3166 country codes
#'
#' @param filters an R \code{structure} with Institute filters
#' @param at.least stop fetching when at.least records are received from Genesys
#'
#' @examples
#' \dontrun{
#'   # Retrieve taxa of selected accessions
#'   filters <- c();
#'   filters$accessions = TRUE; # Has accessions in Genesys
#'   institutes <- genesysr::list_institutes(filters)
#' }
#' 
#' @seealso \code{\link{mcpd_filter}}
#'
#' @export
#' @return List of institutes
list_institutes <- function(filters = list(), at.least = NULL) {

  path <- api1_url("/wiews/list")
  
  # Fetch first page to determine number of records
  data <- .fetch_csv_page(path, filters, page = 0, size = 100)
  pages <- .MAX_ALLOWED_PAGES
  
  for (page in 1:pages) {
    if (page > .MAX_ALLOWED_PAGES) {
      # Break if over max pages
      message(paste("Not requesting data after page", .MAX_ALLOWED_PAGES, "Stopping."))
      break
    }
    p <- .fetch_csv_page(path, filters, page = page, size = 100)

    if (nrow(p) == 0) {
      # print("Got last page")
      break
    }
    
    data[setdiff(names(p), names(data))] <- NA
    p[setdiff(names(data), names(p))] <- NA
    data <- rbind(data, p)

    if (length(p) == 0) {
      # print("Got last page")
      break
    }
    if (! is.null(at.least) && at.least <= length(data)) {
      message(paste("Received", length(data), "of", at.least, "requested. Stopping."))
      break
    }
  }

  data
}


