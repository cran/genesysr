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
.MAX_ALLOWED_PAGES <- 500

#' Who am i?
#'
#' @export
me <- function() {
  resp <- .api_call("/me")
  message(jsonlite::toJSON(resp, pretty = TRUE))
  invisible(resp)
}

#' Fetch accession passport data (paginated)
#'
#' @param filters an R \code{structure} with Genesys filters
#' @param size number of records to load per page (page size)
#' @param page the page index (0-based)
#' @param selector NULL or a function to "select" variables of interest
#'
#' @seealso \code{\link{mcpd_filter}}
#'
#' @examples
#' \dontrun{
#'   # Retrieve accession data by country of origin
#'   accessions <- fetch_accessions(mcpd_filter(ORIGCTY = c("DEU", "SVN")))
#' }
#' 
#' @return Paged data structure
.fetch_accessions_page <- function(filters = list(), page = 0, size = 1000, selector = NULL) {
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .post(path = "/acn/filter", query=list(p = page, l = size), body = filters)
  if (httr::status_code(resp) != 200) {
    stop("Genesys responded with HTTP status code ", httr::status_code(resp), ". Expected 200.")
  }
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  paged <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  message(paste("Retrieved page", page + 1, "of", paged$totalPages, "with", paged$numberOfElements, "rows in", end_time - start_time, "ms."))
  
  # Apply selector
  if (is.function(selector)) {
    paged$content <- lapply(paged$content, selector)
  }
  paged
}

#' Fetch accession passport data
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
#'   accessions <- fetch_accessions(mcpd_filter(ORIGCTY = c("DEU", "SVN")))
#'
#'   # Fetch Musa
#'   musa <- genesysr::fetch_accessions(list(taxonomy.genus = c('Musa')))
#'
#'   # Apply selector function
#'   accessions <- fetch_accessions(mcpd_filter(ORIGCTY = c("DEU", "SVN")), selector = function(x) {
#'     list(id = x$id, acceNumb = x$acceNumb, instCode = x$institute$code)
#'   })
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
  paged <- .fetch_accessions_page(filters, page = 0, size, selector)
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
      message(paste("Receved", paged$numberOfElements, "of", at.least, "requested. Stopping."))
      break
    }
  }
  
  paged
}
