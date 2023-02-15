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

.genesysEnv <- new.env(parent = emptyenv())

#' Configure package defaults on load
#' 
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  setup_production()
}

#' Setup for Genesys Production
#'
#' Use the Genesys R Client with <https://api.genesys-pgr.org> requiring \code{\link{user_login}}
#'
#' @export
setup_production <- function() {
  setup(server = "https://api.genesys-pgr.org", client_id = "oHgJR.NjcdJAIB175gBDbuLEK3@www.genesys-pgr.org", client_secret = "public")
}

#' Setup for Genesys Sandbox
#'
#' Use the Genesys R Client with <https://api.sandbox.genesys-pgr.org> requiring \code{\link{user_login}}
#'
#' @export
setup_sandbox <- function() {
  setup(server = "https://api.sandbox.genesys-pgr.org", client_id = "cCS6e.BAn9u2WkhIwgxBLxOVqZ@sandbox.genesys-pgr.org", client_secret = NULL)
}

#' Configure the Genesys environment
#'
#' @param server Server base URL (e.g. "https://api.genesys-pgr.org" or "https://api.sandbox.genesys-pgr.org")
#' @param client_id OAuth client ID
#' @param client_secret OAuth client secret
#'
#' @export
#' @seealso See utility methods \code{\link{setup_production}}, \code{\link{setup_sandbox}}
#'
#' @examples
#'   # Link with sandbox
#'   setup_sandbox()
#'
setup <- function(server = NULL, client_id = NULL, client_secret = NULL) {
  assign("server", server, envir = .genesysEnv)
  assign("client_id", client_id, envir = .genesysEnv)
  assign("client_secret", client_secret, envir = .genesysEnv)
}

#' Print Genesys client configuration
#'
#' @export
print_setup <- function() {
  message(paste("Genesys URL:", .genesysEnv$server))
  message(paste("Client ID:", .genesysEnv$client_id))
  message(paste("Client secret:", .genesysEnv$client_secret))
}

#' Provide OAuth2 token to use for authorization with Genesys
#'
#' @param authorization OAuth2 Authorization header obtained from somewhere else (e.g. an ENV variable)
#'
#' @seealso \code{\link{user_login}}, \code{\link{client_login}}
#' @export
authorization <- function(authorization) {
  assign("Authorization", authorization, envir = .genesysEnv)
  message(paste('Genesys Authorization:', authorization))
}

#' Ensure that environment has OAuth token
#' @keywords internal
.check_auth <- function() {
  if (is.null(.genesysEnv$Authorization)) {
    warning("You must first authorize with Genesys with user_login() or client_login(...).");
  }
}

#' Login to Genesys as a user
#'
#' The authorization URL will open in a browser, ask the user to grant
#' permissions to R and the verification code must be copy-pasted after
#' you grant access to the client.
#'
#' @seealso \code{\link{setup}}
#'
#' @importFrom utils browseURL
#' @export
user_login <- function() {
  url <- paste0(.genesysEnv$server, "/oauth/authorize")
  browseURL(httr::modify_url(url, query = list(
    client_id = .genesysEnv$client_id, client_secret = .genesysEnv$client_secret,
    redirect_uri = "oob",
    scope = "read",
    response_type= "code"
    )))

  code <- readline("Enter the authorization code: ");

  url <- paste0(.genesysEnv$server, "/oauth/token")
  resp <- httr::POST(url, body = list(
    client_id = .genesysEnv$client_id, client_secret = .genesysEnv$client_secret,
    redirect_uri = "oob",
    grant_type = "authorization_code", code = code
    ), encode = "form")

  if (httr::http_type(resp) != "application/json" || httr::status_code(resp) != 200) {
    stop(paste("API did not return json", httr::content(resp, "text")), call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  authorization(paste("Bearer", parsed$access_token))

  invisible(structure(
    parsed,
    class = "genesys_auth"
  ))
}


#' Login to Genesys as a service client (system-to-system)
#'
#' The client must be enabled for Client Credential grant on Genesys.
#'
#' @seealso \code{\link{setup}}
#'
#' @export
client_login <- function() {
  url <- paste0(.genesysEnv$server, "/oauth/token")
  resp <- httr::POST(url, body = list(
    client_id = .genesysEnv$client_id, client_secret = .genesysEnv$client_secret,
    grant_type = "client_credentials"
  ), encode = "form")

  if (httr::http_type(resp) != "application/json" || httr::status_code(resp) != 200) {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  authorization(paste("Bearer", parsed$access_token))

  invisible(structure(
    parsed,
    class = "genesys_auth"
  ))
}


#' @keywords internal
.api_call <- function(path, method = "get") {
  .check_auth()
  resp <- httr::GET(path, httr::add_headers(
    Authorization = .genesysEnv$Authorization
    )
  )
  resp
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # return(resp)
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  return(parsed)
}


#' Get full Genesys API v1 URL for a specific path
#'
#' @param path relative path of the API v1 endpoint (e.g. \code{/me})
#'
#' @return Absolute URL to an API call
#' @export
#'
#' @examples
#'  api1_url("/me")
api1_url <- function(path) {
  paste0(.genesysEnv$server, "/api/v1", path)
}


#' Get full Genesys API v1 URL for a specific path
#'
#' @param path relative path of the API v1 endpoint (e.g. \code{/me})
#'
#' @return Absolute URL to an API call
#' @export
#'
#' @examples
#'  api2_url("/me")
api2_url <- function(path) {
  paste0(.genesysEnv$server, "/api/v2", path)
}

#' @keywords internal
.get <- function(path, query = NULL, accept = "application/json") {
  .check_auth()
  resp <- httr::GET(path, query = query, httr::add_headers(
    Authorization = .genesysEnv$Authorization,
    "Accept" = accept
  ))
  if (httr::http_type(resp) != accept) {
    stop("API did not return ", accept, " but Content-Type: ", httr::content(resp), ". See response content:\n", httr::content(resp), call. = FALSE)
  }
  resp
}

#' HTTP POST method
#'
#' @param path API path
#' @param query query string parameters
#' @param body request body (will be serialized to JSON)
#' @param content.type Content-Type of the body
#'
#' @return httr response
#' @keywords internal
.post <- function(path, query = NULL, body = NULL, content.type = "application/json", accept = "application/json") {
  .check_auth()
  content <- jsonlite::toJSON(body, auto_unbox = TRUE)
  if (! is.null(body) && length(body) == 0) {
    # If body is provided, but has length of 0
    content <- "{}"
  }
  # print(paste("Body is:", content))
  resp <- httr::POST(path, query = query, 
    httr::add_headers(
      Authorization = .genesysEnv$Authorization,
      "Content-Type" = content.type,
      "Accept" = accept
    ), 
    # httr::verbose(),
    body = content
  )

  if (httr::status_code(resp) != 200) {
    stop("Genesys responded with HTTP status code ", httr::status_code(resp), ". Expected 200. See response content:\n", httr::content(resp), call. = FALSE)
  }
  if (httr::http_type(resp) != accept) {
    stop("API did not return ", accept, " but Content-Type: ", httr::content(resp), ". See response content:\n", httr::content(resp), call. = FALSE)
  }
  
  # if (httr::http_type(resp) != "application/json") {
  #  stop("API did not return json", call. = FALSE)
  # }
  resp
}
