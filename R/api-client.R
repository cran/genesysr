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


.VERSION = packageVersion("genesysr")
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
  message(paste("genesysr:", .VERSION))
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
#' permissions to R. After successful authentication the browser will 
#' display a message:
#' 
#' ```
#' Authentication complete. Please close this page and return to R.
#' ```
#' 
#' @seealso \code{\link{setup}}
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code
#' @export
user_login <- function() {

  # browser()
  client <- oauth_client(
    id = .genesysEnv$client_id,
    secret = .genesysEnv$client_secret,
    token_url = paste0(.genesysEnv$server, "/oauth/token"),
    auth = "body",
#    name = "Genesys R"
  )
  if (interactive()) {
    message("Please login to Genesys in the browser window");
    token <- oauth_flow_auth_code(client, pkce = T, host_name = "127.0.0.1", scope = "openid", auth_url = paste0(.genesysEnv$server, "/oauth/authorize"))
    # browser()
    authorization(paste("Bearer", token$access_token))
    invisible(token)
  }
}


#' Login to Genesys as a service client (system-to-system)
#'
#' The client must be enabled for Client Credential grant on Genesys.
#'
#' @seealso \code{\link{setup}}
#'
#' @importFrom httr2 oauth_client oauth_flow_client_credentials
#' @export
client_login <- function() {

  client <- oauth_client(
    id = .genesysEnv$client_id,
    secret = .genesysEnv$client_secret,
    token_url = paste0(.genesysEnv$server, "/oauth/token"),
    name = "Genesys R"
  )
  token <- oauth_flow_client_credentials(client, scope = "openid", token_params = list())

  authorization(paste("Bearer", token$access_token))
  invisible(token)
}

# Prepare API request
#' @keywords internal
#' @import magrittr
.api_request <- function(path, method = "get", accept = "application/json", query = NULL, body = NULL, content.type = "application/json") {
  .check_auth()
  
  if (typeof(query$select) != "NULL") {
    query$select <- paste(unlist(query$select), collapse=',')
  }
  
  params <- query
  if (!is.null(query)) names(params) <- paste0(names(params))
  
  #browser()
  req <- httr2::request(base_url = .genesysEnv$server) %>%
    httr2::req_url_path_append(path) %>%
    httr2::req_headers(
      Authorization = .genesysEnv$Authorization,
      Accept = accept,
    ) %>%
    httr2::req_url_query(!!!params) %>%
    httr2::req_method(method = method) %>%
    httr2::req_user_agent(paste("genesysr", .VERSION, "(https://cran.r-project.org/package=genesysr)"))
  
  if (! is.null(body)) {
    req <- req %>% httr2::req_body_raw(body = body, type = content.type);
  }

  invisible(req)
}

#' @keywords internal
#' @import magrittr
.api_call <- function(path, method = "get", accept = "application/json", query = NULL, body = NULL, content.type = "application/json") {
  req <- .api_request(path = path, method = method, accept = accept, query = query, body = body, content.type = content.type);
  
  resp <- req %>%
    httr2::req_perform(verbosity = 0) %>% # Set verbosity to 3 for debugging
    httr2::resp_body_string(encoding = "UTF8")

  return(resp)
}


#' Get partial API v1 URL for the provided path
#'
#' @param path relative path of the API v1 endpoint (e.g. \code{/me})
#'
#' @return Returns "/api/v1" + path
#' @export
#'
#' @examples
#'  api1_url("/me")
api1_url <- function(path) {
  paste0("/api/v1", path)
}


#' Get partial API v2 URL for the provided path
#'
#' @param path relative path of the API v2 endpoint (e.g. \code{/me})
#'
#' @return Returns "/api/v2" + path
#' @export
#'
#' @examples
#'  api2_url("/me")
api2_url <- function(path) {
  paste0("/api/v2", path)
}

#' @keywords internal
.get <- function(path, query = NULL, accept = "application/json") {
  resp <- .api_call(path, query = query, accept = accept)
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
  content <- jsonlite::toJSON(body, auto_unbox = TRUE)
  if (! is.null(body) && length(body) == 0) {
    # If body is provided, but has length of 0
    content <- "{}"
  }
  # print(paste("Body is:", content))
  resp <- .api_call(path, method = "post", query = query, accept = accept, content.type = content.type, body = content)

  resp
}
