# Genesys R client

The `genesysr` R package provides functions for authentication with Genesys and functions to fetch accession data from <https://www.genesys-pgr.org> database.

## Installing the development version

```R
devtools::install_git('https://gitlab.croptrust.org/genesys-pgr/genesysr')
```

# Using `genesysr`

1.  Setup and configure
2.  Authenticate
3.  User authentication
4.  Client authentication
5.  Query

## Setup and configuration

```R
library('genesysr')

# www.genesys-pgr.org
genesysr::setup_production()

# or sandbox.genesys-pgr.org
genesysr::setup_sandbox()

# or custom
genesysr::setup(server = "http://localhost:8080",
  client_id = "...", client_secret = "...")
```

## User authentication

The package is configured with default **Genesys R client** credentials that require user authentication:

```R
# Setup
genesysr::setup_production()

# Open Genesys web page in browser and get verifier code
genesysr::user_login()
```

## Client authentication

Accessing Genesys **without user interaction** requires that a client is registered on Genesys with **client credentials** grant. Contact helpdesk@genesys-pgr.org for assistance.

```R
# Setup
library('genesysr')
genesysr::setup(server = "https://www.genesys-pgr.org",
  client_id = "someId.....@www.genesys-pgr.org",
  client_secret = "your-private-secret")

# Obtain access tokens
genesysr::client_login()
```

## Query Genesys

```R
filters <- mcpd_filter(ORIGCTY = c("DEU", "SVN"))
accessions <- genesysr::fetch_accessions(filters)
```
