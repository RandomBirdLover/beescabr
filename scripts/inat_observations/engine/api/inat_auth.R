# =============================================================
# inat_observations/engine/api/inat_auth.R   (module -- no side effects, defines functions)
# beescabr -- OAuth 2.0 (authorization-code) sign-in for the iNaturalist API, so a curator
# of a trusted project (or the owner of obscured records) can pull the TRUE coordinates of
# sensitive taxa (e.g. Bombus crotchii, obscured plants) that the public API hides.
#
# Credentials live ONLY in the gitignored data/secrets/inat_api.env and are read at run
# time -- never hardcoded. If they are absent, auth is OFF and the pipeline behaves exactly
# as before (unauthenticated -> obscured coordinates).
#
# Flow (per iNat): browser authorize -> OAuth access token (/oauth/token) -> exchange at
# /users/api_token for the API JWT -> send the JWT as Bearer to api.inaturalist.org/v1.
# The one-time browser step is cached on disk by httr2; later runs are silent.
#
# Depends on: httr2 (+ config.R for INAT_BASE_URL / INAT_USER_AGENT used by callers).
# =============================================================

if (!exists("INAT_BASE_URL")) source("scripts/config.R")
suppressPackageStartupMessages(library(httr2))

INAT_SECRETS_FILE    <- "data/secrets/inat_api.env"
INAT_OAUTH_AUTH_URL  <- "https://www.inaturalist.org/oauth/authorize"
INAT_OAUTH_TOKEN_URL <- "https://www.inaturalist.org/oauth/token"
INAT_API_TOKEN_URL   <- "https://www.inaturalist.org/users/api_token"

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# read a KEY=value line from the gitignored secrets file (an env var of the same name wins)
.inat_secret <- function(key) {
  v <- Sys.getenv(key)
  if (nzchar(v)) return(v)
  if (!file.exists(INAT_SECRETS_FILE)) return("")
  ln <- readLines(INAT_SECRETS_FILE, warn = FALSE)
  ln <- ln[grepl(paste0("^\\s*", key, "\\s*="), ln)]
  if (!length(ln)) return("")
  val <- sub(paste0("^\\s*", key, "\\s*=\\s*"), "", ln[length(ln)])
  trimws(gsub('^["\']|["\']$', "", trimws(val)))
}

# auth is ON only when an app client id is present (so an unfilled file = unauthenticated)
inat_auth_enabled <- function() nzchar(.inat_secret("INAT_CLIENT_ID"))

.inat_oauth_creds <- function() {
  cr <- list(client_id     = .inat_secret("INAT_CLIENT_ID"),
             client_secret = .inat_secret("INAT_CLIENT_SECRET"),
             redirect_uri  = .inat_secret("INAT_REDIRECT_URI"))
  miss <- names(cr)[!nzchar(unlist(cr))]
  if (length(miss))
    stop("iNat auth not configured -- missing ", paste(toupper(miss), collapse = ", "),
         " in ", INAT_SECRETS_FILE, " (or the environment).")
  cr
}

.inat_oauth_client <- function(cr = .inat_oauth_creds()) {
  oauth_client(id = cr$client_id, secret = cr$client_secret,
               token_url = INAT_OAUTH_TOKEN_URL, name = "beescabr")
}

# get the API JWT (browser sign-in on first use, then cached on disk by httr2)
inat_api_jwt <- function() {
  cr   <- .inat_oauth_creds()
  resp <- request(INAT_API_TOKEN_URL) |>
    req_oauth_auth_code(client       = .inat_oauth_client(cr),
                        auth_url     = INAT_OAUTH_AUTH_URL,
                        redirect_uri = cr$redirect_uri,
                        pkce         = FALSE,           # confidential client (has a secret)
                        cache_disk   = TRUE,
                        cache_key    = "beescabr-inat") |>
    req_perform()
  jwt <- resp_body_json(resp)$api_token %||% ""
  if (!nzchar(jwt)) stop("iNat: empty api_token from ", INAT_API_TOKEN_URL)
  jwt
}

# the token the transport sends as Bearer; NULL when auth is off. Cached per R session.
.inat_auth_env <- new.env(parent = emptyenv())
inat_auth_token <- function(force = FALSE) {
  if (!inat_auth_enabled()) return(NULL)
  if (force || is.null(.inat_auth_env$jwt)) .inat_auth_env$jwt <- inat_api_jwt()
  .inat_auth_env$jwt
}
