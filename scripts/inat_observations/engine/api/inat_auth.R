# =============================================================
# inat_observations/engine/api/inat_auth.R   (module -- no side effects, defines functions)
# beescabr -- OAuth 2.0 (authorization-code) sign-in for the iNaturalist API, so an account
# that observers have TRUSTED (or the owner of the records) can pull the TRUE coordinates of
# sensitive taxa (e.g. Bombus crotchii, obscured plants) that the public API hides.
#
# Trust on iNaturalist is granted PER OBSERVER, to a specific account: our surveyors trusted
# @randombirdlover and @cabrillonationalmonument with their obscured coordinates. It is not a
# project setting, and it does NOT transfer to a new account. Signing in as some other
# account returns obscured coordinates no matter how valid its credentials are.
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

if (!exists("cred_get")) source("scripts/utils/credentials.R")

# read a KEY=value line from the gitignored secrets file (an env var of the same name wins).
# Reading NEVER prompts: the prompt belongs at the start of a run (see inat_auth_prompt()),
# not deep inside a request, where it could interrupt a long pull.
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

# inat_auth_prompt(): ask for credentials ONCE at the start of a run if none are present.
# Keys are personal. Whoever runs the pipeline pulls as their own account, and only a
# TRUSTED account (one observers have granted coordinate trust to) receives true
# coordinates for obscured taxa. Running
# unauthenticated is allowed, but it must be a choice: the coordinates come back fuzzed.
inat_auth_prompt <- function(say = message, ...) {
  if (nzchar(.inat_secret("INAT_CLIENT_ID"))) return(invisible(TRUE))
  say("")
  say("  iNaturalist sign-in is not configured on this machine.")
  say("  Without it the pull still works, but SENSITIVE taxa come back with OBSCURED")
  say("  coordinates. True coordinates need an account that observers have individually")
  say("  trusted, such as the park's own iNaturalist account.")
  say("  Set one up ONCE, on the account OBSERVERS ALREADY TRUST (the park account),")
  say("  not on a new personal one: coordinate trust is granted per observer to a")
  say("  specific account and does not carry over.")
  say("    1. Sign in to iNaturalist as that account.")
  say("    2. Go to https://www.inaturalist.org/oauth/applications/new")
  say("    3. Name it something identifiable, e.g. officialbeescabr.")
  say("    4. Set the callback URL to http://localhost:3000/beescabr")
  say("    5. Copy the Client ID and Client Secret it shows you.")
  say("  The FIRST run opens a browser once to authorize; later runs are silent.")
  say("")
  say("  Enter the THREE values below. They are yours, not somebody else's:")
  say("  the pull runs as whichever account authorized the app.")
  for (k in c("INAT_CLIENT_ID", "INAT_CLIENT_SECRET", "INAT_REDIRECT_URI"))
    cred_get(k, INAT_SECRETS_FILE, what = paste("iNaturalist", k), say = say, ...)
  invisible(nzchar(.inat_secret("INAT_CLIENT_ID")))
}

# inat_auth_login(): the signed-in iNaturalist username, or "" when unauthenticated.
# Used to SHOW the operator whose account a run is pulling as, so nobody unknowingly
# runs on somebody else's credentials.
inat_auth_login <- function() {
  if (!inat_auth_enabled()) return("")
  tok <- tryCatch(inat_auth_token(), error = function(e) NULL)
  if (is.null(tok)) return("")
  out <- tryCatch({
    # INAT_BASE_URL already ends in "/", so the path carries no leading slash
    # (same convention as authorize_inat.R); a double slash 404s.
    req <- httr2::request(paste0(INAT_BASE_URL, "users/me"))
    req <- httr2::req_user_agent(httr2::req_headers(req, Authorization = paste("Bearer", tok)),
                                 INAT_USER_AGENT)
    httr2::resp_body_json(httr2::req_perform(req))
  }, error = function(e) NULL)
  if (is.null(out) || !length(out$results)) return("")
  as.character(out$results[[1]]$login %||% "")
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
