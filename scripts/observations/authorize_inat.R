# =============================================================
# observations/authorize_inat.R
# Run this ONCE, interactively, to authorize beescabr with your iNaturalist account and
# confirm authenticated access. A browser window opens the first time -- sign in and click
# "Authorize". Fill data/secrets/inat_api.env (client id / secret / redirect uri) first.
#
#   source("scripts/observations/authorize_inat.R")
#
# It prints (1) who you're signed in as and (2) the coordinate fields on a few obscured
# records, so we can see the exact private-coordinate shape before wiring it into the pull.
# =============================================================

source("scripts/config.R")
source("scripts/observations/engine/api/inat_auth.R")

if (!inat_auth_enabled())
  stop("Fill INAT_CLIENT_ID / INAT_CLIENT_SECRET / INAT_REDIRECT_URI in ", INAT_SECRETS_FILE, " first.")

message("Requesting an iNaturalist API token (a browser window opens the first time)...")
jwt <- inat_auth_token(force = TRUE)
message("OK -- got an API token.")

auth_get <- function(path, query = list())
  request(paste0(INAT_BASE_URL, path)) |>
    req_url_query(!!!query) |>
    req_headers(Authorization = paste("Bearer", jwt)) |>
    req_user_agent(INAT_USER_AGENT) |>
    req_perform() |> resp_body_json()

# 1) confirm who we're signed in as
me  <- auth_get("users/me")
who <- me$results[[1]]$login %||% "(unknown)"
message(sprintf("Signed in as: %s", who))

# 2) YOUR OWN records: you can always see the true spot on your own observations, even
#    when obscured. This confirms the mechanism AND shows the exact private-coord format.
cat("\n=== obscured records YOU own (any taxon) ===\n")
mine <- auth_get("observations", list(user_login = who, geoprivacy = "obscured", per_page = 10, order_by = "id"))
cat(sprintf("you own %s obscured record(s)\n", mine$total_results %||% 0))
for (o in mine$results)
  cat(sprintf("  obs %s | %s | public=%s | private=%s\n",
              o$id %||% "?", (o$taxon$name %||% "?"),
              o$location %||% "NULL", o$private_location %||% "(none)"))

cat("\n=== the two taxa you mentioned -- YOUR own records ===\n")
for (tx in c("Bombus crotchii", "Euphorbia misera")) {
  o2 <- auth_get("observations", list(user_login = who, taxon_name = tx, per_page = 5, order_by = "id"))
  cat(sprintf("%s: you have %s\n", tx, o2$total_results %||% 0))
  for (o in o2$results)
    cat(sprintf("  obs %s | obscured=%s | public=%s | private=%s\n",
                o$id %||% "?", o$obscured %||% NA, o$location %||% "NULL", o$private_location %||% "(none)"))
}
cat("\nDone -- paste this back. If 'private' shows real coordinates on your obscured records, it works.\n")
