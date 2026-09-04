# =============================================================
# website/setup_notice.R
# beescabr -- tell whoever is building the site, on their FIRST build, what GitHub
# Pages needs from them.
#
# WHY: the deploy step assumes Pages is already serving docs/ from the main branch and
# that you can push to origin. Neither of those is in this repository -- they are
# settings on a GitHub account -- so neither survives a clone or a fork. Someone
# picking the project up gets a rebuilt docs/ folder, a push that fails or goes
# nowhere visible, and no clue why. Saying it in a guide is not enough; the person
# running the pipeline is looking at the console, so the console says it.
#
# Shown only when there is no site yet, so it never nags an established setup.
# Pure: takes the facts, returns lines. Tested in test-website-setup-notice.R.
# =============================================================

# "https://github.com/taro/beescabr(.git)" or "git@github.com:taro/beescabr.git"
# -> list(owner = "taro", repo = "beescabr"), or NULL when it is not a GitHub remote.
# the repository this project came from -- a clone or a fork starts pointing here,
# and pushing to it needs write access nobody inheriting the project will have.
UPSTREAM_REPO <- "RandomBirdLover/beescabr"

.parse_github_remote <- function(remote) {
  if (is.null(remote) || !nzchar(trimws(remote))) return(NULL)
  r <- sub("\\.git$", "", trimws(remote))
  m <- regmatches(r, regexec("github\\.com[:/]([^/]+)/([^/]+)$", r))[[1]]
  if (length(m) != 3) return(NULL)
  list(owner = m[2], repo = m[3])
}

# has_site: does docs/ already hold a built index.html?
# remote:   the URL of `origin`, or "" when there is none.
website_setup_notice <- function(has_site, remote) {
  if (isTRUE(has_site)) return(NULL)                 # an established site needs no lecture
  gh <- .parse_github_remote(remote)

  head <- c(
    "",
    "-------------------------------------------------------------------",
    "FIRST BUILD OF THE SITE -- two things GitHub needs from you",
    "-------------------------------------------------------------------",
    "",
    "The pages are being written into docs/. That is all this command does.",
    "Publishing them needs two settings that live on GitHub, not in this",
    "repository, so they do NOT come with a clone.")

  if (is.null(gh)) {
    body <- c(
      "",
      "1. There is no git remote called 'origin' -- or it is not a GitHub one.",
      "   Nothing can be deployed until this repository has a GitHub home.",
      "   Run this in the R console (RStudio), replacing <you> and <repo>:",
      "",
      "     system(\"git remote add origin https://github.com/<you>/<repo>\")",
      "",
      "2. Once it does, turn on Pages:",
      "     Settings -> Pages -> Build and deployment",
      "     Source: Deploy from a branch",
      "     Branch: main    Folder: /docs    -> Save")
  } else {
    site <- sprintf("https://%s.github.io/%s", tolower(gh$owner), gh$repo)
    is_upstream <- identical(tolower(paste0(gh$owner, "/", gh$repo)), tolower(UPSTREAM_REPO))
    step1 <- if (is_upstream) c(
      "",
      sprintf("1. origin still points at  %s  -- the ORIGINAL.", UPSTREAM_REPO),
      "   You cannot publish to it without write access. Fork it to your own",
      "   account on github.com, then point origin at your fork. In the R",
      "   console (RStudio), replacing <you> with your GitHub username:",
      "",
      "     system(\"git remote set-url origin https://github.com/<you>/beescabr\")"
    ) else c(
      "",
      sprintf("1. A deploy would push to  %s/%s,", gh$owner, gh$repo),
      sprintf("   forked from %s. That looks like your own copy,", UPSTREAM_REPO),
      "   so there is nothing to change here.")
    body <- c(step1,
      "",
      "2. Turn on Pages for that repository, on github.com:",
      "     Settings -> Pages -> Build and deployment",
      "     Source: Deploy from a branch",
      "     Branch: main    Folder: /docs    -> Save",
      "",
      sprintf("   The site then appears at  %s",
              if (is_upstream) "https://<you>.github.io/beescabr" else site))
  }

  c(head, body,
    "",
    "Until Pages is on, the pages exist only on this machine.",
    "-------------------------------------------------------------------",
    "")
}
