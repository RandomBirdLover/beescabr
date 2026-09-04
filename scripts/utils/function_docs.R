# =============================================================
# utils/function_docs.R -- find the project's API and document it.
# A function called from a file other than its own is API: somebody else has to
# know what to pass it. This module locates that set, reads the roxygen block
# above each one, and renders dev-docs/FUNCTIONS.md from what it finds.
#
# Generated, not hand-kept, on purpose -- the ANALYSIS_DECISIONS master table
# drifted to half-true because a human had to remember to update it.
# Run it: source("scripts/utils/function_docs.R"); write_functions_md()
# Enforced by tests/testthat/test-function-docs.R.
# =============================================================
if (!exists("beescabr_require")) source("scripts/config.R")

# Regex for a top-level definition: NAME <- function / NAME = function.
.DEF_RE <- "^([A-Za-z_.][A-Za-z0-9_.]*)[[:space:]]*(<-|=)[[:space:]]*function"

#' Every top-level function definition under a scripts directory
#'
#' @param dir Path to the scripts directory to scan.
#' @return A data frame with `name`, `file` (relative to `dir`) and `line`.
r_function_defs <- function(dir) {
  files <- list.files(dir, pattern = "[.]R$", recursive = TRUE)
  out <- lapply(files, function(f) {
    lines <- readLines(file.path(dir, f), warn = FALSE)
    hit <- grep(.DEF_RE, lines)
    if (!length(hit)) return(NULL)
    data.frame(name = sub(paste0(.DEF_RE, ".*$"), "\\1", lines[hit]),
               file = f, line = hit, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' The subset of functions that are called from a file other than their own
#'
#' Comments are stripped before searching, so a function named only in prose
#' does not count as a caller. `main()` is excluded: every stage script defines
#' one and the runner calls it, which says nothing about a shared API.
#'
#' @param dir Path to the scripts directory to scan.
#' @return `r_function_defs()` filtered to cross-file callees, with `documented`
#'   (has a roxygen block), `params_complete` (every argument has an `@param`),
#'   `title` (first roxygen line) and `params` (argument names) added.
r_api_functions <- function(dir) {
  defs <- r_function_defs(dir)
  defs <- defs[defs$name != "main", , drop = FALSE]
  # A leading dot is R's private marker -- .tid_of() is a helper of the file it
  # lives in, not something another script is meant to call.
  defs <- defs[!grepl("^[.]", defs$name), , drop = FALSE]
  # A name defined in two files is a local convention (several scripts each write
  # their own read_prep()), not one shared function. Counting those as cross-file
  # turns every duplicate into a false API entry, so require a single definition.
  defs <- defs[defs$name %in% names(which(table(defs$name) == 1L)), , drop = FALSE]
  files <- list.files(dir, pattern = "[.]R$", recursive = TRUE)
  bodies <- lapply(files, function(f)
    paste(sub("#.*$", "", readLines(file.path(dir, f), warn = FALSE)), collapse = "\n"))
  names(bodies) <- files

  keep <- vapply(seq_len(nrow(defs)), function(i) {
    pat <- paste0("(?<![\\w.$])", gsub("([.])", "\\\\\\1", defs$name[i]), "[[:space:]]*\\(")
    others <- setdiff(files, defs$file[i])
    any(vapply(bodies[others], function(b) grepl(pat, b, perl = TRUE), logical(1)))
  }, logical(1))
  defs <- defs[keep, , drop = FALSE]
  if (!nrow(defs)) return(cbind(defs, documented = logical(0)))

  docs <- lapply(seq_len(nrow(defs)), function(i)
    .read_def(file.path(dir, defs$file[i]), defs$line[i]))
  defs$title  <- vapply(docs, function(d) d$title, character(1))
  defs$params <- I(lapply(docs, function(d) d$params))
  defs$tags   <- I(lapply(docs, function(d) d$tags))
  defs$documented <- vapply(docs, function(d) d$documented, logical(1))
  defs$roxygen <- vapply(docs, function(d) d$roxygen, logical(1))
  defs$ret <- vapply(docs, function(d) d$ret, character(1))
  defs$params_complete <- vapply(docs, function(d)
    all(d$params %in% names(d$tags)), logical(1))
  defs[order(defs$file, defs$name), , drop = FALSE]
}

# Read one definition: its roxygen block (if any) and its argument names.
.read_def <- function(path, at) {
  lines <- readLines(path, warn = FALSE)

  # roxygen block = the unbroken run of #' lines immediately above the definition
  i <- at - 1L
  block <- character(0)
  while (i >= 1L && grepl("^[[:space:]]*#'", lines[i])) {
    block <- c(sub("^[[:space:]]*#'[[:space:]]?", "", lines[i]), block); i <- i - 1L
  }
  roxy <- length(block) > 0

  # Fall back to whatever plain comment is already there -- 156 of these functions
  # carry a useful one-liner written before roxygen was the convention, and a real
  # sentence from 2025 beats an empty cell while the blocks get converted. Prefer
  # the trailing comment on the definition line, then the run of # lines above it.
  if (!roxy) {
    tail_cmt <- sub("^[^#]*#+[[:space:]]*", "", lines[at])
    if (grepl("#", lines[at]) && nzchar(trimws(tail_cmt))) {
      block <- trimws(tail_cmt)
    } else {
      i <- at - 1L
      while (i >= 1L && grepl("^[[:space:]]*#", lines[i]) &&
             !grepl("^[[:space:]]*#[[:space:]]*[-=]{3,}", lines[i])) {
        block <- c(trimws(sub("^[[:space:]]*#+[[:space:]]?", "", lines[i])), block)
        i <- i - 1L
      }
      block <- block[nzchar(block)]
    }
  }

  # argument names: read the signature, which may wrap over several lines
  sig <- paste(lines[at:min(length(lines), at + 20L)], collapse = " ")
  sig <- sub("^.*?function[[:space:]]*\\(", "", sig)
  sig <- .balanced_head(sig)
  args <- .split_top(sig)
  args <- trimws(sub("=.*$", "", args))
  args <- args[nzchar(args) & args != "..."]

  tags <- list()
  for (ln in block[grepl("^@param[[:space:]]", block)]) {
    p <- sub("^@param[[:space:]]+([^[:space:]]+).*$", "\\1", ln)
    tags[[p]] <- trimws(sub("^@param[[:space:]]+[^[:space:]]+", "", ln))
  }
  title <- if (length(block)) block[1] else ""
  ret <- block[grepl("^@return", block)]
  list(documented = length(block) > 0, roxygen = roxy, title = title,
       params = args, tags = tags,
       ret = if (length(ret)) trimws(sub("^@return[[:space:]]*", "", ret[1])) else "")
}

# Text up to the paren that closes the one we are already inside.
.balanced_head <- function(s) {
  ch <- strsplit(s, "")[[1]]; depth <- 0L
  for (i in seq_along(ch)) {
    if (ch[i] == "(") depth <- depth + 1L
    else if (ch[i] == ")") { if (depth == 0L) return(paste(ch[seq_len(i - 1L)], collapse = ""))
                             depth <- depth - 1L }
  }
  s
}

# Split on commas that are not inside nested parens/brackets -- a default value
# like `c(1, 2)` is one argument, not two.
.split_top <- function(s) {
  ch <- strsplit(s, "")[[1]]; depth <- 0L; cur <- character(0); out <- character(0)
  for (c1 in ch) {
    if (c1 %in% c("(", "[", "{")) depth <- depth + 1L
    else if (c1 %in% c(")", "]", "}")) depth <- depth - 1L
    if (c1 == "," && depth == 0L) { out <- c(out, paste(cur, collapse = "")); cur <- character(0) }
    else cur <- c(cur, c1)
  }
  c(out, paste(cur, collapse = ""))
}

#' Render the API reference as markdown
#'
#' @param dir Path to the scripts directory to scan.
#' @return A character vector of lines, ready to write to dev-docs/FUNCTIONS.md.
functions_md <- function(dir) {
  api <- r_api_functions(dir)
  out <- c(
    "# Functions",
    "",
    "Every function called from a file other than its own -- the project's API.",
    "A function used only inside one script is not here; read it in place.",
    "",
    sprintf("**%d functions across %d files.** Grouped by the folder that defines them.",
            nrow(api), length(unique(api$file))),
    "",
    "*Generated by `scripts/utils/function_docs.R`. Do not edit by hand --",
    "edit the `#'` block above the function and regenerate.*",
    "")

  api$folder <- ifelse(dirname(api$file) == ".", "scripts/", paste0("scripts/", dirname(api$file), "/"))
  for (fo in unique(api$folder)) {
    out <- c(out, "", paste0("## `", fo, "`"), "")
    sub <- api[api$folder == fo, , drop = FALSE]
    for (fl in unique(sub$file)) {
      out <- c(out, paste0("### ", basename(fl)), "",
               "| Function | What it does |", "|---|---|")
      s <- sub[sub$file == fl, , drop = FALSE]
      for (i in seq_len(nrow(s))) {
        args <- paste(s$params[[i]], collapse = ", ")
        out <- c(out, sprintf("| `%s(%s)` | %s |", s$name[i], args,
                              if (nzchar(s$title[i])) s$title[i] else "*undocumented*"))
      }
      out <- c(out, "")
    }
  }
  out
}

#' Write dev-docs/FUNCTIONS.md from the current source
#'
#' @param dir Path to the scripts directory to scan.
#' @param out Path of the markdown file to write.
#' @return `out`, invisibly.
write_functions_md <- function(dir = "scripts", out = "dev-docs/FUNCTIONS.md") {
  writeLines(functions_md(dir), out)
  invisible(out)
}
