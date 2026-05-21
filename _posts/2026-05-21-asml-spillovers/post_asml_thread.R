# Post the ASML spillover thread to Bluesky.
#
# Usage (in RStudio):
#   1. Generate an app password at https://bsky.app/settings/app-passwords
#   2. In an R console (NOT in this file — your password should not be
#      checked in):
#        Sys.setenv(BSKY_HANDLE       = "mondpanther.bsky.social")
#        Sys.setenv(BSKY_APP_PASSWORD = "xxxx-xxxx-xxxx-xxxx")
#   3. Open this file and source() it. Default is DRY RUN — it prints
#      what would be posted and confirms image sizes are OK.
#   4. To actually publish, after a happy dry run:
#        Sys.setenv(BSKY_PUBLISH = "1"); source("post_asml_thread.R")
#
# Requires: httr2, jsonlite. Auto-downscales images larger than ~900KB
# using `magick` if installed (Bluesky's blob limit is 1MB).

# ---- packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  if (!requireNamespace("httr2",    quietly = TRUE)) stop("install.packages('httr2')")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("install.packages('jsonlite')")
  library(httr2); library(jsonlite)
})

handle <- Sys.getenv("BSKY_HANDLE",       unset = "")
appkey <- Sys.getenv("BSKY_APP_PASSWORD", unset = "")
if (!nzchar(handle) || !nzchar(appkey))
  stop("Set BSKY_HANDLE and BSKY_APP_PASSWORD with Sys.setenv() first.")

publish <- toupper(Sys.getenv("BSKY_PUBLISH", "")) %in% c("1", "TRUE", "T", "YES")

PDS  <- "https://bsky.social"   # PDS endpoint; bsky.social is the main one
img_dir <- dirname(sys.frame(1)$ofile %||% "")
`%||%` <- function(a, b) if (is.null(a) || !length(a) || !nzchar(a)) b else a
if (!nzchar(img_dir)) img_dir <- "."  # if not sourced from file, use cwd

# ---- helpers ----------------------------------------------------------------

# Find http(s) URLs and produce AT-Protocol "link" facets so they render
# as clickable links in the Bluesky client. Byte offsets, not char offsets.
link_facets <- function(text) {
  m <- gregexpr("https?://[^\\s)<>]+", text, perl = TRUE)[[1]]
  if (length(m) == 0 || m[1] == -1) return(list())
  out <- vector("list", length(m))
  for (i in seq_along(m)) {
    s_chr <- m[i]; l_chr <- attr(m, "match.length")[i]
    pre   <- substr(text, 1, s_chr - 1L)
    word  <- substr(text, s_chr, s_chr + l_chr - 1L)
    s_byte <- nchar(pre,  type = "bytes")
    l_byte <- nchar(word, type = "bytes")
    out[[i]] <- list(
      index    = list(byteStart = s_byte,
                       byteEnd   = s_byte + l_byte),
      features = list(list(`$type` = "app.bsky.richtext.facet#link",
                            uri    = word))
    )
  }
  out
}

mime_for <- function(p) {
  switch(tolower(tools::file_ext(p)),
    png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
    webp = "image/webp", gif = "image/gif",
    "application/octet-stream")
}

# Bluesky blob limit is 1 MB. Try to recompress / resize down to <=900KB
# using magick if available; otherwise warn and let upload fail with a
# clear message.
downsize_if_needed <- function(path, max_bytes = 900000L) {
  if (file.size(path) <= max_bytes) return(path)
  if (!requireNamespace("magick", quietly = TRUE))
    stop(sprintf(
      "%s is %.2f MB; Bluesky's blob limit is 1 MB. ",
      basename(path), file.size(path) / 1e6),
      "Install the 'magick' package to auto-shrink, or compress manually.")
  img <- magick::image_read(path)
  for (q in c(90, 80, 70, 60, 50)) {
    out <- tempfile(fileext = ".jpg")
    magick::image_write(img, out, format = "jpeg", quality = q)
    if (file.size(out) <= max_bytes) return(out)
  }
  for (w in c(1600, 1200, 900, 700, 500)) {
    img2 <- magick::image_resize(img, paste0(w, "x"))
    out  <- tempfile(fileext = ".jpg")
    magick::image_write(img2, out, format = "jpeg", quality = 80)
    if (file.size(out) <= max_bytes) return(out)
  }
  stop(sprintf("Could not shrink %s below %s bytes.",
               basename(path), max_bytes))
}

# ---- session ----------------------------------------------------------------

new_session <- function() {
  request(paste0(PDS, "/xrpc/com.atproto.server.createSession")) |>
    req_method("POST") |>
    req_body_json(list(identifier = handle, password = appkey)) |>
    req_perform() |>
    resp_body_json()
}

# httr2's `req_body_raw()` sets the request Content-Type itself — passing
# it via `req_headers()` separately gets clobbered, which is what caused
# the previous HTTP 400. Always send the mime type as the 2nd arg here.
# `req_error(body = ...)` surfaces the API's actual error message so a
# future failure isn't just "400 Bad Request".
upload_blob <- function(sess, path) {
  ds  <- downsize_if_needed(path)
  raw <- readBin(ds, what = "raw", n = file.size(ds))
  resp <- request(paste0(PDS, "/xrpc/com.atproto.repo.uploadBlob")) |>
    req_method("POST") |>
    req_headers(Authorization = paste("Bearer", sess$accessJwt)) |>
    req_body_raw(raw, type = mime_for(ds)) |>
    req_error(body = function(r) {
      ct <- tryCatch(resp_content_type(r), error = function(e) "")
      if (grepl("json", ct)) {
        j <- resp_body_json(r)
        paste0(j$error %||% "", ": ", j$message %||% "")
      } else resp_body_string(r)
    }) |>
    req_perform() |>
    resp_body_json()
  resp$blob
}

# Delete a stale post by its at:// URI. Handy if a partial run left an
# orphaned first post you want to clean before re-running.
delete_post <- function(sess, at_uri) {
  rkey <- sub("^.*/", "", at_uri)
  request(paste0(PDS, "/xrpc/com.atproto.repo.deleteRecord")) |>
    req_method("POST") |>
    req_headers(Authorization = paste("Bearer", sess$accessJwt)) |>
    req_body_json(list(repo = sess$did,
                        collection = "app.bsky.feed.post",
                        rkey = rkey)) |>
    req_perform()
  invisible(at_uri)
}

# Replace the most recent post whose text contains `match_substring`:
# delete the existing one, then re-post `new_text` (optionally with
# images) into the SAME thread slot by reusing the deleted post's
# reply.root / reply.parent. Saves you from nuking and rebuilding the
# whole thread when one post needs a tiny edit.
replace_recent_post <- function(sess, match_substring, new_text,
                                 image_paths = character(0),
                                 image_alts  = character(0),
                                 lookback = 50L) {
  feed <- request(paste0(PDS, "/xrpc/app.bsky.feed.getAuthorFeed")) |>
    req_url_query(actor = sess$did, limit = lookback) |>
    req_headers(Authorization = paste("Bearer", sess$accessJwt)) |>
    req_perform() |>
    resp_body_json()

  hit <- NULL
  for (entry in feed$feed) {
    txt <- entry$post$record$text %||% ""
    if (grepl(match_substring, txt, fixed = TRUE)) { hit <- entry; break }
  }
  if (is.null(hit))
    stop("No recent post matching '", match_substring,
         "' in last ", lookback, " feed entries.")

  cat("Match found:\n  URI :", hit$post$uri, "\n")
  cat("  Text:", substr(hit$post$record$text, 1, 80),
      if (nchar(hit$post$record$text) > 80) "..." else "", "\n")

  reply <- hit$post$record$reply   # may be NULL for the thread root

  cat("Deleting...\n")
  delete_post(sess, hit$post$uri)

  cat("Re-posting corrected version...\n")
  res <- create_post(sess, new_text,
                      image_paths = image_paths,
                      image_alts  = image_alts,
                      root   = if (!is.null(reply)) reply$root   else NULL,
                      parent = if (!is.null(reply)) reply$parent else NULL)
  cat("New URI:", res$uri, "\n")
  invisible(res)
}

create_post <- function(sess, text, image_paths = character(0),
                         image_alts = character(0),
                         root = NULL, parent = NULL) {
  if (nchar(text) > 300L)
    warning(sprintf("Post is %d chars (>300) — Bluesky may reject.",
                    nchar(text)))
  facets <- link_facets(text)
  record <- list(
    `$type`    = "app.bsky.feed.post",
    text       = text,
    createdAt  = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    langs      = list("en")
  )
  if (length(facets) > 0) record$facets <- facets
  if (length(image_paths) > 0) {
    if (length(image_alts) == 0) image_alts <- rep("", length(image_paths))
    # `lapply` (rather than `mapply`) keeps the result UNNAMED — critical,
    # because jsonlite turns named lists into JSON objects but the bsky
    # API expects `images` to be a JSON array. Naming on the outer list
    # was the cause of the createRecord HTTP 400.
    images <- lapply(seq_along(image_paths), function(i) {
      blob <- upload_blob(sess, image_paths[[i]])
      list(alt = image_alts[[i]], image = blob)
    })
    record$embed <- list(`$type` = "app.bsky.embed.images",
                          images  = images)
  }
  if (!is.null(root) && !is.null(parent)) {
    record$reply <- list(root = root, parent = parent)
  }
  resp <- request(paste0(PDS, "/xrpc/com.atproto.repo.createRecord")) |>
    req_method("POST") |>
    req_headers(Authorization = paste("Bearer", sess$accessJwt)) |>
    req_body_json(list(
      repo       = sess$did,
      collection = "app.bsky.feed.post",
      record     = record
    )) |>
    req_error(body = function(r) {
      ct <- tryCatch(resp_content_type(r), error = function(e) "")
      if (grepl("json", ct)) {
        j <- resp_body_json(r)
        paste0(j$error %||% "", ": ", j$message %||% "")
      } else resp_body_string(r)
    }) |>
    req_perform() |>
    resp_body_json()
  list(uri = resp$uri, cid = resp$cid)
}

# ---- thread definition ------------------------------------------------------
# Edit these blocks freely before publishing. Each `text` should be <=300
# chars (the script warns otherwise). Each post may attach up to 4 images.
# `alts` is the alt text for accessibility — please fill in.

img <- function(name) file.path(img_dir, name)

thread <- list(
  list(
    text = paste0(
      "The Works in Progress piece on ASML is fascinating — how a tiny ",
      "Eindhoven spin-out came to dominate one of the most consequential ",
      "technologies of our time.\n\n",
      "But beyond the chip-machines, ASML is also a powerful generator of ",
      "*knowledge spillovers*.\n\n",
      "A thread 🧵"),
    images = character(0),
    alts   = character(0)
  ),
  list(
    text = paste0(
      "An average ASML patent generates ~$16M in global spillover value. ",
      "That's well above the typical Dutch patent.\n\n",
      "But still well below the average Qualcomm patent involving Dutch ",
      "inventors."),
    images = img("asml-global-spillovers.png"),
    alts   = "Bar chart: average global knowledge spillovers per patent for Dutch firms; Qualcomm leads, ASML mid-pack."
  ),
  list(
    text = paste0(
      "Now restrict spillover benefits to *other Dutch innovators only*.\n\n",
      "The ranking flips. ASML — alongside neighbouring Eindhoven firms ",
      "ASM International and Philips — dominates."),
    images = img("asml-national-spillovers.png"),
    alts   = "Bar chart: average Dutch-only spillovers per patent; ASML, ASM International and Philips at the top."
  ),
  list(
    text = paste0(
      "Why?\n\n",
      "Successful industrial strategy?\n",
      "Greater commitment to a local innovation ecosystem?\n",
      "A more effective revolving door of local engineers and scientists?\n\n",
      "Hard to tell. But you can see it from \"space\" using HiGGlo, the ",
      "friendly citation-network viewer."),
    images = character(0),
    alts   = character(0)
  ),
  list(
    text = paste0(
      "Qualcomm's citation network reaches across the globe — a long, dense ",
      "ladder of direct and indirect spillovers crossing continents."),
    images = img("citenet-qualcomm.png"),
    alts   = "World map: Qualcomm's globally distributed direct and indirect citation links."
  ),
  list(
    text = paste0(
      "ASML's looks very different: a tight cluster inside the Netherlands, ",
      "many links indirect."),
    images = img("citenet-asml.png"),
    alts   = "World map: ASML's citation links cluster densely inside the Netherlands."
  ),
  list(
    text = paste0(
      "Full write-up with interactive charts and HiGGlo deep-links:\n",
      "https://mondpanther.github.io/wwwmondpanther/posts/2026-05-21-asml-spillovers/\n\n",
      "Tools:\n",
      "• ISE → http://www.prinzproject.io/iseapp\n",
      "• HiGGlo → https://tinyurl.com/higglo"),
    images = character(0),
    alts   = character(0)
  )
)

# ---- dry run / publish ------------------------------------------------------

cat("\n===== ASML Bluesky thread =====\n")
cat("Handle :", handle, "\n")
cat("Posts  :", length(thread), "\n")
cat("Mode   :", if (publish) "PUBLISH" else "DRY RUN", "\n\n")

for (i in seq_along(thread)) {
  p <- thread[[i]]
  cat(sprintf("--- Post %d/%d  (%d chars) ---\n",
              i, length(thread), nchar(p$text)))
  cat(p$text, "\n")
  if (length(p$images) > 0) {
    for (j in seq_along(p$images)) {
      sz <- if (file.exists(p$images[j])) {
        sprintf("%.2f MB", file.size(p$images[j]) / 1e6)
      } else "MISSING"
      cat(sprintf("  [img%d] %s (%s) — alt: %s\n",
                  j, basename(p$images[j]), sz, p$alts[j]))
    }
  }
  cat("\n")
}

if (!publish) {
  cat("Dry run complete. To publish:\n")
  cat('  Sys.setenv(BSKY_PUBLISH = "1"); source("post_asml_thread.R")\n')
} else {
  cat("Logging in...\n")
  sess <- new_session()
  cat("OK; did =", sess$did, "\n\n")

  root   <- NULL
  parent <- NULL
  for (i in seq_along(thread)) {
    p <- thread[[i]]
    cat(sprintf("Posting %d/%d ...\n", i, length(thread)))
    res <- create_post(sess, p$text,
                        image_paths = p$images,
                        image_alts  = p$alts,
                        root = root, parent = parent)
    cat("  ->", res$uri, "\n")
    if (is.null(root)) root <- res
    parent <- res
  }
  cat("\nThread published ✓\n")
  cat("View at: https://bsky.app/profile/", handle, "\n", sep = "")
}
