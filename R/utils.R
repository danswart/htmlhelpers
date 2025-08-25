# ============================================================================
# File: R/utils.R (Internal helper functions)
# ============================================================================

#' Validate file exists and is correct type
#' @noRd
validate_file <- function(file_path, type = NULL) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string", call. = FALSE)
  }

  if (!file.exists(file_path)) {
    stop(sprintf("File not found: '%s'", file_path), call. = FALSE)
  }

  if (!is.null(type)) {
    ext <- tolower(tools::file_ext(file_path))
    valid_exts <- get_valid_extensions(type)

    if (!ext %in% valid_exts) {
      stop(sprintf(
        "Invalid %s file extension: '.%s'\nSupported: %s",
        type, ext, paste(valid_exts, collapse = ", ")
      ), call. = FALSE)
    }
  }
}

#' Get valid file extensions for each type
#' @noRd
get_valid_extensions <- function(type) {
  switch(type,
         video = c("mp4", "mov", "webm", "ogg", "ogv", "avi", "m4v"),
         image = c("png", "jpg", "jpeg", "gif", "svg", "webp", "bmp"),
         audio = c("mp3", "wav", "ogg", "m4a", "flac"),
         pdf = c("pdf"),
         c()
  )
}

#' Get MIME type from file extension
#' @noRd
get_mime_type <- function(file_path, category = NULL) {
  ext <- tolower(tools::file_ext(file_path))

  mime_types <- list(
    # Video
    mp4 = "video/mp4",
    mov = "video/quicktime",
    webm = "video/webm",
    ogg = if (category == "video") "video/ogg" else "audio/ogg",
    ogv = "video/ogg",
    avi = "video/x-msvideo",
    m4v = "video/mp4",
    # Image
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    webp = "image/webp",
    bmp = "image/bmp",
    # Audio
    mp3 = "audio/mpeg",
    wav = "audio/wav",
    m4a = "audio/mp4",
    flac = "audio/flac",
    # Document
    pdf = "application/pdf"
  )

  return(mime_types[[ext]])
}

#' Build HTML attributes string
#' @noRd
build_attributes <- function(...) {
  args <- list(...)
  args <- args[!sapply(args, is.null)]

  if (length(args) == 0) return("")

  attrs <- mapply(function(name, value) {
    if (is.logical(value)) {
      if (value) name else NULL
    } else {
      sprintf('%s="%s"', name, as.character(value))
    }
  }, names(args), args, SIMPLIFY = FALSE)

  attrs <- Filter(Negate(is.null), attrs)
  paste(attrs, collapse = " ")
}

#' Build style string
#' @noRd
build_style <- function(width = NULL, height = NULL, custom = NULL) {
  styles <- c()

  if (!is.null(width)) styles <- c(styles, paste0("width:", width))
  if (!is.null(height)) styles <- c(styles, paste0("height:", height))
  if (!is.null(custom)) styles <- c(styles, custom)

  paste(styles, collapse = ";")
}

#' Output HTML or print code
#' @noRd
output_html <- function(html_code, copy_paste = FALSE) {
  if (copy_paste) {
    cat(html_code, "\n")
    invisible(NULL)
  } else {
    htmltools::HTML(html_code)
  }
}

