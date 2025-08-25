# ============================================================================
# File: R/html4video.R
# ============================================================================

#' Embed Video Files in HTML Documents
#'
#' Converts video files (MP4, MOV, WebM, Ogg, AVI, M4V) into HTML5 video elements
#' for embedding in Quarto documents, R Markdown, or other HTML outputs.
#'
#' @param file_path Character string. Path to the video file.
#' @param width Character string. Width of the video (default "70%").
#' @param height Character string or NULL. Height of the video (default NULL).
#' @param autoplay Logical. Should the video start playing automatically? (default FALSE)
#' @param loop Logical. Should the video loop continuously? (default FALSE)
#' @param muted Logical. Should the video be muted? (default FALSE)
#' @param controls Logical. Should video controls be displayed? (default TRUE)
#' @param nodownload Logical. Should the download option be hidden? (default TRUE)
#' @param id Character string or NULL. HTML id attribute for the element.
#' @param class Character string or NULL. HTML class attribute for styling.
#' @param style Character string or NULL. Additional inline CSS styles.
#' @param align Character string. Alignment: "center" (default), "left", "right".
#' @param copy_paste Logical. If TRUE, prints raw HTML code to console.
#'
#' @return HTML object for embedding, or NULL if copy_paste=TRUE.
#' @export
#' @examples
#' \dontrun{
#' # Using package sample files
#' sample_video <- system.file("extdata", "media", "sample.mp4",
#'                             package = "htmlhelpers")
#' html4video(sample_video)
#'
#' # Using user's own files
#' html4video("my_own_video.mp4")
#'
#' # Custom dimensions and autoplay
#' html4video(sample_video, width = "100%", height = "400px",
#'            autoplay = TRUE, muted = TRUE)
#'
#' # Using copy_paste parameter for non-revealjs documents
#' html4video(sample_video, width = "100%", height = "400px",
#'             autoplay = TRUE, muted = TRUE, copy_paste = TRUE)
#'
#' }
html4video <- function(file_path,
                       width = "70%",
                       height = NULL,
                       autoplay = FALSE,
                       loop = FALSE,
                       muted = FALSE,
                       controls = TRUE,
                       nodownload = TRUE,
                       id = NULL,
                       class = NULL,
                       style = NULL,
                       align = "center",
                       copy_paste = FALSE) {

  # Use internal validator
  validate_file(file_path, type = "video")

  # Get MIME type
  mime_type <- get_mime_type(file_path, category = "video")

  # Build attributes
  attrs <- build_attributes(
    controls = controls,
    autoplay = autoplay,
    loop = loop,
    muted = muted,
    controlsList = if (nodownload && controls) "nodownload" else NULL,
    id = id,
    class = class
  )

  # Build style
  video_style <- build_style(
    width = width,
    height = height,
    custom = style
  )

  # Container alignment
  container_style <- switch(align,
                            left = "text-align:left;",
                            right = "text-align:right;",
                            center = "text-align:center;",
                            "text-align:center;"  # default
  )

  # Generate HTML
  html_code <- sprintf(
    '<div style="%s">
    <video %s style="%s">
      <source src="%s" type="%s">
      Your browser does not support the video tag.
    </video>
  </div>',
    container_style,
    attrs,
    video_style,
    file_path,
    mime_type
  )

  # Clean up extra spaces
  html_code <- gsub("\\s+", " ", html_code)

  return(output_html(html_code, copy_paste))
}
