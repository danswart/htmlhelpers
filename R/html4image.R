# ============================================================================
# File: R/html4image.R
# ============================================================================

#' Embed Image Files in HTML Documents
#'
#' Converts image files (PNG, JPG, GIF, SVG, WebP) into HTML img elements
#' for embedding in Quarto documents, R Markdown, or other HTML outputs.
#'
#' @param file_path Character string. Path to the image file.
#' @param width Character string or NULL. Width of the image.
#' @param height Character string or NULL. Height of the image.
#' @param alt Character string. Alternative text for accessibility (important!).
#' @param caption Character string or NULL. Caption to display below image.
#' @param link Character string or NULL. URL to link the image to.
#' @param id Character string or NULL. HTML id attribute.
#' @param class Character string or NULL. HTML class attribute.
#' @param style Character string or NULL. Additional inline CSS.
#' @param align Character string. Alignment: "center" (default), "left", "right".
#' @param copy_paste Logical. If TRUE, prints raw HTML code to console.
#'
#' @return HTML object for embedding, or NULL if copy_paste=TRUE.
#' @export
#' @examples
#' \dontrun{
#' sample_gif <- system.file("extdata", "images", "sample.gif",
#' #' package = "htmlhelpers")
#' html4image(sample_gif, alt = "Sample animation")
#'
#' # Basic usage with alt text
#' html4image("images/plot.png", alt = "Scatter plot of results")
#'
#' # With caption and link
#' html4image("images/photo.jpg",
#'            alt = "Team photo",
#'            caption = "Our team in 2024",
#'            link = "https://example.com")
#'
#' }
html4image <- function(file_path,
                       width = NULL,
                       height = NULL,
                       alt = "",
                       caption = NULL,
                       link = NULL,
                       id = NULL,
                       class = NULL,
                       style = NULL,
                       align = "center",
                       copy_paste = FALSE) {

  # Validate file
  validate_file(file_path, type = "image")

  # Warn if no alt text
  if (alt == "") {
    warning("Consider adding alt text for accessibility", call. = FALSE)
  }

  # Build attributes
  attrs <- build_attributes(
    src = file_path,
    alt = alt,
    id = id,
    class = class
  )

  # Build style
  img_style <- build_style(
    width = width,
    height = height,
    custom = style
  )

  # Container alignment
  container_style <- switch(align,
                            left = "text-align:left;",
                            right = "text-align:right;",
                            center = "text-align:center;",
                            "text-align:center;"
  )

  # Build image tag
  img_tag <- sprintf('<img %s style="%s">', attrs, img_style)

  # Wrap in link if provided
  if (!is.null(link)) {
    img_tag <- sprintf('<a href="%s">%s</a>', link, img_tag)
  }

  # Add caption if provided
  if (!is.null(caption)) {
    html_code <- sprintf(
      '<figure style="%s">
      %s
      <figcaption>%s</figcaption>
    </figure>',
      container_style,
      img_tag,
      caption
    )
  } else {
    html_code <- sprintf('<div style="%s">%s</div>', container_style, img_tag)
  }

  # Clean up
  html_code <- gsub("\\s+", " ", html_code)

  return(output_html(html_code, copy_paste))
}
