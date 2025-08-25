# ============================================================================
# File: R/html4pdf.R
# ============================================================================

#' Embed PDF Files as HTML in Documents
#'
#' Creates an iframe or object element to display PDF files in HTML documents.
#'
#' @param file_path Character string. Path to the PDF file.
#' @param width Character string. Width (default "100%").
#' @param height Character string. Height (default "600px").
#' @param method Character string. "iframe" (default) or "object".
#' @param fallback_text Character string. Text shown if PDF cannot be displayed.
#' @param id Character string or NULL. HTML id attribute.
#' @param class Character string or NULL. HTML class attribute.
#' @param style Character string or NULL. Additional inline CSS.
#' @param copy_paste Logical. If TRUE, prints raw HTML code to console.
#'
#' @return HTML object for embedding, or NULL if copy_paste=TRUE.
#' @export
html4pdf <- function(file_path,
                     width = "100%",
                     height = "600px",
                     method = "iframe",
                     fallback_text = "Your browser cannot display this PDF.",
                     id = NULL,
                     class = NULL,
                     style = NULL,
                     copy_paste = FALSE) {

  validate_file(file_path, type = "pdf")

  attrs <- build_attributes(id = id, class = class)
  pdf_style <- build_style(width = width, height = height, custom = style)

  if (method == "iframe") {
    html_code <- sprintf(
      '<iframe src="%s" %s style="%s">%s</iframe>',
      file_path, attrs, pdf_style, fallback_text
    )
  } else {
    html_code <- sprintf(
      '<object data="%s" type="application/pdf" %s style="%s">
        <p>%s <a href="%s">Download PDF</a></p>
      </object>',
      file_path, attrs, pdf_style, fallback_text, file_path
    )
  }

  return(output_html(html_code, copy_paste))
}

