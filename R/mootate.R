#' Mutate and restore variable labels for sweet_read and other labelled objects
#'
#' Works like dplyr::mutate(), but if the input object has class "sweet_read",
#' restores variable labels from the "var_labels" attribute after mutation using purrr::walk.
#' Otherwise, uses sjlabelled::copy_labels() to restore labels from the original object.
#'
#' @param .data A data frame (ideally of class "sweet_read")
#' @param ... Mutations (as in dplyr::mutate)
#' @return A data frame with labels restored.
#' @export
mootate <- function(.data, ...) {

  out <- dplyr::mutate(.data, ...)
  if (inherits(.data, "sweet_read")) {
    var_labels <- attr(.data, "var_labels")
    if (!is.null(var_labels)) {
      out <- purrr::imap(out, function(col, nm) {
        lbl <- var_labels[[nm]]
        if (!is.null(lbl)) attr(col, "label") <- lbl
        col
      })
      out <- as.data.frame(out, stringsAsFactors = FALSE)
      attr(out, "var_labels") <- var_labels
      class(out) <- unique(c("sweet_read", setdiff(class(out), "sweet_read")))
    }
  } else {
    # Use sjlabelled to restore labels for non-sweet_read objects
    if (requireNamespace("sjlabelled", quietly = TRUE)) {
      out <- sjlabelled::copy_labels(.data, out)
    }
  }
  out
}
