library(trees3D)

make_label <- testthat:::make_label

expect_not_identical <- function (object, expected, info = NULL, label = NULL, expected.label = NULL)
{
    lab_act <- make_label(object, label)
    lab_exp <- make_label(expected, expected.label)
    ident <- identical(object, expected)
    expect(!ident, sprintf("%s identical to %s.\n", lab_act,
        lab_exp), info = info)
    invisible(object)
}

expect_not_error <- function (object, regexp = NULL, ..., info = NULL, label = NULL)
{
    lab <- make_label(object, label)
    error <- tryCatch({
        object
        NULL
    }, error = function(e) {
        e
    })
   if (identical(regexp, NA)) {
        expect(!is.null(error), sprintf("%s did not throw an error.\n%s",
            lab, error$message), info = info)
    }
    else if (is.null(regexp) || is.null(error)) {
        expect(is.null(error), sprintf("%s did throw an error.",
            lab), info = info)
    }
    else {
        expect_match(error$message, regexp, ..., info = info)
    }
    invisible(NULL)
}

expect_not_warning <- function (object, regexp = NULL, ..., all = FALSE, info = NULL,
    label = NULL)
{
    lab <- make_label(object, label)
    warnings <- capture_warnings(object)
    n <- length(warnings)
    bullets <- paste("* ", warnings, collapse = "\n")
    msg <- sprintf(ngettext(n, "%d warning", "%d warnings"),
        n)
    if (identical(regexp, NA)) {
        expect(length(warnings) == 0, sprintf("%s showed %s.\n%s",
            lab, msg, bullets), info = info)
    }
    else if (is.null(regexp) || length(warnings) == 0) {
        expect(length(warnings) == 0, sprintf("%s showed %s.\n%s",
            lab, msg, bullets), info = info)
    }
    else {
        expect_match(warnings, regexp, all = all, ..., info = info)
    }
    invisible(NULL)
}


expect_isin <- function(object, expected, ..., info = NULL, label = NULL,
                         expected.label = NULL, na.rm=TRUE) {

  if(na.rm)
    object <- object[!is.na(object)]
  i <- object %in% expected

  comp <- compare(all(i), TRUE, ...)
  expect(
    comp$equal,
    sprintf("%s - should not contain: %s", info, paste(object[!i], collapse= ", "))
  )

  invisible(object)
}

expect_contains <- function(object, expected, ..., info = NULL, label = NULL,
                         expected.label = NULL) {

 i <- expected %in% object

  comp <- compare(all(i), TRUE, ...)
  expect(
    comp$equal,
    sprintf("%s - does not contain: %s", info, paste(expected[!i], collapse= ", "))
  )

  invisible(object)
}

expect_not_NA <- function (object, info = NULL, label = NULL) {
    lab <- make_label(object, label)
    i <- !is.na(object)
    comp <- compare(all(i), TRUE)
    expect(comp$equal,
            sprintf("%s - contains NAs: %s", info, lab))
    invisible(object)
}
