`%notin%` <- Negate(`%in%`)

cat0 <- function(..., sep="") {
  return(cat(..., sep=sep))
}

# returns true if match is found
str_matches <- function(haystack, needle) {
  return(length(grep(needle, haystack))!=0)
}

# returns first parenthized match
str_match_inner <- function(haystack, paren_regexp) {
  return(trimws(stringr::str_match(haystack, paren_regexp)[2]))
}

gsubv <- function(pattern, replacement, x, ...) {
  if (length(pattern) != length(replacement)) {
    throw("gsubr(): length(pattern) != length(replacement")
  }
  for (i in seq_len(length(pattern))) {
    x <- gsub(pattern[i], replacement[i], x, ...)
  }
  return(x)
}

asciify.filename <- function(x) {
  fs::path_sanitize(
    gsubv(
      c(" ", ":", "[()]",
        "[æä]", "[øö]", "å", "ü",
        "[ÆÄ]", "[ØÖ]", "Å", "Ü",
        "[^A-Za-z0-9._-]"),
      c("_", ".", "",
        "ae", "oe", "aa", "u",
        "AE", "OE", "AA", "U",
        "x"),
      x),
    "x")
}

parse.date <- function(input) {
  result <- tryCatch({
    as.POSIXlt(input,
               tz="",
               tryformats = c("%Y-%m-%d %H:%M:%OS",
                              "%Y-%m-%d %H:%M",
                              "%Y-%m-%d"))
  }, warning=function(w) { stop("Incorrectly formatted date/time: ", input)
  }, error=function(w) { stop("Incorrectly formatted date/time: ", input)
  })
  return(result)
}
