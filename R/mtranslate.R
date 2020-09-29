pkg.env <- new.env()

set.ms.translator.api.key <- function(x) {
  if (is.null(pkg.env$ms.translator.api.key)) {
    pkg.env$ms.translator.api.key <- x
  }
}

get.ms.translator.api.key <- function() {
  pkg.env$ms.translator.api.key
}

# escape backslash and double quotes
to.json.string <- function(x) {
  gsubv(c("\\", "\""),
        c("\\\"", "\\\""),
        x,
        fixed=T)
}

# machine translate norwegian input to output.lang
mtranslate <- function (x, output.lang) {
  x <- enc2utf8(x)
  if (output.lang == "no") { return(x) }
  if (output.lang == "se") { output.lang <- "sv" }
  url <- paste0(
    "https://api.cognitive.microsofttranslator.com/translate?api-version=3.0",
    "&from=", "no",
    "&to=", output.lang)
  request.body <- paste0('[{"text":"',
                         to.json.string(x),
                         '"}]')
  reply <- httr::POST(url,
                     httr::add_headers(
                       "Ocp-Apim-Subscription-Key"=get.ms.translator.api.key(),
                       "content-Type"="application/json; charset=UTF-8"
                     ),
                     body=request.body)
  if (reply$status_code != 200) {
    warning(paste0("Machine translation failed with status code: ",
                     reply$status_code))
    return(x)
  }
  return(httr::content(reply)[[1]][[1]][[1]]$text)
}
