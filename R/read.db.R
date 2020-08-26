#library(RODBC)

read.custinfo <- function (odbc.dsn, abonnenter) {
  conn <- RODBC::odbcConnect(odbc.dsn)

  e_navn <- character(0)
  kaede <- character(0)

  for (abonnent in abonnenter) {
    sql <- paste(
      "select e_navn, kaede from abinfo where abonnent =",
      abonnent)
    data <- RODBC::sqlQuery(conn, sql, as.is=T)
    e_navn <- c(e_navn, data$e_navn)
    kaede <- c(kaede, data$kaede)
  }
  close(conn)
  e_navn <- enc2utf8(trimws(e_navn))
  kaede <- as.integer(kaede)
  result <- data.frame(e_navn, kaede)
  rownames(result) <- abonnenter

  return(result)
}

read.signalog <- function (odbc.dsn, abonnent, from.date=NULL, to.date=NULL) {

  conn <- RODBC::odbcConnect(odbc.dsn)

  sql <- paste(
    "select s_elem, sig_dato, hnd_type, al_kort, gruppenr, tekst_1",
    "from signalog where abonnent =", abonnent)
  if (!is.null(from.date)) {
    sql <- paste0(sql, " and sig_dato >= '", from.date, "'")
  }
  if (!is.null(to.date)) {
    sql <- paste0(sql, " and sig_dato < '", to.date, "'")
  }

  data <- RODBC::sqlQuery(conn, sql, as.is=T)
  data$sig_dato <- sub(":", " ", data$sig_dato)
  data$al_kort <- trimws(data$al_kort)

  close(conn)
  return(data)
}



