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
    sql <- paste0(sql,
                  " and sig_dato >=",
                  db.singlequote.format.date(from.date))
  }
  if (!is.null(to.date)) {
    sql <- paste0(sql,
                  " and sig_dato <",
                  db.singlequote.format.date(to.date))
  }

  signalog <- RODBC::sqlQuery(conn, sql, as.is=T)
  signalog$sig_dato <- sub(":", " ", signalog$sig_dato)
  signalog$al_kort <- trimws(signalog$al_kort)

  close(conn)

  return(signalog)
}

manual.processings.pr.month <- function(odbc.dsn, abonnent, date=Sys.time()) {
  month.start <- date <- as.POSIXlt(date)
  month.start$mday <- 1
  month.start$hour <- 0
  month.start$min <- 0
  month.start$sec <- 0
  sql <- paste(
    "select (select count(*) from signalog",
    "where abonnent =", abonnent,
    "and hnd_type = '10'",
    "and sig_dato >=", db.singlequote.format.date(month.start),
    "and sig_dato <", db.singlequote.format.date(date),
    ")-(select count(*) from signalog",
    "where abonnent =", abonnent,
    "and al_kort = 'VARSEL'",
    "and tekst_1 like '%Virtuell Vekterrunde%'",
    "and sig_dato >=", db.singlequote.format.date(month.start),
    "and sig_dato <", db.singlequote.format.date(date),
    ")")
  conn <- RODBC::odbcConnect(odbc.dsn)
  result <- RODBC::sqlQuery(conn, sql, as.is=T)
  close(conn)
  return(as.integer(result[1,1]))
}

db.singlequote.format.date <- function(date) {
  paste0("'",
         format(date, "%Y-%m-%d:%H:%M:%OS"),
         "'")
}
