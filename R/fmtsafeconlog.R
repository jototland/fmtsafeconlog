#library(yaml)

fmtsafeconlog <- function(odbc.dsn, config.file) {
  options(encoding="UTF-8")
  # Les data fra konfigurasjonfila
  config <- yaml::read_yaml(file=config.file)

  # Lag mappa vi lagrer forenklede logger i, hvis den ikke eksisterer
  output.dir <- config$outputDir
  if (!file.exists(output.dir)) {
    dir.create(output.dir, recursive=T)
  }

  # Valider dato/klokkeslett fra konfigurasjonsfila
  from.date <- parse.date(config$fromDate)
  to.date <- parse.date(config$toDate)
  from.date.str <- ifelse(is.null(from.date),"", format(from.date, "%Y-%m-%d %H:%M:%OS"))
  to.date.str <- ifelse(is.null(to.date),"", format(to.date, "%Y-%m-%d %H:%M:%OS"))

  # Sjekk om vi skal sende mail
  send.mail <- tolower(config$sendMail)
  if (send.mail %in% c("j", "ja", "y", "yes,", "true")) {
    send.mail <- T
  } else if (send.mail %in% c("n", "nei", "no", "false")) {
    send.mail <-T
  } else { stop("Ugyldig verdi i konfigurasjonfil: send.mail")}

  # Sjekk om vi skal lagre i utboksen (standard), eller faktisk sende
  only.save.mail.draft <- T
  if (send.mail) {
    only.save.mail.draft <- tolower(config$onlySaveMailDraft)
    if (only.save.mail.draft %in% c("j", "ja", "y", "yes,", "true"))
      only.save.mail.draft <- T
    else if (only.save.mail.draft %in% c("n", "nei", "no", "false"))
      only.save.mail.draft <-T
    else { stop("Ugyldig verdi i konfigurasjonfil: only.save.mail.draft")}
  }

  # Lag data.frame med abonnent, e_navn, kaede fra safecon database
  customers <-
    read.custinfo(odbc.dsn,
                  sapply(config$customers, function(x) {x$abonnent}))

  # Legg til navn som kommer fra konfigurasjonsfile
  customers$navn <- sapply(config$customers, function(x) {x$navn})

  # Legg til språk, basert på kjedekode
  customers$lang <- "no"
  customers$lang[customers$kaede >= 5000 & customers$kaede < 5999] <- "se"

  # Sjekk at kundenavn i konfigurasjonsfil og safecon stemmer overens
  if (!all(tolower(customers$e_navn) == tolower(customers$navn))) {
    wrong.ones <- which(tolower(customers$e_navn) != tolower(customers$navn))
    errmsg <-
      paste0("Følgende kundenavn avviker fra safecon:\n",
             paste0(
               customers$navn[wrong.ones],
               " != ",
               customers$e_navn[wrong.ones],
               collapse="\n"))
    stop(errmsg)
  }

  # Lag passende filnavn
  customers$filename <-
    paste0(output.dir,
           "/",
           asciify.filename(
             paste0(rownames(customers), " ",
                    customers$navn, " ",
                    from.date.str ,"-", to.date.str,
                    ".txt")))

  # Gå gjenom hver kunde i konfigurasjonfila
  for (customer in config$customers) {
    abonnent <- as.character(customer$abonnent)

    # Sett språk til etter hva vi allerede har funnet ut
    lang <- customers[abonnent, "lang"]
    trl <- function(x) { translate(x, lang=lang) }

    # Les signalloggen fra safecon database
    siglog <- read.signalog(odbc.dsn=odbc.dsn,
                            abonnent=abonnent,
                            from.date=from.date,
                            to.date=to.date)

    # Lag en output-fil
    fd <- file(customers[abonnent, "filename"],
               open="wt", encoding="UTF-8")
    out <- function(...) { writeLines(paste0(...), con=fd) }

    # Lag en header som kan stå i toppen av fila og mailen
    header <-
      paste0(trl("Forenklet signalbehandlingslogg"), ":\n",
             trl("Safecon kundenr: "), abonnent, "\n",
             trl("Navn: "), customer$navn, "\n",
             trl("Dato fra og med: "), from.date.str, "\n",
             trl("Dato til: "), to.date.str)
    writeLines(header, con=fd)

    # Lagre nåværende posisjon, så vi vet hvor mye som er skrevet
    headerPos <- seek(fd, where=0, origin="current")

    # Skriv ut forenklet signallog
    write.signalog(siglog, fd)

    # Hvor lang ble den forenklede signalloggen?
    bytes.written <- seek(fd, where=0, origin="current") - headerPos

    # Mail og loggen i vedlegget formateres ulikt,
    # hvis vi har noe å melde eller ikke
    trailer <- ""
    if (bytes.written == 0) {
      trailer <- paste0("\n\n", trl("Ingen hendelser i tidsrommet"))
      writeLines(trailer, con=fd)
    } else {
      trailer <- paste0("\n\n", trl("Se vedlegg."))
      writeLines("\n", trl("Logg slutt."), con=fd)
    }

    # Lukk output-fila
    close(fd)

    # Sjekk om kunde vil ha mail, også ved ingen hendelser
    always.mail <- customer$alwaysMail
    if (is.null(always.mail)) {
      always.mail <- F
    } else if (always.mail %in% c(1, "1", "true", "yes", "y", "ja", "j")) {
      always.mail <- T
    } else {
      always.mail <- F
    }

    # Hvis vi skal sende mail, så gjør vi det nå
    if (send.mail && (bytes.written != 0 || always.mail)) {
      if (customer$addManualProcessingPrMonth) {
        manProc <- manual.processings.pr.month(odbc.dsn, customer$abonnent)
        trailer <- paste0(
          trailer, "\n\n",
          trl("Antall behandlinger hittil i måneden: "), manProc, "\n",
          trl("(dette bør være under 10)"))
      }
      send.outlook.email(to=customer$mailto,
                         cc=customer$mailcc,
                         bcc=customer$mailbcc,
                         onbehalf="customercenter@safe4.com",
                         subject=paste0("Hendelseslogg ", customer$navn,
                                        " fra ", from.date.str,
                                        " til ", to.date.str),
                         body=paste0(header, trailer, "\n\n",
                                    trl("Med vennlig hilsen Safe4"),
                                    "\n"),
                         attachments = customers[abonnent, "filename"],
                         onlySaveDraft=only.save.mail.draft)
    }
  }
}
