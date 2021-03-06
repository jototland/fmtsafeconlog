write.signalog <- function(siglog, con=stdout(), lang="no") {
  trl <- function(x) { translate(x, lang=lang) }
  out <- function(...) { writeLines(paste0(...), con=con) }

  previous_s_elem <- 0
  venteliste <- F
  venteliste.al_kort <- NULL
  venteliste.gruppenr <- NULL
  venteliste.sperr_angitte <- F
  venteliste.sperreliste <- NULL
  autobehandling <- F
  signalement <- F
  behandling <- 0
  retursignal <- F
  siste.innb <- F
  nsigs <- nrow(siglog)
  for (i in seq_len(nsigs)) {
    s_elem <- siglog[i,"s_elem"]
    sig_dato <- siglog[i, "sig_dato"]
    hnd_type <- siglog[i, "hnd_type"]
    al_kort <- siglog[i, "al_kort"]
    gruppenr <- siglog[i, "gruppenr"]
    tekst_1 <- siglog[i, "tekst_1"]
    tekst_2 <- if (i < nsigs) (siglog[i+1, "tekst_1"]) else ''

    # Hopp over oppgaver til kundesenteret, etc
    if (s_elem == 0 && al_kort == '') {
      next
    }

    signalement <- s_elem != 0

    # Opprettes nytt signalement?
    if (signalement && s_elem != previous_s_elem) {
      if (al_kort %in% c("VARSEL") &&
          str_matches(tekst_1, "Virtuell Vekterrunde")) {
        autobehandling <- F
        out("\n\n")
      } else if (hnd_type %in% '22') {
        autobehandling <- F
        out("\n\n", "Manuelt opprettet behandling", "\n",
            sig_dato, " ", trl("Operatør starter ny behandling manuelt"))
      } else if (str_matches(tekst_2, "definert, signal til VL")) {
        autobehandling <- T
        out("\n\n", trl("Automatisk behandling"))
      } else {
        autobehandling <- F
        behandling <- behandling + 1
        out("\n\n",
            trl("Behandling"),
            " ", behandling)
      }
    }
    previous_s_elem <- s_elem

    # Legges signalement på venteliste? Hva blokkeres?
    if (hnd_type %in% '63') {
      venteliste <- T
      if (str_matches(tekst_1, ", Er sperret:")) {
        venteliste.sperr_angitte <- T
        lst <- str_match_inner(tekst_1, "Er sperret:+(.*)$")
        venteliste.sperreliste <- lst
        lst <- stringr::str_split(lst, ", ")[[1]]
        venteliste.al_kort <- lst[grep("[0-9]+", lst)]
        venteliste.gruppenr <- lst[grep("[a-zA-Z_-].*", lst)]
        minutter <- str_match_inner(tekst_1, "Vent tid:\\s*(\\d+)")
        if (length(venteliste.al_kort) + length(venteliste.gruppenr) == 0) {
          out(sig_dato, " ",
              sprintf(
                trl("Legges på venteliste, avventer nye signaler, tid utløper etter %s minutter"),
                minutter))
        } else {
          out(sig_dato, " ",
              sprintf(
                trl("Blokkerer %s, avventer alle andre signaler, tid utløper etter %s minutter"),
                venteliste.sperreliste, minutter))
        }
      } else if (str_matches(tekst_1, ", Sperr ikke:")) {
        venteliste.sperr_angitte <- F
        lst <- str_match_inner(tekst_1, "Sperr ikke:+(.*)$")
        venteliste.sperreliste <- lst
        lst <- stringr::str_split(lst, ", ")[[1]]
        venteliste.al_kort <- lst[grep("[0-9]+", lst)]
        venteliste.gruppenr <- lst[grep("[a-zA-Z_-].*", lst)]
        minutter <- str_match_inner(tekst_1, "Vent tid:\\s*(\\d+)")
        out(sig_dato, " ",
            sprintf(
              trl("Avventer %s, blokkerer alle andre signaler, tid utløper etter %s minutter"),
              venteliste.sperreliste, minutter))
      } else if (str_matches(tekst_1, "Vent tid:\\s*\\d+$")) {
        venteliste.sperr_angitte <- F
        venteliste.sperreliste <- ""
        venteliste.al_kort <- NULL
        venteliste.gruppenr <- NULL
        minutter <- str_match_inner(tekst_1, "Vent tid:\\s*(\\d+)")
        out(sig_dato, " ",
            sprintf(
              trl("Blokkerer alle signaler, tid utløper etter %s minutter"),
              minutter))
      } else if (str_matches(tekst_1, "definert, signal til VL")) {
        out(sig_dato, " ",
            trl("Signal behandles automatisk, legges på venteliste"))
        autobehandling <- T
      } else if (!str_matches(tekst_1, "^Anm:")) {
        warning(paste0("ukjent måte å legge noe på venteliste: abonnent=", (siglog[i, "abonnent"]),
                       " sig_dato=", sig_dato, " s_elem=", s_elem, " hnd_type=", hnd_type, " tekst_1=", tekst_1))
      }
    }

    if (autobehandling) {
      if (al_kort %notin% c("", "POLLUT", "KLAR")) {
        autobehandling <- F
      }
    }

    # Venteliste og nytt signal: mulig retur
    if (venteliste && !autobehandling) {
      if (al_kort != "") {
        retursignal <- F
        if (venteliste.sperr_angitte &&
            al_kort %notin% venteliste.al_kort &&
            gruppenr %notin% venteliste.gruppenr) {
          retursignal <- T
        } else {
          retursignal <- T
          if (al_kort %notin% venteliste.al_kort &&
              gruppenr %notin% venteliste.gruppenr) {
            retursignal <- F
          }
        }
        if (retursignal) {
          venteliste <- F
          behandling <- behandling + 1
          out("\n\n",
              trl("Behandling"),
              " ", behandling)
          out(sig_dato, " ",
              trl("Mottatt nytt signal, tilbake fra venteliste"))
        }
      }
    }

    # Venteliste: timeout eller manuelt avsluttet
    if (venteliste && hnd_type %in% '62') {
      venteliste <- F
      autobehandling <- F
      if (str_matches(tekst_1, "^Overført fra venteliste")) {
        autobehandling <- F
        out("\n\n", "Manuelt opprettet behandling", "\n",
            sig_dato, " ", trl("Manuelt tatt ned fra venteliste"))
      } else {
        behandling <- behandling + 1
        out("\n\n",
            trl("Behandling"),
            " ", behandling)
        if (str_matches(tekst_1, "^Retursignal fra VL")) {
          out(sig_dato, " ", trl("Timeout fra venteliste"))
        } else if (str_matches(tekst_1, "^Nyt signal, overf")) {
          # do nothing, we already handle this perfectly
        } else {
          out("\n\n", sig_dato, " ", trl("Retur fra venteliste"))
          warning(paste0("ukjent måte å komme tilbake fra venteliste: abonnent=", (siglog[i, "abonnent"]),
                         " sig_dato=", sig_dato, " s_elem=", s_elem, " hnd_type=", hnd_type, " tekst_1=", tekst_1))
        }
      }
    }

    # Kontaktperson ringt
    if (hnd_type %in% '02' ||
        (hnd_type %in% '71' && str_matches(tekst_1, "Anmerkning:.*Tlf:.*Anm:"))) {
      out(sig_dato, " ",
          trl("Ringer kontaktperson på tlf"), " ",
          str_match_inner(tekst_1, "Tlf:\\s*(\\d+)"), " ",
          str_match_inner(tekst_1, "Lok:\\s*\\d+\\s*(.*)\\s*Anm:"))
    }

    # Utrykning påbegynnes
    if (hnd_type %in% '32') {
      out(sig_dato, " ",
          trl("Operatør bestemmer seg for å sende utrykning"))
    }

    # Anmerkninger fra operatør
    if (str_matches(tekst_1, "^Anmerkning:") &&
        !str_matches(tekst_1, "Anmerkning:.*Tlf:.*Anm:")) {
      anm <- str_match_inner(tekst_1, "^Anmerkning:(.*)$")
      if (anm != "") {
        out(sig_dato, " ",
            trl("Operatør skriver: "),
            anm)
        mtrl.anm <- mtranslate(anm, lang)
        if (mtrl.anm != anm) {
          out(sig_dato, " (",
              trl("Maskinoversatt: "),
              mtrl.anm,
              ")")
        }
      }
    }

    if (str_matches(tekst_1, "Anm:")) {
      anm <- str_match_inner(tekst_1, "Anm:(.*)$")
      if (anm != "") {
        out(sig_dato, " ",
            trl("Operatør skriver: "),
            anm)
        mtrl.anm <- mtranslate(anm, lang)
        if (mtrl.anm != anm) {
          out(sig_dato, " (",
              trl("Maskinoversatt: "),
              mtrl.anm,
              ")")
        }
      }
    }

    # Hvordan formatere signaler som behandles,
    # og hvordan formatere signaler som går rett i logg
    prefix <- paste0(sig_dato, " ")
    postfix <- ''
    if (s_elem ==0 || venteliste) {
      prefix <- paste0(prefix, translate("(til logg: ", lang=lang))
      postfix <- ')'
    }

    # INNB og ALARM
    # Kentima sender alltid sone 0, kameranummer sendes separat
    if (hnd_type %in% 'EU' && str_matches(tekst_1, "Hendelsesoppdatering:.*Kamera \\d+\\]") && siste.innb) {
      kam <- str_match_inner(tekst_1, "Hendelsesoppdatering:.*Kamera (\\d+)\\]")
      out(prefix,
          sprintf(trl("Kamera %s"), kam),
          postfix)
    }
    # Her begynner den egentlige innbruddssignalshåndteringen
    if (al_kort %in% c("INNB", "ALARM")) {
      siste.innb <- T
      out(prefix,
          sprintf(trl("Alarm utløst sone %d"),
                  gruppenr),
          postfix)
    } else {
      siste.innb <- F
    }

    # Bosch CI CID: 300 er SYSTEM eller KLAR på kamera
    if (str_matches(tekst_1, "CI CID: 300")) {
      if (al_kort %in% "SYSTEM") {
        out(prefix,
            sprintf(trl("Systemfeil sone %d"),
                    gruppenr),
            postfix)
      } else if (al_kort %in% "KLAR") {
        out(prefix,
            sprintf(trl("Systemfeil ok, sone %d"),
                    gruppenr),
            postfix)
      }
    }

    if (al_kort %in% "POLLUT") {
      out(prefix,
          sprintf(trl("Pollefeil")),
          postfix)
    }

    if (al_kort %in% "KLAR" && str_matches(tekst_1, "YK")) {
      out(prefix,
          sprintf(trl("Signaloverføring ok")),
          postfix)
    }


    # VARSEL om virtuell videorunde
    if (al_kort %in% c("VARSEL") &&
        str_matches(tekst_1, "Virtuell Vekterrunde")) {
      out(prefix,
          sprintf(trl("Tidsvarsling om virtuell vekterrunde")),
          postfix)
    }

    # FRAK skrives ut med klokkeslett/dato
    if (al_kort %in% c("KLAR") &&
        str_matches(tekst_1, "Virtuell Vekterrunde")) {
      out(prefix,
          sprintf(trl("Virtuell vekterrunde fullført")),
          postfix)
    }

    # Avsluttes signalement?
    if (hnd_type %in% '03') {
      venteliste <- F
      out(trl("Behandling avsluttes"))
    }

    if (hnd_type %in% '65') {
      venteliste <- F
      out(trl("Behandling avsluttes automatisk"))
      autobehandling <- F
    }
  }
}
