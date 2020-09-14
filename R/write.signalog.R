if (!exists("%notin%", mode="function")) {
  source("utils.R")
}

if (!exists("translate", mode="function")) {
  source("translate.R")
}

write.signalog <- function(siglog, con=stdout(), lang="no") {
  trl <- function(x) { translate(x, lang=lang) }
  out <- function(...) { writeLines(paste0(...), con=con) }

  previous_s_elem <- 0
  venteliste <- F
  venteliste.al_kort <- NULL
  venteliste.gruppenr <- NULL
  venteliste.sperr_angitte <- F
  venteliste.sperreliste <- NULL
  behandling <- 0
  retursignal <- F
  for (i in seq_len(nrow(siglog))) {
    s_elem <- siglog[i,"s_elem"]
    sig_dato <- siglog[i, "sig_dato"]
    hnd_type <- siglog[i, "hnd_type"]
    al_kort <- siglog[i, "al_kort"]
    gruppenr <- siglog[i, "gruppenr"]
    tekst_1 <- siglog[i, "tekst_1"]

    signalement <- s_elem != 0

    # Opprettes nytt signalement?
    if (signalement && s_elem != previous_s_elem) {
      if (al_kort %in% c("VARSEL") &&
          str_matches(tekst_1, "Virtuell Vekterrunde")) {
        out("\n\n")
      } else {
        behandling <- behandling + 1
        out("\n\n",
            trl("Behandling"),
            " ", behandling)
      }
      if (hnd_type %in% '22') {
        out(sig_dato, " ", trl("Operatør starter ny behandling manuelt"))
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
      } else if (!str_matches(tekst_1, "^Anm:")) {
        warning(paste0("ukjent måte å legge noe på venteliste: abonnent=", (siglog[i, "abonnent"]),
                       " sig_dato=", sig_dato, " s_elem=", s_elem, " hnd_type=", hnd_type, " tekst_1=", tekst_1))
      }
    }

    # Venteliste og nytt signal: mulig retur
    if (venteliste) {
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
      behandling <- behandling + 1
      out("\n\n",
          trl("Behandling"),
          " ", behandling)
      if (str_matches(tekst_1, "^Retursignal fra VL")) {
        out(sig_dato, " ", trl("Timeout fra venteliste"))
      } else if (str_matches(tekst_1, "^Overført fra venteliste")) {
        out(sig_dato, " ", trl("Manuelt tatt ned fra venteliste"))
      }
    }

    # Kontaktperson ringt
    if (hnd_type %in% '02') {
      out(sig_dato, " ",
          trl("Ringer kontaktperson på tlf"),
          str_match_inner(tekst_1, "Tlf:\\s*(\\d+)"), " ",
          str_match_inner(tekst_1, "Lok:\\s*\\d+\\s*(.*)\\s*Anm:"))
    }

    # Utrykning påbegynnes
    if (hnd_type %in% '32') {
      out(sig_dato, " ",
          trl("Operatør bestemmer seg for å sende utrykning"))
    }

    # Anmerkninger fra operatør
    if (str_matches(tekst_1, "^Anmerkning:")) {
      anm <- str_match_inner(tekst_1, "^Anmerkning:(.*)$")
      if (anm != "") {
        out(sig_dato, " ",
            trl("Operatør skriver: "),
            anm)
      }
    }

    if (str_matches(tekst_1, "Anm:")) {
      anm <- str_match_inner(tekst_1, "Anm:(.*)$")
      if (anm != "") {
        out(sig_dato, " ",
            trl("Operatør skriver: "),
            anm)
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
    if (al_kort %in% c("INNB", "ALARM")) {
      out(prefix,
          sprintf(trl("Alarm utløst sone %d"),
                  gruppenr),
          postfix)
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
    }

  }
}
