translations <- list(
  "Behandling"=list(
    se="Behandling"
  ),

  "Operatør starter ny behandling manuelt"=list(
    se="Operatör startar ny behandling manuellt"
  ),

  "Legges på venteliste, avventer nye signaler, tid utløper etter %s minutter"=list(
    se="Läggs på väntlistan, avvaktar nya signaler, tid går ut efter %s minuter"
  ),

  "Blokkerer %s, avventer alle andre signaler, tid utløper etter %s minutter"=list(
    se="Blockerar %s, avvaktar alla andra signaler, tiden går ut efter %s minuter"
  ),

  "Avventer %s, blokkerer alle andre signaler, tid utløper etter %s minutter"=list(
    se="Avvaktar %s, blockerar alla andra signaler, tid går ut efter %s minuter"
  ),

  "Blokkerer alle signaler, tid utløper etter %s minutter"=list(
    se="Blockerar alla signaler, tiden går ut efter %s minuter"
  ),

  "Signal behandles automatisk, legges på venteliste"=list(
    se="Signal behandlas automatiskt, placeras på väntelistan"
  ),

  "Automatisk behandling"=list(
    se="Automatisk behandling"
  ),

  "Manuelt opprettet behandling"=list(
    se="Manuellt skapad behandling"
  ),

  "Mottatt nytt signal, tilbake fra venteliste"=list(
    se="Ny signal mottagen, tillbaka från väntlistan"
  ),

  "Timeout fra venteliste"=list(
    se="Timeout från väntlistan"
  ),

  "Manuelt tatt ned fra venteliste"=list(
    se="Manuellt hämtat från väntlistan"
  ),

  "Ringer kontaktperson på tlf"=list(
    se="Ringer kontaktperson via tlf"
  ),

  "Operatør bestemmer seg for å sende utrykning"=list(
    se="Operatör bestämmer sig för att skicka en väktare"
  ),

  "Operatør skriver: "=list(
    se="Operatör skriver: "
  ),

  "(til logg: "=list(
    se="(till log: "
  ),

  "Alarm utløst sone %d"=list(
    se="Alarm utlöst zon %d"
  ),

  "Systemfeil sone %d"=list(
    se="Systemfel zon %d"
  ),

  "Systemfeil ok, sone %d"=list(
    se="Systemfel ok, zon %d"
  ),

  "Pollefeil"=list(
    se="Pollingfel"
  ),

  "Signaloverføring ok"=list(
    se="Signalöverföring ok"
  ),

    "Tilkobling"=list(
    se="Tillkoppling"
  ),

  "Frakobling"=list(
    se="Frånkoppling"
  ),

  "Behandling avsluttes"=list(
    se="Behandling avslutas"
  ),

  "Behandling avsluttes automatisk"=list(
    se="Behandling avslutas automatiskt"
  ),

  "Forenklet signalbehandlingslogg"=list(
    se="Förenklad signalbehandlingslog"
  ),

  "Safecon kundenr: "=list(
    se="Safecon kundnummer: "
  ),

  "Navn: "=list(
    se="Namn: "
  ),

  "Dato fra og med: "=list(
    se="Datum från och med: "
  ),

  "Dato til: "=list(
    se="Datum till: "
  ),

  "Ingen hendelser i tidsrommet"=list(
    se="Inga händelser inom tidsintervallet"
  ),

  "Logg slutt."=list(
    se="Log slut."
  ),

  "Se vedlegg."=list(
    se="Se bifogat."
  ),

  "Ingen hendelser i tidsrommet"=list(
    se="Inga händelser inom tidsintervallet"
  ),

  "Med vennlig hilsen Safe4"=list(
    se="Med vänlig hälsning Safe4"
  ),

  "Antall behandlinger hittil i måneden: "=list(
    se="Antal behandlingar hittills denna månad: "
  ),

  "(dette bør være under %s)"=list(
    se="(detta bör vara under %s)"
  ),

  "Tidsvarsling om virtuell vekterrunde"=list(
    se="Tidsvarning om virtuell vaktrunda"
  ),

  "Virtuell vekterrunde fullført"=list(
    se="Virtuell vaktrunda slutförd"
  ),

    "Maskinoversatt: "=list(
    se="Maskinöversatt: "
  ),

  "Kamera %s"=list(
    se="Kamera %s"
  ),

  "Antallet behandlinger er et estimat,\nse tidligere utsendte rapporter for mer nøyaktige tall."=list(
    se="Antalet behandlingar är en uppskattning,\nse tidigare utfärdade rapporter för mer exakta siffror."
  )
)
translations <-
  lapply(translations, function (x) {lapply(x, enc2utf8) })
names(translations) <- enc2utf8(names(translations))

translate <- function(x, lang="no") {
  x <- enc2utf8(x)
  if (is.null(translations[[x]])) {
      warning("Missing translation for: ", x)
  }
  if (lang == "no") {
    return(x)
    }
  translation <- translations[[x]]
  if (is.null(x)) {
    return(x)
  }
  result <- translation[[lang]]
  if (is.null(result)) {
    warning("Missing translation for language ", lang, " for: ", x)
    return(x)
  } else {
    return(result)
  }
}
