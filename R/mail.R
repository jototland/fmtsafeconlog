#library(RDCOMClient)

send.outlook.email <-
  function(to=character(0),          # vector of strings (email adresses)
           cc=character(0),          # vector of strings (email adresses)
           bcc=character(0),         # vector of strings (email adresses)
           onbehalf=NULL,            # string (from address)
           subject=NULL,             # string
           body=NULL,                # string
           attachments=character(0), # vector of strings (filenames)
           onlySaveDraft=T) {        # change to F if you want to send instad of save draft
  outlookApp <- RDCOMClient::COMCreate("Outlook.Application")
  outlookMail <- outlookApp$CreateItem(0)
  if (length(to) != 0) {
    outlookMail[["To"]] <- iconv(paste(to, collapse="; "), from="utf-8")
  }
  if (length(cc) != 0) {
    outlookMail[["Cc"]] <- iconv(paste(cc, collapse="; "), from ="utf-8")
  }
  if (length(bcc) != 0) {
    outlookMail[["Bcc"]] <- iconv(paste(bcc, collapse="; "), from="utf-8")
  }
  if (!is.null(onbehalf)) {
    outlookMail[["SentOnBehalfOfName"]] <- iconv(onbehalf, from="utf-8")
  }
  if (!is.null(subject)) {
    outlookMail[["subject"]] <- iconv(subject, from="utf-8")
  }
  if (!is.null(body)) {
    outlookMail[["body"]] <- iconv(body, sub="x", from="utf-8")
  }
  if (length(attach) > 0) {
    for (attachment in attachments) {
      outlookMail[["Attachments"]]$Add(normalizePath(attachment))
    }
  }
  if (onlySaveDraft) {
    outlookMail$Save()
  } else {
    outlookMail$Send()
  }
}