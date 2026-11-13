# Letar i Windows certifikatförråd med programmet certutil, så exemplet
# körs bara på Windows.
if (.Platform$OS.type == "windows") {
  cert <- tryCatch(scb_hamta_cert(), error = function(e) NULL)
  if (is.null(cert)) {
    message("Inget SCB-certifikat för sökpåver-API:t är installerat.")
  } else {
    cert$store
  }
}
