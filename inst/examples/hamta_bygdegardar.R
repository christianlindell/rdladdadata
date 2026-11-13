# Kräver paketen V8 och funcVGR, VGR:s nätverk och inloggning till
# geodata.vgregion.se (för kommunytorna), och internet. Exemplet körs bara i
# en interaktiv session.
nar_vgr <- tryCatch({
  s <- suppressWarnings(socketConnection("geodata.vgregion.se", 5432, open = "r+", timeout = 3))
  close(s)
  TRUE
}, error = function(e) FALSE)
if (interactive() && nar_vgr && requireNamespace("V8", quietly = TRUE) &&
    requireNamespace("funcVGR", quietly = TRUE)) {
  bygdegardar <- hamta_bygdegardar()
  table(bygdegardar$lan)

  # Som vanlig data frame utan geometri
  bygdegardar_df <- hamta_bygdegardar(returnera_sf = FALSE)
}
