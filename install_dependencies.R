# Script per installazione automatica delle dipendenze
# Esegui questo script prima di usare l'applicazione

cat("=== Installazione Dipendenze Software dPCR ===\n\n")

# Lista pacchetti richiesti
required_packages <- c(
  # Core Shiny
  "shiny",
  "shinydashboard",

  # Data manipulation
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",

  # Visualization
  "ggplot2",
  "plotly",
  "DT",
  "viridis",
  "colourpicker",

  # PDF processing
  "pdftools",

  # Statistical analysis
  "broom",

  # Report generation
  "rmarkdown",
  "knitr"
)

# Pacchetti opzionali
optional_packages <- c(
  "openxlsx",      # Per export Excel
  "gridExtra",     # Per layout grafici avanzati
  "tinytex"        # Per generazione PDF (se non hai LaTeX)
)

# Funzione per installare pacchetti se mancanti
install_if_missing <- function(packages, optional = FALSE) {

  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste0("Installazione ", pkg, "...\n"))

      tryCatch({
        install.packages(pkg, dependencies = TRUE)
        cat(paste0("  ✓ ", pkg, " installato con successo\n"))
      }, error = function(e) {
        if (optional) {
          cat(paste0("  ⚠ ", pkg, " (opzionale) non installato: ", e$message, "\n"))
        } else {
          cat(paste0("  ✗ ERRORE: impossibile installare ", pkg, ": ", e$message, "\n"))
        }
      })
    } else {
      cat(paste0("  ✓ ", pkg, " già installato\n"))
    }
  }
}

# Installa pacchetti richiesti
cat("\n--- Pacchetti Richiesti ---\n")
install_if_missing(required_packages, optional = FALSE)

# Installa pacchetti opzionali
cat("\n--- Pacchetti Opzionali ---\n")
install_if_missing(optional_packages, optional = TRUE)

# Installa TinyTeX se necessario per PDF
cat("\n--- Configurazione LaTeX per Report PDF ---\n")
if (!tinytex::is_tinytex()) {
  cat("TinyTeX non trovato. Vuoi installarlo per generare report PDF? (S/n): ")
  response <- readline()

  if (tolower(response) %in% c("s", "si", "yes", "y", "")) {
    cat("Installazione TinyTeX in corso (potrebbe richiedere alcuni minuti)...\n")
    tinytex::install_tinytex()
    cat("  ✓ TinyTeX installato\n")
  } else {
    cat("  ⚠ TinyTeX non installato. I report saranno generati in HTML.\n")
  }
} else {
  cat("  ✓ TinyTeX già installato\n")
}

# Verifica versione R
cat("\n--- Verifica Versione R ---\n")
r_version <- getRversion()
cat(paste0("Versione R installata: ", r_version, "\n"))

if (r_version < "4.0.0") {
  cat("  ⚠ ATTENZIONE: Versione R < 4.0.0. Si consiglia l'aggiornamento.\n")
} else {
  cat("  ✓ Versione R compatibile\n")
}

# Test caricamento pacchetti
cat("\n--- Test Caricamento Pacchetti ---\n")
failed_packages <- c()

for (pkg in required_packages) {
  test_result <- tryCatch({
    library(pkg, character.only = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (test_result) {
    cat(paste0("  ✓ ", pkg, " caricato correttamente\n"))
  } else {
    cat(paste0("  ✗ ", pkg, " ERRORE nel caricamento\n"))
    failed_packages <- c(failed_packages, pkg)
  }
}

# Riepilogo finale
cat("\n=== RIEPILOGO INSTALLAZIONE ===\n")

if (length(failed_packages) == 0) {
  cat("\n✓ SUCCESSO! Tutti i pacchetti richiesti sono installati e funzionanti.\n")
  cat("\nPuoi ora eseguire l'applicazione con:\n")
  cat("  shiny::runApp('app.R')\n\n")
} else {
  cat("\n✗ ATTENZIONE: Alcuni pacchetti hanno problemi:\n")
  cat(paste("  -", failed_packages, "\n"))
  cat("\nProva a reinstallarli manualmente:\n")
  cat(paste0("  install.packages(c('", paste(failed_packages, collapse = "', '"), "'))\n\n"))
}

# Informazioni di sistema
cat("\n--- Informazioni Sistema ---\n")
cat(paste0("Sistema operativo: ", Sys.info()["sysname"], " ", Sys.info()["release"], "\n"))
cat(paste0("Versione R: ", R.version.string, "\n"))
cat(paste0("RStudio: ", ifelse(Sys.getenv("RSTUDIO") == "1", "Sì", "No"), "\n"))

cat("\n=== Installazione completata ===\n")
