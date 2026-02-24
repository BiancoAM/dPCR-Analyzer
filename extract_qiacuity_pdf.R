# Estrazione automatica dati da PDF QIAcuity
# Versione 7 - Estrazione automatica di TUTTI i campioni

library(pdftools)
library(stringr)

extract_sample_data <- function(pdf_path, target_name) {

  cat("\nElaborando:", basename(pdf_path), "\n")

  text <- pdf_text(pdf_path)
  full_text <- paste(text, collapse = "\n")

  results <- list()

  # Trova TUTTI i campioni con pattern specifici
  # Accetta: numeri-numeri+lettera (24-3222A), DNA control, NTC, etc.
  # Esclude: FAM, HEX (marker names)
  sample_list_pattern <- "([A-Z]\\d+)\\s+(\\d+-\\d+[A-Z]?|DNA\\s+control|NTC)"
  sample_matches <- str_match_all(full_text, sample_list_pattern)[[1]]

  # Rimuovi duplicati (stesso well + sample_id)
  if (nrow(sample_matches) > 0) {
    sample_key <- paste(sample_matches[,2], sample_matches[,3], sep="_")
    sample_matches <- sample_matches[!duplicated(sample_key), , drop=FALSE]
  }

  if (nrow(sample_matches) == 0) {
    cat("  Nessun campione trovato nel PDF\n")
    return(data.frame(
      sample_id = character(),
      sample_type = character(),
      target = character(),
      mutant_positive = numeric(),
      wt_positive = numeric(),
      total_partitions = numeric(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }

  cat("  Trovati", nrow(sample_matches), "campioni nel PDF\n")

  for (idx in 1:nrow(sample_matches)) {
    well_id <- sample_matches[idx, 2]
    sample_id <- sample_matches[idx, 3]
    sample_id <- trimws(sample_id)

    cat("  ", well_id, "-", sample_id, "... ")

    # Cerca il blocco dati per questo campione
    # Pattern: well + sample name + ... GREEN ... µEFF (evita FAM-HEX)
    search_pattern <- paste0(
      well_id, "\\s+",
      gsub("-", "\\\\-", gsub("\\s+", "\\\\s+", sample_id)),
      "[\\s\\S]{50,600}?GREEN[\\s\\S]{50,1500}?µEFF_"
    )

    block_match <- regexpr(search_pattern, full_text, perl = TRUE)

    if (block_match[1] > 0) {
      # Estrai un blocco esteso per catturare tutti i numeri
      block_start <- block_match[1]
      block_end <- block_match[1] + attr(block_match, "match.length") + 500
      block <- substr(full_text, block_start, block_end)

      # Trova le posizioni dei marker
      green_pos_marker <- regexpr("GREEN", block, perl = TRUE)[1]
      # Evita di matchare FAM-HEX, cerca solo µEFF_ o HEX standalone con spazi
      second_channel_pos <- regexpr("(µEFF_|\\s+HEX\\s+|µellow)", block, perl = TRUE)[1]

      if (second_channel_pos > 0 && green_pos_marker > 0) {
        # Dividi DALLA posizione di GREEN, non dall'inizio del blocco
        # Così evitiamo di estrarre numeri dal sample_id
        green_section <- substr(block, green_pos_marker, second_channel_pos - 1)
        second_section <- substr(block, second_channel_pos, nchar(block))

        # Estrai numeri da ogni sezione
        green_nums <- as.numeric(str_extract_all(green_section, "\\b\\d+\\b")[[1]])
        second_nums <- as.numeric(str_extract_all(second_section, "\\b\\d+\\b")[[1]])

        # Logica semplificata: partizioni sono numeri > 1000
        # Valid = numero più grande (> 15000)
        # Positive = primo numero nel range 1000-15000

        # Per GREEN (WT):
        green_big <- green_nums[green_nums > 15000]
        green_partitions <- green_nums[green_nums >= 1000 & green_nums <= 15000]

        if (length(green_big) >= 1) {
          green_valid <- green_big[1]
        } else {
          green_valid <- 25000  # Default
        }

        if (length(green_partitions) > 0) {
          green_pos <- green_partitions[1]
        } else {
          green_pos <- 0
        }

        # Per secondo canale (µEFF = MUTAZIONE):
        second_big <- second_nums[second_nums > 15000]
        second_partitions <- second_nums[second_nums >= 1000 & second_nums <= 15000]

        if (length(second_big) >= 1) {
          second_valid <- second_big[1]
        } else {
          second_valid <- green_valid
        }

        if (length(second_partitions) > 0) {
          second_pos <- second_partitions[1]
        } else {
          second_pos <- 0
        }

        # Determina sample_type
        sample_type <- if (grepl("NTC|ctrl|control|neg", sample_id, ignore.case = TRUE)) {
          "neg_ctrl"
        } else if (grepl("pos|positive|DNA", sample_id, ignore.case = TRUE)) {
          "pos_ctrl"
        } else {
          "sample"
        }

        results[[length(results) + 1]] <- data.frame(
          sample_id = sample_id,
          sample_type = sample_type,
          target = target_name,
          mutant_positive = second_pos,  # µEFF = mutazione
          wt_positive = green_pos,       # GREEN = WT
          total_partitions = green_valid,
          notes = paste("Well:", well_id),
          stringsAsFactors = FALSE
        )

        cat("OK (WT=", sprintf("%5d", green_pos), ", MUT=", sprintf("%5d", second_pos), ", Valid=", green_valid, ")\n")

      } else {
        cat("SKIP (no secondo canale)\n")
      }

    } else {
      cat("NON TROVATO\n")
    }
  }

  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    return(data.frame(
      sample_id = character(),
      sample_type = character(),
      target = character(),
      mutant_positive = numeric(),
      wt_positive = numeric(),
      total_partitions = numeric(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# ====================
# MAIN
# ====================

cat("\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("         ESTRAZIONE AUTOMATICA DATI DA PDF QIACUITY\n")
cat(paste(rep("=", 70), collapse=""), "\n")

if (!require("pdftools", quietly = TRUE)) {
  cat("\nInstallazione pdftools...\n")
  install.packages("pdftools")
  library(pdftools)
}

pdf1 <- "pdf_reports/02-04-2025-mutation detection_skin_stain_zampieri.pdf"

cat("\n--- Estrazione da:", basename(pdf1), "---\n")
data_mut1 <- extract_sample_data(pdf1, "SKIN_STAIN")

# Usa solo il primo file per ora
data_mut2 <- data.frame()

all_data <- rbind(data_mut1, data_mut2)

cat("\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("RISULTATO FINALE:\n")
cat(paste(rep("-", 70), collapse=""), "\n")
cat(sprintf("  Mutazione 1: %2d campioni estratti\n", nrow(data_mut1)))
cat(sprintf("  Mutazione 2: %2d campioni estratti\n", nrow(data_mut2)))
cat(sprintf("  TOTALE:      %2d campioni\n", nrow(all_data)))
cat(paste(rep("=", 70), collapse=""), "\n")

if (nrow(all_data) > 0) {
  output_file <- "examples/qiacuity_extracted_data.csv"
  write.csv(all_data, output_file, row.names = FALSE, quote = FALSE)

  cat("\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat(" CSV CREATO CON SUCCESSO!\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("\nFile:", output_file, "\n\n")
  cat("ANTEPRIMA DATI:\n")
  cat(paste(rep("-", 70), collapse=""), "\n\n")
  print(all_data, row.names = FALSE)

  cat("\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat(" PRONTO PER L'IMPORT NELL'APP!\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("\nPer importare:\n")
  cat("  1. Apri app_fixed.R in RStudio\n")
  cat("  2. Vai al tab 'Input Dati'\n")
  cat("  3. Seleziona 'Import CSV'\n")
  cat("  4. Carica: examples/qiacuity_extracted_data.csv\n")
  cat("\n")

} else {
  cat("\n✗ ERRORE: Nessun dato estratto\n\n")
}
