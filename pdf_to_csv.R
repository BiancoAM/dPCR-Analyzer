# ============================================================
#  dPCR Analyzer - PDF to CSV Converter
#  Converts QIAcuity nanoplate PDF reports to CSV
#  for import into dPCR Analyzer Pro
# ============================================================
#
#  HOW TO USE:
#  1. Set PDF_PATH to your QIAcuity PDF file
#  2. Set TARGET_NAME to your target gene/mutation name
#  3. Run the script (Source or Ctrl+Shift+Enter)
#  4. The CSV will be saved in the same folder as the PDF
# ============================================================

# ==== USER SETTINGS =========================================

PDF_PATH    <- "/Volumes/mony2/pdf_reports_dPCR/mutation detection_skin_stain.pdf"   # <-- Change this
TARGET_NAME <- "LZTR1(c.742C>T)"                  # <-- Change this

# ============================================================

# Install pdftools if needed
if (!requireNamespace("pdftools", quietly = TRUE)) {
  install.packages("pdftools")
}
library(pdftools)
library(stringr)

# ---- Core extraction function ------------------------------

extract_qiacuity_data <- function(pdf_path, target_name) {

  if (!file.exists(pdf_path)) {
    stop("PDF file not found: ", pdf_path)
  }

  cat("\n=== QIAcuity PDF Extractor ===\n")
  cat("File   :", basename(pdf_path), "\n")
  cat("Target :", target_name, "\n\n")

  text      <- pdf_text(pdf_path)
  full_text <- paste(text, collapse = "\n")

  results <- list()

  # Find all samples (well ID + sample name)
  sample_pattern <- "([A-Z]\\d+)\\s+(\\d+-\\d+[A-Z]?|DNA\\s+control|NTC|[A-Za-z0-9_\\-\\.]+)"
  sample_matches <- str_match_all(full_text, sample_pattern)[[1]]

  # Remove duplicates
  if (nrow(sample_matches) > 0) {
    uniq_key       <- paste(sample_matches[,2], sample_matches[,3])
    sample_matches <- sample_matches[!duplicated(uniq_key), , drop = FALSE]
    # Filter out obvious non-sample matches (FAM, HEX, etc.)
    exclude <- grepl("^(FAM|HEX|ROX|Cy5|GREEN|RED|BLUE|µ|nL)$",
                     sample_matches[,3], ignore.case = TRUE)
    sample_matches <- sample_matches[!exclude, , drop = FALSE]
  }

  if (nrow(sample_matches) == 0) {
    cat("WARNING: No samples found in PDF.\n")
    cat("  Tip: Make sure the PDF is a QIAcuity nanoplate report.\n\n")
    return(invisible(NULL))
  }

  cat(sprintf("Found %d samples:\n", nrow(sample_matches)))

  for (i in seq_len(nrow(sample_matches))) {
    well_id   <- sample_matches[i, 2]
    sample_id <- trimws(sample_matches[i, 3])

    cat(sprintf("  [%s] %-20s ... ", well_id, sample_id))

    # Find data block for this sample
    search_pat <- paste0(
      well_id, "\\s+",
      gsub("-", "\\\\-", gsub("\\s+", "\\\\s+", sample_id)),
      "[\\s\\S]{50,600}?GREEN[\\s\\S]{50,1500}?µEFF_"
    )
    block_match <- regexpr(search_pat, full_text, perl = TRUE)

    if (block_match[1] > 0) {
      block_start <- block_match[1]
      block_end   <- block_match[1] + attr(block_match, "match.length") + 500
      block       <- substr(full_text, block_start, block_end)

      green_pos_m  <- regexpr("GREEN", block, perl = TRUE)[1]
      second_pos_m <- regexpr("(µEFF_|\\s+HEX\\s+|µellow)", block, perl = TRUE)[1]

      if (green_pos_m > 0 && second_pos_m > 0) {
        green_sec  <- substr(block, green_pos_m, second_pos_m - 1)
        second_sec <- substr(block, second_pos_m, nchar(block))

        green_nums  <- as.numeric(str_extract_all(green_sec,  "\\b\\d+\\b")[[1]])
        second_nums <- as.numeric(str_extract_all(second_sec, "\\b\\d+\\b")[[1]])

        # Partitions > 15000, positives between 0-15000
        g_total <- ifelse(any(green_nums > 15000),  max(green_nums[green_nums > 15000]),  25000)
        g_pos   <- ifelse(any(green_nums >= 0 & green_nums <= 15000),
                          green_nums[green_nums >= 0 & green_nums <= 15000][1], 0)
        m_total <- ifelse(any(second_nums > 15000), max(second_nums[second_nums > 15000]), g_total)
        m_pos   <- ifelse(any(second_nums >= 0 & second_nums <= 15000),
                          second_nums[second_nums >= 0 & second_nums <= 15000][1], 0)

        # Auto-detect sample type
        stype <- dplyr::case_when(
          grepl("NTC|neg|control|ctrl", sample_id, ignore.case = TRUE) ~ "neg_ctrl",
          grepl("pos|positive",         sample_id, ignore.case = TRUE) ~ "pos_ctrl",
          TRUE ~ "sample"
        )

        results[[length(results) + 1]] <- data.frame(
          sample_id          = sample_id,
          target             = target_name,
          mutant_positive    = m_pos,
          wt_positive        = g_pos,
          total_partitions   = g_total,
          stringsAsFactors   = FALSE
        )
        cat(sprintf("OK  [WT=%5d | MUT=%5d | Total=%d]\n", g_pos, m_pos, g_total))

      } else {
        cat("SKIP (second channel not found)\n")
      }
    } else {
      cat("NOT FOUND\n")
    }
  }

  if (length(results) == 0) {
    cat("\nERROR: No data extracted. Check PDF format.\n")
    return(invisible(NULL))
  }

  df <- do.call(rbind, results)

  # Save CSV next to the PDF
  out_dir  <- dirname(pdf_path)
  out_name <- paste0(tools::file_path_sans_ext(basename(pdf_path)), "_extracted.csv")
  out_path <- file.path(out_dir, out_name)
  write.csv(df, out_path, row.names = FALSE, quote = FALSE)

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat(" CSV saved to:", out_path, "\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Ready to import in dPCR Analyzer Pro!\n")
  cat("  1. Open app_publication.R\n")
  cat("  2. Set Experiment Type: Allele-Specific (Dual-Color)\n")
  cat("  3. Data Input -> Import CSV -> upload the file above\n\n")

  print(df, row.names = FALSE)
  return(invisible(df))
}

# ---- Run -------------------------------------------------------

result <- extract_qiacuity_data(PDF_PATH, TARGET_NAME)
