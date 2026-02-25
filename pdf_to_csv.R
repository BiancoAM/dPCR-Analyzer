# ============================================================
#  dPCR Analyzer - PDF to CSV Converter
#  Converts QIAcuity nanoplate PDF reports to CSV
#  Compatible with: QIAcuity Software Suite 2.x and 3.x
# ============================================================
#
#  HOW TO USE:
#  1. Set PDF_PATH to your QIAcuity PDF file
#  2. Set TARGET_NAME to your target gene/mutation name
#  3. Run the script (Source or Ctrl+Shift+Enter)
#  4. The CSV will be saved in the same folder as the PDF
# ============================================================

# ==== USER SETTINGS =========================================

if (!exists("PDF_PATH"))    PDF_PATH    <- "/Volumes/mony2/pdf_reports_dPCR/nuovo.pdf"   # <-- Change this
if (!exists("TARGET_NAME")) TARGET_NAME <- "nuovo"                # <-- Change this

# ============================================================

if (!requireNamespace("pdftools", quietly = TRUE)) install.packages("pdftools")
library(pdftools)
library(stringr)

# ---- Version detection -------------------------------------

detect_suite_version <- function(full_text) {
  if (grepl("Software Suite 3\\.", full_text)) return(3L)
  return(2L)
}

# ---- Parser for Suite 2.x (original format) ----------------

extract_v2 <- function(full_text, target_name) {
  results <- list()

  sample_pattern <- "([A-Z]\\d+)\\s+(\\d+-\\d+[A-Z]?|DNA\\s+control|NTC|[A-Za-z0-9_\\-\\.]+)"
  sample_matches <- str_match_all(full_text, sample_pattern)[[1]]

  if (nrow(sample_matches) > 0) {
    uniq_key <- paste(sample_matches[,2], sample_matches[,3])
    sample_matches <- sample_matches[!duplicated(uniq_key), , drop = FALSE]
    exclude <- grepl("^(FAM|HEX|ROX|Cy5|GREEN|RED|BLUE|µ|nL|Total|-)$",
                     sample_matches[,3], ignore.case = TRUE)
    sample_matches <- sample_matches[!exclude, , drop = FALSE]
  }

  if (nrow(sample_matches) == 0) {
    cat("WARNING: No samples found in PDF (v2 parser).\n")
    return(invisible(NULL))
  }

  cat(sprintf("Found %d samples:\n", nrow(sample_matches)))

  for (i in seq_len(nrow(sample_matches))) {
    well_id   <- sample_matches[i, 2]
    sample_id <- trimws(sample_matches[i, 3])
    cat(sprintf("  [%s] %-20s ... ", well_id, sample_id))

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

        g_total <- ifelse(any(green_nums > 15000),  max(green_nums[green_nums > 15000]),  25000)
        g_pos   <- ifelse(any(green_nums >= 0 & green_nums <= 15000),
                          green_nums[green_nums >= 0 & green_nums <= 15000][1], 0)
        m_total <- ifelse(any(second_nums > 15000), max(second_nums[second_nums > 15000]), g_total)
        m_pos   <- ifelse(any(second_nums >= 0 & second_nums <= 15000),
                          second_nums[second_nums >= 0 & second_nums <= 15000][1], 0)

        results[[length(results) + 1]] <- data.frame(
          sample_id = sample_id, target = target_name,
          mutant_positive = m_pos, wt_positive = g_pos,
          total_partitions = g_total, stringsAsFactors = FALSE
        )
        cat(sprintf("OK  [WT=%5d | MUT=%5d | Total=%d]\n", g_pos, m_pos, g_total))
      } else {
        cat("SKIP (second channel not found)\n")
      }
    } else {
      cat("NOT FOUND\n")
    }
  }
  return(results)
}

# ---- Parser for Suite 3.x (new format) ---------------------

extract_v3 <- function(full_text, target_name) {
  results <- list()

  # Find Absolute Quantification section
  quant_idx <- regexpr("(?i)(Qbsolute|Absolute).{0,15}(uanti|Quanti)", full_text, perl = TRUE)
  if (quant_idx < 0) {
    cat("WARNING: Could not find Absolute Quantification section.\n")
    return(invisible(NULL))
  }
  quant_text <- substr(full_text, quant_idx, nchar(full_text))

  # Find well ID + sample name pairs (preceded by Ref. channel on next lines)
  # Pattern: [WELL_ID]   [SAMPLE_NAME]
  #          [spaces]    Ref. channel
  well_pattern  <- "([A-Z%x][0-9])\\s+([A-Za-z0-9][A-Za-z0-9_\\-\\./: ]{1,60})\\s*\\n[\\s\\S]{0,120}?Ref\\.\\s*channel"
  well_matches  <- str_match_all(quant_text, well_pattern)[[1]]

  # Remove duplicates (same well+sample)
  if (nrow(well_matches) > 0) {
    well_matches <- well_matches[!duplicated(paste(well_matches[,2], well_matches[,3])), , drop=FALSE]
    # Remove lines that are clearly headers
    exclude <- grepl("^(Sample|NTC|Control|Reaction|Target)$", trimws(well_matches[,3]), ignore.case=TRUE)
    well_matches <- well_matches[!exclude, , drop=FALSE]
  }

  if (nrow(well_matches) == 0) {
    cat("WARNING: No samples found in PDF (v3 parser).\n")
    return(invisible(NULL))
  }

  cat(sprintf("Found %d samples:\n", nrow(well_matches)))

  for (i in seq_len(nrow(well_matches))) {
    raw_well   <- well_matches[i, 2]
    sample_id  <- trimws(well_matches[i, 3])

    # Fix OCR well ID (% -> B, x -> E, etc.)
    well_id <- gsub("%", "B", gsub("x", "E", raw_well))

    cat(sprintf("  [%s] %-30s ... ", well_id, sample_id))

    # Find position of this well in quant_text
    esc_sample <- gsub("([[:punct:]])", "\\\\\\1", sample_id)
    m_full_match <- regexpr(paste0(raw_well, "\\s+", esc_sample, "[\\s\\S]{0,180}?Ref\\.\\s*channel"),
                     quant_text, perl = TRUE)

    if (m_full_match < 0) {
      cat("NOT FOUND\n")
      next
    }

    # === Green/WT: integer on the line just before Yreen ===
    # Pattern: (integer) (spaces) (XX.XX threshold) (newline) (spaces) Yreen
    before_block <- substr(quant_text, max(1, m_full_match - 900), m_full_match)
    gp_m <- regexpr("(\\d+)\\s+\\d{1,2}\\.\\d{2}\\s*\\n\\s+Yreen", before_block, perl = TRUE)
    if (gp_m > 0) {
      gp_block <- substr(before_block, gp_m, gp_m + attr(gp_m, "match.length"))
      g_pos <- as.integer(str_extract(gp_block, "^\\d+"))
    } else {
      g_pos <- 0
    }

    # Total partitions: large number (>20000) on the line just before MAH
    tp_m <- regexpr("(\\d+)\\s*\\n[\\s\\S]{0,80}?MAH", before_block, perl = TRUE)
    if (tp_m > 0) {
      tp_block <- substr(before_block, tp_m, tp_m + attr(tp_m, "match.length"))
      tp_nums  <- as.numeric(str_extract_all(tp_block, "\\b\\d+\\b")[[1]])
      total_part <- if (any(tp_nums > 20000)) tp_nums[tp_nums > 20000][1] else 26000
    } else {
      total_part <- 26000
    }

    # === Yellow/MUT: integer on the line just before Mellow ===
    after_start <- m_full_match + attr(m_full_match, "match.length")
    after_block  <- substr(quant_text, after_start, min(nchar(quant_text), after_start + 800))

    mp_m <- regexpr("(\\d+)\\s+\\d{1,2}\\.\\d{2}\\s*\\n\\s+Mellow", after_block, perl = TRUE)
    if (mp_m > 0) {
      mp_block  <- substr(after_block, mp_m, mp_m + attr(mp_m, "match.length"))
      m_pos_val <- as.integer(str_extract(mp_block, "^\\d+"))
    } else {
      m_pos_val <- 0
    }

    # Refine total using pos+neg if sum makes sense
    neg_m <- regexpr("Mellow\\s*\\n\\s+(\\d+)", after_block, perl = TRUE)
    if (neg_m > 0) {
      neg_block <- substr(after_block, neg_m, neg_m + attr(neg_m, "match.length"))
      neg_val   <- as.integer(str_extract(neg_block, "\\d+$"))
      if (!is.na(neg_val) && !is.na(m_pos_val) && (m_pos_val + neg_val) > 20000)
        total_part <- m_pos_val + neg_val
    }

    results[[length(results) + 1]] <- data.frame(
      sample_id = sample_id, target = target_name,
      mutant_positive = m_pos_val, wt_positive = g_pos,
      total_partitions = total_part, stringsAsFactors = FALSE
    )
    cat(sprintf("OK  [WT=%5d | MUT=%5d | Total=%d]\n", g_pos, m_pos_val, total_part))
  }

  return(results)
}

# ---- Main extraction function ------------------------------

extract_qiacuity_data <- function(pdf_path, target_name) {

  if (!file.exists(pdf_path)) stop("PDF file not found: ", pdf_path)

  cat("\n=== QIAcuity PDF Extractor ===\n")
  cat("File   :", basename(pdf_path), "\n")
  cat("Target :", target_name, "\n\n")

  text      <- pdf_text(pdf_path)
  full_text <- paste(text, collapse = "\n")

  version <- detect_suite_version(full_text)
  cat(sprintf("Detected: QIAcuity Software Suite %d.x format\n\n", version))

  results <- if (version >= 3L) {
    extract_v3(full_text, target_name)
  } else {
    extract_v2(full_text, target_name)
  }

  if (length(results) == 0) {
    cat("\nERROR: No data extracted. Check PDF format.\n")
    return(invisible(NULL))
  }

  df <- do.call(rbind, results)

  out_dir  <- dirname(pdf_path)
  out_name <- paste0(tools::file_path_sans_ext(basename(pdf_path)), "_extracted.csv")
  out_path <- file.path(out_dir, out_name)
  write.csv(df, out_path, row.names = FALSE, quote = FALSE)

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat(" CSV saved to:", out_path, "\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Ready to import in dPCR Analyzer!\n")
  cat("  1. Open app_publication.R\n")
  cat("  2. Set Experiment Type: Allele-Specific (Dual-Color)\n")
  cat("  3. Data Input -> Import CSV -> upload the file above\n\n")

  print(df, row.names = FALSE)
  return(invisible(df))
}

# ---- Run ---------------------------------------------------

result <- extract_qiacuity_data(PDF_PATH, TARGET_NAME)
