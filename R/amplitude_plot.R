# Funzioni per creare Amplitude Plot (2D Scatter) - stile QIAcuity
# Mostra ogni singola partizione come punto in base alla fluorescenza

library(ggplot2)
library(dplyr)
library(tidyr)

#' Simula dati amplitude plot da conteggi aggregati
#'
#' Genera partizioni simulate con ampiezze di fluorescenza realistiche
#' basandosi sui conteggi di positive/negative
#'
#' @param data Data frame con mutant_positive, wt_positive, total_partitions
#' @param mutant_pos_range Range fluorescenza per mutant positive (default c(100, 160))
#' @param mutant_neg_range Range fluorescenza per mutant negative (default c(20, 80))
#' @param wt_pos_range Range fluorescenza per WT positive (default c(40, 100))
#' @param wt_neg_range Range fluorescenza per WT negative (default c(10, 50))
#' @param noise_sd Deviazione standard del rumore (default 5)
#' @param downsample Percentuale di partizioni da simulare (0.1-1.0, default 0.5 = 50%)
#' @return Data frame con partizioni simulate
#' @export
simulate_amplitude_data <- function(data,
                                   mutant_pos_range = c(100, 160),
                                   mutant_neg_range = c(20, 80),
                                   wt_pos_range = c(40, 100),
                                   wt_neg_range = c(10, 50),
                                   noise_sd = 5,
                                   downsample = 0.5) {

  all_partitions <- list()

  for (i in 1:nrow(data)) {
    sample <- data[i, ]

    # Numero di partizioni per categoria (con downsampling)
    n_mutant_pos <- round(sample$mutant_positive * downsample)
    n_wt_pos <- round(sample$wt_positive * downsample)
    n_total_downsampled <- round(sample$total_partitions * downsample)
    n_double_neg <- n_total_downsampled - n_mutant_pos - n_wt_pos
    n_double_pos <- 0  # Assumiamo 0 per dPCR allele-specific

    # Genera ampiezze per mutant channel
    if (n_mutant_pos > 0) {
      mutant_pos_amp <- runif(n_mutant_pos, mutant_pos_range[1], mutant_pos_range[2]) +
                        rnorm(n_mutant_pos, 0, noise_sd)
    } else {
      mutant_pos_amp <- numeric(0)
    }

    mutant_neg_amp <- runif(n_double_neg + n_wt_pos, mutant_neg_range[1], mutant_neg_range[2]) +
                      rnorm(n_double_neg + n_wt_pos, 0, noise_sd)

    # Genera ampiezze per WT channel
    if (n_wt_pos > 0) {
      wt_pos_amp <- runif(n_wt_pos, wt_pos_range[1], wt_pos_range[2]) +
                    rnorm(n_wt_pos, 0, noise_sd)
    } else {
      wt_pos_amp <- numeric(0)
    }

    wt_neg_amp <- runif(n_double_neg + n_mutant_pos, wt_neg_range[1], wt_neg_range[2]) +
                  rnorm(n_double_neg + n_mutant_pos, 0, noise_sd)

    # Crea data frame partizioni
    partitions <- data.frame(
      sample_id = sample$sample_id,
      partition_index = 1:n_total_downsampled,
      mutant_amplitude = c(mutant_pos_amp, mutant_neg_amp),
      wt_amplitude = c(rep(wt_neg_range[1] + noise_sd, n_mutant_pos),
                       wt_pos_amp,
                       rep(wt_neg_range[1] + noise_sd, n_double_neg)),
      classification = c(
        rep("Mutant+", n_mutant_pos),
        rep("WT+", n_wt_pos),
        rep("Double Negative", n_double_neg)
      )
    )

    # Shuffla per randomizzare ordine (come partizioni reali)
    partitions <- partitions[sample(nrow(partitions)), ]
    partitions$partition_index <- 1:nrow(partitions)

    all_partitions[[i]] <- partitions
  }

  return(bind_rows(all_partitions))
}

#' Crea amplitude plot stile QIAcuity
#'
#' @param partition_data Data frame da simulate_amplitude_data o dati raw
#' @param channel Channel da visualizzare ("mutant", "wt", o "both")
#' @param color_positive Colore per partizioni positive (default "#1F77B4" blu scuro)
#' @param color_negative Colore per partizioni negative (default "#CCCCCC" grigio)
#' @param facet_by Variabile per faceting (default "sample_id")
#' @return ggplot object
#' @export
plot_amplitude <- function(partition_data,
                          channel = "mutant",
                          color_positive = "#1F77B4",
                          color_negative = "#CCCCCC",
                          color_wt_pos = "#2CA02C",
                          facet_by = "sample_id") {

  if (channel == "mutant") {
    # Plot solo canale mutante
    p <- ggplot(partition_data, aes(x = partition_index, y = mutant_amplitude)) +
      geom_point(aes(color = classification), size = 0.5, alpha = 0.7) +
      scale_color_manual(values = c(
        "Mutant+" = color_positive,
        "WT+" = color_negative,
        "Double Negative" = color_negative
      )) +
      labs(
        title = "Amplitude Plot - Mutant Channel",
        x = "Analyzed Partition",
        y = "Amplitude (RFU)",
        color = "Classification"
      )

  } else if (channel == "wt") {
    # Plot solo canale WT
    p <- ggplot(partition_data, aes(x = partition_index, y = wt_amplitude)) +
      geom_point(aes(color = classification), size = 0.5, alpha = 0.7) +
      scale_color_manual(values = c(
        "Mutant+" = color_negative,
        "WT+" = color_wt_pos,
        "Double Negative" = color_negative
      )) +
      labs(
        title = "Amplitude Plot - WT Channel",
        x = "Analyzed Partition",
        y = "Amplitude (RFU)",
        color = "Classification"
      )

  } else {
    # Plot 2D (mutant vs WT) - scatter 2D
    p <- ggplot(partition_data, aes(x = wt_amplitude, y = mutant_amplitude)) +
      geom_point(aes(color = classification), size = 1, alpha = 0.5) +
      scale_color_manual(values = c(
        "Mutant+" = color_positive,
        "WT+" = color_wt_pos,
        "Double Negative" = color_negative
      )) +
      labs(
        title = "2D Amplitude Plot - Mutant vs WT",
        x = "WT Amplitude (RFU)",
        y = "Mutant Amplitude (RFU)",
        color = "Classification"
      )
  }

  # Aggiungi faceting se richiesto
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_by)), ncol = 5)
  }

  # Theme pulito stile QIAcuity
  p <- p +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "#E8E8E8", color = NA),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray80"),
      legend.position = "bottom"
    )

  return(p)
}

#' Importa dati raw amplitude da file QIAcuity
#'
#' @param file_path Path al file CSV/XLSX con dati raw
#' @param format Formato file ("csv" o "xlsx")
#' @return Data frame con dati amplitude
#' @export
import_amplitude_raw <- function(file_path, format = "csv") {

  if (format == "csv") {
    data <- read.csv(file_path, stringsAsFactors = FALSE)
  } else if (format == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Pacchetto openxlsx necessario per leggere file Excel")
    }
    data <- openxlsx::read.xlsx(file_path)
  } else {
    stop("Formato non supportato. Usa 'csv' o 'xlsx'")
  }

  # Verifica colonne necessarie
  required <- c("sample_id", "partition_index", "mutant_amplitude", "wt_amplitude")

  if (!all(required %in% names(data))) {
    stop(paste("File deve contenere colonne:", paste(required, collapse = ", ")))
  }

  return(data)
}

#' Crea plot combinato con tutti i pannelli stile QIAcuity report
#'
#' @param data Data frame aggregato (conteggi)
#' @param simulate Se TRUE usa simulazione, altrimenti cerca dati raw
#' @param raw_file Path a file dati raw (opzionale)
#' @return ggplot object
#' @export
create_qiacuity_style_plot <- function(data, simulate = TRUE, raw_file = NULL) {

  if (simulate) {
    cat("Generando partizioni simulate...\n")
    partition_data <- simulate_amplitude_data(data)
  } else {
    if (is.null(raw_file)) {
      stop("Specificare raw_file se simulate = FALSE")
    }
    cat("Caricando dati raw da", raw_file, "...\n")
    partition_data <- import_amplitude_raw(raw_file)
  }

  cat("Creando amplitude plot...\n")

  # Plot stile QIAcuity (canale mutante)
  p <- plot_amplitude(partition_data,
                     channel = "mutant",
                     color_positive = "#1F77B4",
                     color_negative = "#CCCCCC")

  return(p)
}
