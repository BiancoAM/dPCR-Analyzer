# Modulo per il calcolo delle concentrazioni in dPCR
# Usa la statistica di Poisson per calcoli accurati

library(dplyr)
library(tibble)

#' Calcola concentrazioni usando la distribuzione di Poisson
#'
#' Per dPCR basata su partizioni, la concentrazione è calcolata come:
#' λ = -ln(partizioni negative / partizioni totali)
#' Concentrazione = λ / volume partizione
#'
#' @param data Data frame con colonne positive_partitions, negative_partitions, total_partitions
#' @param partition_volume Volume di una singola partizione in nanolitri (default 0.33 nL per QIAcuity)
#' @param total_partitions Numero totale di partizioni analizzate (default 26000 per QIAcuity Nanoplate)
#' @param confidence_level Livello di confidenza per intervalli (default 0.95)
#' @return Data frame con concentrazioni calcolate e intervalli di confidenza
#' @export
calculate_concentrations <- function(data,
                                    partition_volume = 0.33,
                                    total_partitions = 26000,
                                    confidence_level = 0.95) {

  # Verifica che i dati abbiano le colonne necessarie
  required_cols <- c("positive_partitions", "negative_partitions", "total_partitions")
  if (!all(required_cols %in% names(data))) {
    stop("Il data frame deve contenere le colonne: ", paste(required_cols, collapse = ", "))
  }

  results <- data %>%
    mutate(
      # Usa total_partitions dal dataset se disponibile, altrimenti usa il parametro
      n_total = ifelse(!is.na(total_partitions), total_partitions, !!total_partitions),
      n_positive = positive_partitions,
      n_negative = negative_partitions,

      # Assicurati che n_negative sia corretto
      n_negative = ifelse(is.na(n_negative), n_total - n_positive, n_negative),

      # Calcola frazione negativa
      fraction_negative = n_negative / n_total,

      # Gestisci casi limite (0 positive o tutte positive)
      fraction_negative = case_when(
        fraction_negative <= 0 ~ 0.0001,  # Evita log(0)
        fraction_negative >= 1 ~ 0.9999,  # Evita log di frazione >= 1
        TRUE ~ fraction_negative
      ),

      # Calcola lambda (numero medio di molecole per partizione)
      lambda = -log(fraction_negative),

      # Calcola concentrazione in copie per nL
      conc_per_nL = lambda / partition_volume,

      # Converti in copie per µL (più comune)
      concentration = conc_per_nL * 1000,

      # Calcola intervalli di confidenza usando Poisson
      # Limiti di confidenza per lambda
      lambda_lower = qchisq((1 - confidence_level) / 2, 2 * n_positive) / (2 * n_total),
      lambda_upper = qchisq(1 - (1 - confidence_level) / 2, 2 * (n_positive + 1)) / (2 * n_total),

      # Converti limiti lambda in concentrazione
      ci_lower = (lambda_lower / partition_volume) * 1000,
      ci_upper = (lambda_upper / partition_volume) * 1000,

      # Calcola coefficiente di variazione (CV)
      cv_percent = (sqrt(lambda) / lambda) * 100,

      # Calcola LOD (Limit of Detection) - tipicamente 3 partizioni positive
      lod = (3 / n_total / partition_volume) * 1000,

      # Flag per concentrazioni sotto LOD
      below_lod = n_positive < 3,

      # Flag per saturazione (troppi positivi, Poisson non valida)
      saturated = fraction_negative < 0.05,

      # Qualità generale
      quality = case_when(
        below_lod ~ "Below LOD",
        saturated ~ "Saturated",
        n_positive < 10 ~ "Low Precision",
        cv_percent > 50 ~ "High CV",
        TRUE ~ "Good"
      )
    ) %>%
    select(
      # Mantieni colonne originali
      any_of(c("sample_id", "target", "filename")),
      # Aggiungi risultati
      n_positive,
      n_negative,
      n_total,
      lambda,
      concentration,
      ci_lower,
      ci_upper,
      cv_percent,
      lod,
      quality,
      below_lod,
      saturated
    )

  return(results)
}

#' Calcola concentrazione con metodo alternativo (frazione diretta)
#'
#' Metodo più semplice ma meno accurato per alte o basse concentrazioni
#'
#' @param positive Numero partizioni positive
#' @param total Numero totale partizioni
#' @param partition_volume Volume partizione in nL
#' @return Concentrazione in copie/µL
#' @export
calculate_concentration_simple <- function(positive, total, partition_volume = 0.33) {
  fraction_positive <- positive / total
  copies_per_partition <- fraction_positive
  concentration <- (copies_per_partition / partition_volume) * 1000
  return(concentration)
}

#' Calcola LOD (Limit of Detection) e LOQ (Limit of Quantification)
#'
#' @param total_partitions Numero totale di partizioni
#' @param partition_volume Volume partizione in nL
#' @param lod_threshold Soglia per LOD (default 3 eventi)
#' @param loq_threshold Soglia per LOQ (default 10 eventi)
#' @return Lista con LOD e LOQ
#' @export
calculate_detection_limits <- function(total_partitions = 26000,
                                       partition_volume = 0.33,
                                       lod_threshold = 3,
                                       loq_threshold = 10) {

  lod <- (lod_threshold / total_partitions / partition_volume) * 1000
  loq <- (loq_threshold / total_partitions / partition_volume) * 1000

  return(list(
    LOD_copies_per_uL = lod,
    LOQ_copies_per_uL = loq,
    LOD_threshold_events = lod_threshold,
    LOQ_threshold_events = loq_threshold
  ))
}

#' Calcola dinamica range (range di linearità)
#'
#' @param total_partitions Numero totale partizioni
#' @param partition_volume Volume partizione in nL
#' @return Lista con range dinamico
#' @export
calculate_dynamic_range <- function(total_partitions = 26000,
                                    partition_volume = 0.33) {

  # Lower limit: 3 positive partitions (LOD)
  lower_limit <- (3 / total_partitions / partition_volume) * 1000

  # Upper limit: 95% partitions negative (evita saturazione)
  # Con 5% positive, lambda ≈ 0.051
  upper_limit <- (-log(0.95) / partition_volume) * 1000

  # Dynamic range in logs
  log_range <- log10(upper_limit / lower_limit)

  return(list(
    lower_limit_copies_per_uL = lower_limit,
    upper_limit_copies_per_uL = upper_limit,
    dynamic_range_logs = log_range
  ))
}

#' Corregge concentrazioni per diluizione
#'
#' @param concentrations Vettore di concentrazioni
#' @param dilution_factor Fattore di diluizione (es. 10 per 1:10)
#' @return Concentrazioni corrette
#' @export
correct_for_dilution <- function(concentrations, dilution_factor) {
  return(concentrations * dilution_factor)
}

#' Calcola numero di copie totali in un campione
#'
#' @param concentration Concentrazione in copie/µL
#' @param volume_uL Volume totale del campione in µL
#' @return Numero totale di copie
#' @export
calculate_total_copies <- function(concentration, volume_uL) {
  return(concentration * volume_uL)
}

#' Converte concentrazione in ng/µL (per DNA)
#'
#' @param concentration Concentrazione in copie/µL
#' @param genome_size_bp Dimensione genoma in base pairs
#' @param avg_bp_weight Peso medio per bp (default 650 Da)
#' @return Concentrazione in ng/µL
#' @export
concentration_copies_to_ng <- function(concentration,
                                       genome_size_bp,
                                       avg_bp_weight = 650) {

  # Peso molecolare del genoma in Dalton
  mw_dalton <- genome_size_bp * avg_bp_weight

  # Converti in grammi per molecola
  grams_per_molecule <- mw_dalton / 6.022e23

  # Concentrazione in g/µL
  conc_g_per_uL <- concentration * grams_per_molecule

  # Converti in ng/µL
  conc_ng_per_uL <- conc_g_per_uL * 1e9

  return(conc_ng_per_uL)
}

#' Calcola precisione attesa per un dato numero di partizioni positive
#'
#' @param n_positive Numero di partizioni positive
#' @param total_partitions Totale partizioni
#' @return CV percentuale atteso
#' @export
calculate_expected_cv <- function(n_positive, total_partitions = 26000) {

  # CV per Poisson = sqrt(n) / n
  lambda <- -log((total_partitions - n_positive) / total_partitions)
  cv <- sqrt(lambda) / lambda * 100

  return(cv)
}

#' Combina replicati tecnici
#'
#' @param data Data frame con replicati (deve avere colonna 'replicate')
#' @param group_vars Variabili per raggruppare (es. c("sample_id", "target"))
#' @return Data frame con medie e SD dei replicati
#' @export
combine_technical_replicates <- function(data, group_vars = c("sample_id", "target")) {

  summarized <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_replicates = n(),
      mean_concentration = mean(concentration, na.rm = TRUE),
      sd_concentration = sd(concentration, na.rm = TRUE),
      cv_replicates = (sd_concentration / mean_concentration) * 100,
      mean_positive = mean(n_positive, na.rm = TRUE),
      mean_total = mean(n_total, na.rm = TRUE),
      .groups = 'drop'
    )

  return(summarized)
}

#' Calcola concentrazioni per esperimenti dual-color (allele-specific)
#'
#' Per dPCR dual-color, calcola concentrazioni separate per mutante e WT
#' e calcola la frazione allelica
#'
#' @param data Data frame con colonne mutant_positive, wt_positive, total_partitions
#' @param partition_volume Volume di una singola partizione in nanolitri (default 0.33 nL)
#' @return Data frame con concentrazioni e frazioni alleliche
#' @export
calculate_dual_color_concentrations <- function(data, partition_volume = 0.33) {

  # Verifica colonne necessarie
  required_cols <- c("mutant_positive", "wt_positive", "total_partitions")
  if (!all(required_cols %in% names(data))) {
    stop("Il data frame deve contenere le colonne: ", paste(required_cols, collapse = ", "))
  }

  results <- data %>%
    mutate(
      # Calcola partizioni negative per ogni canale
      mutant_negative = total_partitions - mutant_positive,
      wt_negative = total_partitions - wt_positive,

      # Calcola lambda per mutante
      mutant_lambda = -log(pmax(mutant_negative / total_partitions, 0.0001)),
      mutant_concentration = (mutant_lambda / partition_volume) * 1000,

      # Calcola lambda per WT
      wt_lambda = -log(pmax(wt_negative / total_partitions, 0.0001)),
      wt_concentration = (wt_lambda / partition_volume) * 1000,

      # Calcola frazione allelica
      total_concentration = mutant_concentration + wt_concentration,
      mutant_fraction = mutant_concentration / total_concentration,
      wt_fraction = wt_concentration / total_concentration,

      # Converti in percentuale
      percent_mutant = mutant_fraction * 100,
      percent_wt = wt_fraction * 100,

      # Calcola CV per ogni canale
      mutant_cv = (sqrt(mutant_lambda) / mutant_lambda) * 100,
      wt_cv = (sqrt(wt_lambda) / wt_lambda) * 100,

      # Flag qualità
      mutant_low = mutant_positive < 3,
      wt_low = wt_positive < 3
    )

  return(results)
}

#' Identifica e raggruppa replicati tecnici
#'
#' Riconosce replicati dal suffisso _1, _2, _3 nel sample_id
#' e calcola media e deviazione standard
#'
#' @param data Data frame con risultati
#' @return Data frame con media, SD, SE e numero replicati
#' @export
summarize_replicates <- function(data) {

  # Estrai nome base rimuovendo _1, _2, _3, etc.
  data <- data %>%
    mutate(base_name = gsub("_\\d+$", "", sample_id))

  # Aggiungi sample_type se non presente (per compatibilità con CSV)
  if (!"sample_type" %in% names(data)) {
    data$sample_type <- "sample"
  }
  if (!"target" %in% names(data)) {
    data$target <- "unknown"
  }

  # Conta quanti replicati per ogni base_name
  replicate_counts <- data %>%
    group_by(base_name) %>%
    summarise(n_replicates = n(), .groups = 'drop')

  # Se ci sono replicati (n > 1), calcola statistiche
  summarized <- data %>%
    group_by(base_name, sample_type, target) %>%
    summarise(
      n_replicates = n(),

      # Concentrazioni
      mutant_concentration_mean = mean(mutant_concentration, na.rm = TRUE),
      mutant_concentration_sd = sd(mutant_concentration, na.rm = TRUE),
      mutant_concentration_se = sd(mutant_concentration, na.rm = TRUE) / sqrt(n()),

      wt_concentration_mean = mean(wt_concentration, na.rm = TRUE),
      wt_concentration_sd = sd(wt_concentration, na.rm = TRUE),
      wt_concentration_se = sd(wt_concentration, na.rm = TRUE) / sqrt(n()),

      # Frazioni
      percent_mutant_mean = mean(percent_mutant, na.rm = TRUE),
      percent_mutant_sd = sd(percent_mutant, na.rm = TRUE),
      percent_mutant_se = sd(percent_mutant, na.rm = TRUE) / sqrt(n()),

      # Partizioni (media)
      mutant_positive = round(mean(mutant_positive, na.rm = TRUE)),
      wt_positive = round(mean(wt_positive, na.rm = TRUE)),
      total_partitions = round(mean(total_partitions, na.rm = TRUE)),

      .groups = 'drop'
    ) %>%
    # Sostituisci NA con 0 per SD quando c'è solo 1 replicato
    mutate(
      mutant_concentration_sd = ifelse(is.na(mutant_concentration_sd), 0, mutant_concentration_sd),
      wt_concentration_sd = ifelse(is.na(wt_concentration_sd), 0, wt_concentration_sd),
      percent_mutant_sd = ifelse(is.na(percent_mutant_sd), 0, percent_mutant_sd),
      mutant_concentration_se = ifelse(is.na(mutant_concentration_se), 0, mutant_concentration_se),
      wt_concentration_se = ifelse(is.na(wt_concentration_se), 0, wt_concentration_se),
      percent_mutant_se = ifelse(is.na(percent_mutant_se), 0, percent_mutant_se)
    )

  return(summarized)
}
