# Preview dei nuovi grafici con i tuoi dati estratti
# Esegui questo script per vedere esempi di tutti i plot disponibili

library(plotly)
library(dplyr)
library(tidyr)
library(viridis)

cat("\n")
cat("==================================================\n")
cat("  PREVIEW GRAFICI - dPCR Analyzer Publication\n")
cat("==================================================\n\n")

# Carica i dati estratti dal PDF
data_file <- "examples/qiacuity_extracted_data.csv"

if (!file.exists(data_file)) {
  cat("ERRORE: File non trovato:", data_file, "\n")
  cat("Esegui prima extract_qiacuity_pdf.R per generare i dati\n")
  quit()
}

data <- read.csv(data_file, stringsAsFactors = FALSE)

cat("Dati caricati:", nrow(data), "campioni\n")
cat("Campioni:", paste(data$sample_id, collapse = ", "), "\n\n")

# Calcola concentrazioni (semplificato)
partition_volume <- 0.33  # nL

data <- data %>%
  mutate(
    mutant_negative = total_partitions - mutant_positive,
    mutant_lambda = -log(pmax(mutant_negative/total_partitions, 0.0001)),
    mutant_concentration = (mutant_lambda / partition_volume) * 1000,

    wt_negative = total_partitions - wt_positive,
    wt_lambda = -log(pmax(wt_negative/total_partitions, 0.0001)),
    wt_concentration = (wt_lambda / partition_volume) * 1000,

    mutant_fraction = mutant_concentration / (mutant_concentration + wt_concentration),
    percent_mutant = mutant_fraction * 100
  )

# Colori
color_mutant <- "#FF6B35"  # Orange
color_wt <- "#06FFA5"       # Green

cat("Generazione preview grafici...\n\n")

# ======================
# 1. SCATTER PLOT
# ======================
cat("1. Scatter Plot (Mutant vs WT)\n")
cat("   → Mostra la correlazione tra mutante e WT\n")
cat("   → Punti sopra la linea = più mutante che WT\n\n")

p1 <- plot_ly(data, x = ~wt_concentration, y = ~mutant_concentration,
              text = ~sample_id, type = 'scatter', mode = 'markers',
              marker = list(size = 15, color = color_mutant,
                           line = list(color = 'white', width = 2))) %>%
  add_trace(x = ~wt_concentration, y = ~wt_concentration,
           mode = 'lines', name = '1:1 ratio',
           line = list(dash = 'dash', color = 'gray50', width = 2)) %>%
  layout(title = list(text = "Scatter Plot: Mutant vs WT Concentration",
                     font = list(size = 18)),
         xaxis = list(title = "WT Concentration (copies/µL)"),
         yaxis = list(title = "Mutant Concentration (copies/µL)"),
         font = list(size = 14))

print(p1)

readline(prompt = "\nPremi INVIO per il prossimo grafico...")

# ======================
# 2. DOT PLOT
# ======================
cat("\n2. Dot Plot (Cleveland)\n")
cat("   → Alternativa elegante ai bar plot\n")
cat("   → Perfetto per pubblicazioni\n\n")

plot_data2 <- data %>%
  select(sample_id, mutant_concentration, wt_concentration) %>%
  pivot_longer(cols = c(mutant_concentration, wt_concentration),
               names_to = "type", values_to = "concentration") %>%
  arrange(concentration)

p2 <- plot_ly(plot_data2, y = ~sample_id, x = ~concentration,
              color = ~type, colors = c(color_mutant, color_wt),
              type = 'scatter', mode = 'markers',
              marker = list(size = 12)) %>%
  layout(title = list(text = "Dot Plot: Clean Concentration Comparison",
                     font = list(size = 18)),
         xaxis = list(title = "Concentration (copies/µL)"),
         yaxis = list(title = "Sample ID"),
         font = list(size = 14))

print(p2)

readline(prompt = "\nPremi INVIO per il prossimo grafico...")

# ======================
# 3. VIOLIN PLOT
# ======================
cat("\n3. Violin Plot\n")
cat("   → Mostra la distribuzione completa\n")
cat("   → Include box plot interno\n\n")

plot_data3 <- data %>%
  select(sample_id, mutant_concentration, wt_concentration) %>%
  pivot_longer(cols = c(mutant_concentration, wt_concentration),
               names_to = "type", values_to = "concentration")

p3 <- plot_ly(plot_data3, y = ~concentration, x = ~type,
              color = ~type, colors = c(color_mutant, color_wt),
              type = 'violin', box = list(visible = TRUE),
              meanline = list(visible = TRUE)) %>%
  layout(title = list(text = "Violin Plot: Distribution of Concentrations",
                     font = list(size = 18)),
         xaxis = list(title = "Allele Type"),
         yaxis = list(title = "Concentration (copies/µL)"),
         font = list(size = 14))

print(p3)

readline(prompt = "\nPremi INVIO per il prossimo grafico...")

# ======================
# 4. BUBBLE CHART
# ======================
cat("\n4. Bubble Chart (3D)\n")
cat("   → Visualizza 3 variabili contemporaneamente\n")
cat("   → Dimensione bolla = frazione mutante %\n\n")

p4 <- plot_ly(data, x = ~wt_concentration, y = ~mutant_concentration,
              text = ~paste(sample_id, "<br>Mutant:", round(percent_mutant, 1), "%"),
              size = ~percent_mutant,
              color = ~percent_mutant, colors = viridis(100),
              type = 'scatter', mode = 'markers',
              marker = list(sizemode = 'diameter', sizeref = 2,
                           line = list(color = 'white', width = 2))) %>%
  layout(title = list(text = "Bubble Chart: 3D Visualization",
                     font = list(size = 18)),
         xaxis = list(title = "WT Concentration (copies/µL)"),
         yaxis = list(title = "Mutant Concentration (copies/µL)"),
         font = list(size = 14))

print(p4)

readline(prompt = "\nPremi INVIO per il prossimo grafico...")

# ======================
# 5. HEATMAP
# ======================
cat("\n5. Heatmap\n")
cat("   → Pattern di concentrazione tra campioni\n")
cat("   → Colori = intensità concentrazione\n\n")

heat_data <- data %>%
  select(sample_id, mutant_concentration, wt_concentration) %>%
  pivot_longer(cols = c(mutant_concentration, wt_concentration),
               names_to = "type", values_to = "concentration") %>%
  pivot_wider(names_from = sample_id, values_from = concentration) %>%
  select(-type) %>%
  as.matrix()

rownames(heat_data) <- c("Mutant", "WT")

p5 <- plot_ly(z = heat_data,
              x = colnames(heat_data),
              y = rownames(heat_data),
              type = "heatmap",
              colors = viridis(100),
              colorbar = list(title = "Conc.")) %>%
  layout(title = list(text = "Heatmap: Concentration Patterns",
                     font = list(size = 18)),
         xaxis = list(title = "Sample ID"),
         yaxis = list(title = "Allele Type"),
         font = list(size = 14))

print(p5)

readline(prompt = "\nPremi INVIO per il prossimo grafico...")

# ======================
# 6. POPULATION PLOT
# ======================
cat("\n6. Population Distribution\n")
cat("   → 4 popolazioni di partizioni\n")
cat("   → Double Neg, WT only, Mutant only, Double Pos\n\n")

pop_data <- data %>%
  mutate(
    double_neg = total_partitions - mutant_positive - wt_positive,
    mutant_only = mutant_positive,
    wt_only = wt_positive,
    double_pos = 0  # Assuming no double positives for dPCR
  ) %>%
  select(sample_id, double_neg, wt_only, mutant_only, double_pos) %>%
  pivot_longer(cols = c(double_neg, wt_only, mutant_only, double_pos),
               names_to = "population", values_to = "count")

colors <- c("double_neg" = "gray70",
           "wt_only" = color_wt,
           "mutant_only" = color_mutant,
           "double_pos" = "#FFD700")

labels <- c("double_neg" = "Double Negative",
           "wt_only" = "WT Only",
           "mutant_only" = "Mutant Only",
           "double_pos" = "Double Positive")

pop_data$population <- factor(pop_data$population,
                              levels = c("double_neg", "wt_only", "mutant_only", "double_pos"),
                              labels = labels)

p6 <- plot_ly(pop_data, x = ~sample_id, y = ~count,
              color = ~population, colors = colors,
              type = 'bar') %>%
  layout(title = list(text = "Population Distribution (4 Quadrants)",
                     font = list(size = 18)),
         xaxis = list(title = "Sample ID"),
         yaxis = list(title = "Partition Count"),
         barmode = 'stack',
         font = list(size = 14))

print(p6)

cat("\n")
cat("==================================================\n")
cat("  PREVIEW COMPLETATA!\n")
cat("==================================================\n\n")

cat("Ora puoi:\n")
cat("1. Eseguire l'app completa: shiny::runApp('app_publication.R')\n")
cat("2. Scegliere i tuoi colori preferiti\n")
cat("3. Scaricare i grafici in alta risoluzione\n\n")

cat("LEGENDA GRAFICI:\n")
cat("  • Scatter  → Correlazione, outliers\n")
cat("  • Dot      → Confronto pulito, pubblicazione\n")
cat("  • Violin   → Distribuzione, variabilità\n")
cat("  • Bubble   → 3 variabili, effetto visivo\n")
cat("  • Heatmap  → Pattern, clustering\n")
cat("  • Population → Dettaglio partizioni\n\n")

# Mostra tabella risultati
cat("TABELLA RISULTATI:\n")
cat("=================\n\n")
results_summary <- data %>%
  select(sample_id, sample_type,
         mutant_positive, wt_positive,
         mutant_concentration, wt_concentration,
         percent_mutant) %>%
  mutate(
    mutant_concentration = round(mutant_concentration, 1),
    wt_concentration = round(wt_concentration, 1),
    percent_mutant = round(percent_mutant, 1)
  )

print(results_summary, row.names = FALSE)

cat("\n✓ Preview completata! Pronto per l'app!\n\n")
