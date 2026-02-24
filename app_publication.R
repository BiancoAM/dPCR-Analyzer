# Digital PCR Analyzer - Publication Version
# With advanced interactive plots and customizable colors

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(colourpicker)  # For color selection
library(viridis)

# Source modules
source("R/concentration_calculation.R")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "dPCR Analyzer Pro"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Setup", tabName = "setup", icon = icon("cog")),
      menuItem("Data Input", tabName = "input", icon = icon("keyboard")),
      menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Advanced Plots", tabName = "plots", icon = icon("chart-line")),
      menuItem("Plot Settings", tabName = "settings", icon = icon("palette"))
    )
  ),

  dashboardBody(
    tabItems(
      # Setup Tab
      tabItem(tabName = "setup",
        fluidRow(
          box(title = "Instrument Parameters", width = 6,
            numericInput("partition_volume", "Partition Volume (nL)",
                         value = 0.33, min = 0.01, step = 0.01),
            numericInput("total_partitions", "Total Partitions",
                         value = 26000, min = 1000, step = 1000)
          ),
          box(title = "Experiment Type", width = 6,
            radioButtons("exp_type", NULL,
              choices = c("Standard (Single-Color)" = "standard",
                         "Allele-Specific (Dual-Color)" = "dual_color"),
              selected = "dual_color")
          )
        )
      ),

      # Data Input Tab
      tabItem(tabName = "input",
        fluidRow(
          box(title = "Input Mode", width = 12,
            radioButtons("input_mode", NULL,
              choices = c("Import CSV" = "csv", "Manual Input" = "manual"),
              inline = TRUE)
          )
        ),

        # CSV Import
        conditionalPanel(
          condition = "input.input_mode == 'csv'",
          fluidRow(
            box(title = "CSV Import", width = 12,
              fileInput("csv_file", "Select CSV File",
                       accept = ".csv"),
              conditionalPanel(
                condition = "input.exp_type == 'standard'",
                p("Required columns: sample_id, target, positive_partitions, negative_partitions, total_partitions")
              ),
              conditionalPanel(
                condition = "input.exp_type == 'dual_color'",
                p("Required columns: sample_id, target, mutant_positive, wt_positive, total_partitions")
              ),
              hr(),
              actionButton("load_csv", "Load Data", class = "btn-primary"),
              verbatimTextOutput("csv_status")
            )
          )
        ),

        # Manual Input
        conditionalPanel(
          condition = "input.input_mode == 'manual'",
          fluidRow(
            box(title = "Manual Sample Entry", width = 6,
              textInput("man_sample_id", "Sample ID*"),
              selectInput("man_sample_type", "Sample Type",
                choices = c("sample", "pos_ctrl", "neg_ctrl", "reference")),
              textInput("man_target", "Target/Gene*"),
              numericInput("man_total", "Total Partitions", value = 26000)
            ),
            box(title = "Partitions", width = 6,
              conditionalPanel(
                condition = "input.exp_type == 'standard'",
                numericInput("man_positive", "Positive Partitions*", value = NULL),
                numericInput("man_negative", "Negative Partitions", value = NULL)
              ),
              conditionalPanel(
                condition = "input.exp_type == 'dual_color'",
                numericInput("man_mutant_pos", "Mutant Positive*", value = NULL),
                numericInput("man_wt_pos", "WT Positive*", value = NULL)
              ),
              textInput("man_notes", "Notes"),
              br(),
              actionButton("add_manual", "Add Sample", class = "btn-success")
            )
          )
        ),

        # Loaded data table
        fluidRow(
          box(title = "Loaded Data", width = 12,
            actionButton("clear_all", "Clear All", class = "btn-warning"),
            hr(),
            DTOutput("data_loaded_table"),
            hr(),
            verbatimTextOutput("data_summary")
          )
        )
      ),

      # Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(title = "Calculate Concentrations", width = 12,
            actionButton("calculate", "CALCULATE", class = "btn-primary btn-lg"),
            hr(),
            verbatimTextOutput("calc_status")
          )
        )
      ),

      # Results Tab
      tabItem(tabName = "results",
        fluidRow(
          box(title = "Results Table", width = 12,
            downloadButton("download_results", "Download Results (CSV)", class = "btn-success"),
            hr(),
            DTOutput("results_table")
          )
        ),

        # Replicate grouping option
        fluidRow(
          box(title = "Replicate Options", width = 12,
            checkboxInput("group_replicates",
                         "Group Technical Replicates (e.g., Sample_1, Sample_2, Sample_3)",
                         value = FALSE),
            p(style = "font-size: 11px; color: #666;",
              "Check this box if your data contains technical replicates with _1, _2, _3 suffixes. ",
              "When enabled, plots will show mean values with error bars (SD).")
          )
        ),


        # Standard results
        conditionalPanel(
          condition = "input.exp_type == 'standard'",
          fluidRow(
            box(title = "Concentration Plot", width = 12,
              downloadButton("download_std_plot", "Download Plot (PNG)", class = "btn-info"),
              hr(),
              plotlyOutput("standard_conc_plot", height = "400px")
            )
          )
        ),

        # Dual-color results
        conditionalPanel(
          condition = "input.exp_type == 'dual_color'",
          fluidRow(
            box(title = "Allelic Fractions", width = 12,
              DTOutput("fraction_table")
            )
          ),
          fluidRow(
            box(title = "Mutant vs WT Concentrations", width = 12,
              downloadButton("download_conc_plot", "Download Plot (PNG)", class = "btn-info"),
              hr(),
              plotlyOutput("dual_conc_plot", height = "400px")
            )
          ),
          fluidRow(
            box(title = "Allelic Fraction (%)", width = 12,
              downloadButton("download_frac_plot", "Download Plot (PNG)", class = "btn-info"),
              hr(),
              plotlyOutput("fraction_plot", height = "400px")
            )
          )
        )
      ),

      # Advanced Plots Tab
      tabItem(tabName = "plots",
        conditionalPanel(
          condition = "input.exp_type == 'dual_color'",

          fluidRow(
            box(title = "Plot Type Selection", width = 12,
              selectInput("plot_type", "Choose Plot Type:",
                choices = c(
                  "Scatter Plot (Mutant vs WT)" = "scatter",
                  "Dot Plot (Cleveland)" = "dot",
                  "Violin Plot (Distribution)" = "violin",
                  "Bubble Chart (3D)" = "bubble",
                  "Heatmap (Pattern)" = "heatmap",
                  "Population Distribution" = "population"
                ),
                selected = "scatter"
              )
            )
          ),

          fluidRow(
            box(title = "Interactive Plot", width = 12,
              downloadButton("download_advanced_plot", "Download Plot (PNG)", class = "btn-info"),
              hr(),
              plotlyOutput("advanced_plot", height = "600px")
            )
          ),

          fluidRow(
            box(title = "Plot Description", width = 12,
              uiOutput("plot_description")
            )
          )
        )
      ),

      # Plot Settings Tab
      tabItem(tabName = "settings",
        fluidRow(
          box(title = "Color Settings", width = 6,
            h4("Choose Your Colors:"),
            colourInput("color_mutant", "Mutant Color:", value = "#FF6B35",
                       palette = "limited", allowedCols = c("#FF6B35", "#E63946", "#D62828", "#9D4EDD", "#7209B7")),
            colourInput("color_wt", "WT Color:", value = "#4ECDC4",
                       palette = "limited", allowedCols = c("#4ECDC4", "#06FFA5", "#2EC4B6", "#06D6A0", "#118AB2")),
            hr(),
            h4("Color Presets:"),
            actionButton("preset_1", "Green/Orange", class = "btn-sm"),
            actionButton("preset_2", "Blue/Red", class = "btn-sm"),
            actionButton("preset_3", "Purple/Yellow", class = "btn-sm"),
            actionButton("preset_4", "Viridis", class = "btn-sm")
          ),
          box(title = "Plot Themes", width = 6,
            selectInput("theme", "Plot Theme:",
              choices = c(
                "Publication (Clean)" = "publication",
                "Scientific (Grid)" = "scientific",
                "Dark Mode" = "dark",
                "Minimal" = "minimal"
              ),
              selected = "publication"
            ),
            hr(),
            h4("Font Settings:"),
            numericInput("font_size", "Font Size:", value = 12, min = 8, max = 20),
            selectInput("font_family", "Font Family:",
              choices = c("Arial", "Times New Roman", "Helvetica", "Courier"),
              selected = "Arial"
            )
          )
        ),

        fluidRow(
          box(title = "Preview", width = 12,
            plotlyOutput("color_preview", height = "300px")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    results = NULL,
    color_mutant = "#FF6B35",
    color_wt = "#4ECDC4"
  )

  # Color presets
  observeEvent(input$preset_1, {
    updateColourInput(session, "color_mutant", value = "#FF6B35")  # Orange
    updateColourInput(session, "color_wt", value = "#06FFA5")      # Green
  })

  observeEvent(input$preset_2, {
    updateColourInput(session, "color_mutant", value = "#E63946")  # Red
    updateColourInput(session, "color_wt", value = "#118AB2")      # Blue
  })

  observeEvent(input$preset_3, {
    updateColourInput(session, "color_mutant", value = "#7209B7")  # Purple
    updateColourInput(session, "color_wt", value = "#FFD60A")      # Yellow
  })

  observeEvent(input$preset_4, {
    updateColourInput(session, "color_mutant", value = "#440154")  # Viridis purple
    updateColourInput(session, "color_wt", value = "#FDE724")      # Viridis yellow
  })

  # Update reactive colors
  observe({
    rv$color_mutant <- input$color_mutant
    rv$color_wt <- input$color_wt
  })

  # CSV Import
  observeEvent(input$load_csv, {
    req(input$csv_file)

    tryCatch({
      csv_data <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)

      if (input$exp_type == "standard") {
        required <- c("sample_id", "target", "positive_partitions", "negative_partitions", "total_partitions")
      } else {
        required <- c("sample_id", "target", "mutant_positive", "wt_positive", "total_partitions")
      }

      if (!all(required %in% names(csv_data))) {
        output$csv_status <- renderText({
          paste("ERROR: CSV must contain:", paste(required, collapse = ", "))
        })
        return()
      }

      rv$data <- csv_data
      output$csv_status <- renderText({
        paste("✓ Loaded", nrow(csv_data), "samples")
      })
      showNotification("Data loaded successfully!", type = "message")

    }, error = function(e) {
      output$csv_status <- renderText({
        paste("ERROR:", e$message)
      })
    })
  })

  # Manual Input
  observeEvent(input$add_manual, {
    req(input$man_sample_id, input$man_target)

    if (input$exp_type == "dual_color") {
      req(input$man_mutant_pos, input$man_wt_pos)

      new_row <- data.frame(
        sample_id = input$man_sample_id,
        sample_type = input$man_sample_type,
        target = input$man_target,
        mutant_positive = input$man_mutant_pos,
        wt_positive = input$man_wt_pos,
        total_partitions = input$man_total,
        notes = ifelse(is.null(input$man_notes), "", input$man_notes),
        stringsAsFactors = FALSE
      )
    } else {
      req(input$man_positive)

      new_row <- data.frame(
        sample_id = input$man_sample_id,
        sample_type = input$man_sample_type,
        target = input$man_target,
        positive_partitions = input$man_positive,
        negative_partitions = ifelse(is.null(input$man_negative),
                                     input$man_total - input$man_positive,
                                     input$man_negative),
        total_partitions = input$man_total,
        notes = ifelse(is.null(input$man_notes), "", input$man_notes),
        stringsAsFactors = FALSE
      )
    }

    if (is.null(rv$data)) {
      rv$data <- new_row
    } else {
      rv$data <- rbind(rv$data, new_row)
    }

    showNotification("Sample added!", type = "message")
  })

  # Clear all
  observeEvent(input$clear_all, {
    rv$data <- NULL
    rv$results <- NULL
    showNotification("All data cleared", type = "warning")
  })

  # Data loaded table
  output$data_loaded_table <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 10))
  })

  # Data summary
  output$data_summary <- renderText({
    if (is.null(rv$data)) {
      "No data loaded"
    } else {
      paste("Total samples:", nrow(rv$data),
            "\nTargets:", paste(unique(rv$data$target), collapse = ", "))
    }
  })

  # Calculate concentrations
  observeEvent(input$calculate, {
    req(rv$data)

    tryCatch({
      if (input$exp_type == "dual_color") {
        rv$results <- calculate_dual_color_concentrations(
          rv$data,
          partition_volume = input$partition_volume
        )
      } else {
        rv$results <- calculate_concentrations(
          rv$data,
          partition_volume = input$partition_volume
        )
      }

      output$calc_status <- renderText({
        "✓ Calculations completed successfully!"
      })
      showNotification("Analysis complete!", type = "message")

    }, error = function(e) {
      output$calc_status <- renderText({
        paste("ERROR:", e$message)
      })
    })
  })

  # Results table
  output$results_table <- renderDT({
    req(rv$results)
    dt <- datatable(rv$results, options = list(pageLength = 10))
    # Format only columns that exist (dual-color vs standard)
    if (input$exp_type == "dual_color") {
      dt <- dt %>% formatRound(columns = c("mutant_concentration", "wt_concentration"), digits = 2)
    } else {
      dt <- dt %>% formatRound(columns = c("concentration"), digits = 2)
    }
    dt
  })

  # Fraction table (solo dual-color)
  output$fraction_table <- renderDT({
    req(rv$results)
    req(input$exp_type == "dual_color")

    # Aggiungi sample_type se non presente
    results <- rv$results
    if (!"sample_type" %in% names(results)) results$sample_type <- "sample"

    frac_data <- results %>%
      select(sample_id, target, mutant_fraction, percent_mutant) %>%
      mutate(
        mutant_fraction = round(mutant_fraction, 4),
        percent_mutant = round(percent_mutant, 2)
      )

    datatable(frac_data, options = list(pageLength = 10))
  })

  # Standard single-color concentration plot
  output$standard_conc_plot <- renderPlotly({
    req(rv$results)
    req(input$exp_type == "standard")

    if (input$group_replicates) {
      # Group replicates: calcola mean ± SD
      plot_data <- rv$results %>%
        mutate(base_name = gsub("_\\d+$", "", sample_id)) %>%
        group_by(base_name) %>%
        summarise(
          conc_mean = mean(concentration, na.rm = TRUE),
          conc_sd   = sd(concentration,   na.rm = TRUE),
          n         = n(),
          .groups   = "drop"
        ) %>%
        mutate(conc_sd = ifelse(is.na(conc_sd), 0, conc_sd))

      p <- ggplot(plot_data, aes(x = base_name, y = conc_mean)) +
        geom_bar(stat = "identity", fill = rv$color_mutant, width = 0.6) +
        geom_errorbar(aes(ymin = pmax(conc_mean - conc_sd, 0),
                          ymax = conc_mean + conc_sd),
                      width = 0.25) +
        labs(title = "Concentration (Mean ± SD)",
             x = "Sample", y = "Concentration (copies/µL)") +
        theme_minimal(base_size = input$font_size) +
        theme(axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y  = element_text(face = "bold"),
              axis.title   = element_text(face = "bold"),
              plot.title   = element_text(face = "bold"),
              text         = element_text(family = input$font_family))
    } else {
      p <- ggplot(rv$results, aes(x = sample_id, y = concentration)) +
        geom_bar(stat = "identity", fill = rv$color_mutant, width = 0.6) +
        labs(title = "Concentration (copies/µL)",
             x = "Sample ID", y = "Concentration (copies/µL)") +
        theme_minimal(base_size = input$font_size) +
        theme(axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y  = element_text(face = "bold"),
              axis.title   = element_text(face = "bold"),
              plot.title   = element_text(face = "bold"),
              text         = element_text(family = input$font_family))
    }

    ggplotly(p)
  })

  # Dual-color concentration plot
  output$dual_conc_plot <- renderPlotly({
    req(rv$results)

    # Use checkbox to determine if replicate grouping should be applied
    if (input$group_replicates) {
      # Summarize replicates with mean and SD
      summarized <- summarize_replicates(rv$results)

      plot_data <- summarized %>%
        select(base_name, mutant_concentration_mean, mutant_concentration_sd,
               wt_concentration_mean, wt_concentration_sd, n_replicates) %>%
        pivot_longer(cols = c(mutant_concentration_mean, wt_concentration_mean),
                     names_to = "type", values_to = "concentration") %>%
        mutate(
          sd = ifelse(grepl("mutant", type), mutant_concentration_sd, wt_concentration_sd),
          type = gsub("_mean", "", type)
        )

      p <- ggplot(plot_data, aes(x = base_name, y = concentration, fill = type)) +
        geom_bar(stat = "identity", position = position_dodge(0.9)) +
        geom_errorbar(aes(ymin = pmax(concentration - sd, 0), ymax = concentration + sd),
                     position = position_dodge(0.9), width = 0.25) +
        scale_fill_manual(values = c("mutant_concentration" = rv$color_mutant,
                                     "wt_concentration" = rv$color_wt),
                         labels = c("Mutant", "WT")) +
        labs(title = "Mutant vs WT Concentrations (Mean ± SD)",
             x = "Sample",
             y = "Concentration (copies/µL)",
             fill = "Allele") +
        theme_minimal(base_size = input$font_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              axis.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold"),
              legend.text = element_text(face = "bold"),
              text = element_text(family = input$font_family))

    } else {
      # No replicates - use original data
      plot_data <- rv$results %>%
        select(sample_id, mutant_concentration, wt_concentration) %>%
        pivot_longer(cols = c(mutant_concentration, wt_concentration),
                     names_to = "type", values_to = "concentration")

      p <- ggplot(plot_data, aes(x = sample_id, y = concentration, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("mutant_concentration" = rv$color_mutant,
                                     "wt_concentration" = rv$color_wt),
                         labels = c("Mutant", "WT")) +
        labs(title = "Mutant vs WT Concentrations",
             x = "Sample ID",
             y = "Concentration (copies/µL)",
             fill = "Allele") +
        theme_minimal(base_size = input$font_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              axis.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold"),
              legend.text = element_text(face = "bold"),
              text = element_text(family = input$font_family))
    }

    ggplotly(p)
  })

  # Fraction plot
  output$fraction_plot <- renderPlotly({
    req(rv$results)

    # Use checkbox to determine if replicate grouping should be applied
    if (input$group_replicates) {
      # Summarize replicates with mean and SD
      summarized <- summarize_replicates(rv$results)

      p <- ggplot(summarized, aes(x = base_name, y = percent_mutant_mean)) +
        geom_bar(stat = "identity", fill = rv$color_mutant) +
        geom_errorbar(aes(ymin = pmax(percent_mutant_mean - percent_mutant_sd, 0),
                         ymax = pmin(percent_mutant_mean + percent_mutant_sd, 100)),
                     width = 0.25) +
        geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
        labs(title = "Mutant Allelic Fraction (Mean ± SD)",
             x = "Sample",
             y = "Mutant Fraction (%)") +
        theme_minimal(base_size = input$font_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              axis.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold"),
              text = element_text(family = input$font_family))

    } else {
      # No replicates - use original data
      p <- ggplot(rv$results, aes(x = sample_id, y = percent_mutant)) +
        geom_bar(stat = "identity", fill = rv$color_mutant) +
        geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
        labs(title = "Mutant Allelic Fraction",
             x = "Sample ID",
             y = "Mutant Fraction (%)") +
        theme_minimal(base_size = input$font_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y = element_text(face = "bold"),
              axis.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold"),
              text = element_text(family = input$font_family))
    }

    ggplotly(p)
  })

  # Advanced plots
  output$advanced_plot <- renderPlotly({
    req(rv$results)

    if (input$plot_type == "scatter") {
      # Scatter plot: Mutant vs WT
      p <- plot_ly(rv$results, x = ~wt_concentration, y = ~mutant_concentration,
                   text = ~sample_id, type = 'scatter', mode = 'markers',
                   marker = list(size = 12, color = rv$color_mutant,
                                line = list(color = 'white', width = 1))) %>%
        add_trace(x = ~wt_concentration, y = ~wt_concentration,
                 mode = 'lines', name = '1:1 ratio',
                 line = list(dash = 'dash', color = 'gray')) %>%
        layout(title = "Mutant vs WT Concentration",
               xaxis = list(title = "WT Concentration (copies/µL)"),
               yaxis = list(title = "Mutant Concentration (copies/µL)"),
               font = list(size = input$font_size, family = input$font_family))

    } else if (input$plot_type == "dot") {
      # Dot plot (Cleveland plot)
      plot_data <- rv$results %>%
        select(sample_id, mutant_concentration, wt_concentration) %>%
        pivot_longer(cols = c(mutant_concentration, wt_concentration),
                     names_to = "type", values_to = "concentration") %>%
        arrange(concentration)

      p <- plot_ly(plot_data, y = ~sample_id, x = ~concentration,
                   color = ~type, colors = c(rv$color_mutant, rv$color_wt),
                   type = 'scatter', mode = 'markers',
                   marker = list(size = 10)) %>%
        layout(title = "Dot Plot - Concentration Comparison",
               xaxis = list(title = "Concentration (copies/µL)"),
               yaxis = list(title = "Sample ID"),
               font = list(size = input$font_size, family = input$font_family))

    } else if (input$plot_type == "violin") {
      # Violin plot
      plot_data <- rv$results %>%
        select(sample_id, mutant_concentration, wt_concentration) %>%
        pivot_longer(cols = c(mutant_concentration, wt_concentration),
                     names_to = "type", values_to = "concentration")

      p <- plot_ly(plot_data, y = ~concentration, x = ~type,
                   color = ~type, colors = c(rv$color_mutant, rv$color_wt),
                   type = 'violin', box = list(visible = TRUE)) %>%
        layout(title = "Distribution of Concentrations",
               xaxis = list(title = "Allele Type"),
               yaxis = list(title = "Concentration (copies/µL)"),
               font = list(size = input$font_size, family = input$font_family))

    } else if (input$plot_type == "bubble") {
      # Bubble chart (mutant, WT, fraction)
      p <- plot_ly(rv$results, x = ~wt_concentration, y = ~mutant_concentration,
                   text = ~sample_id, size = ~percent_mutant,
                   color = ~percent_mutant, colors = viridis(100),
                   type = 'scatter', mode = 'markers',
                   marker = list(sizemode = 'diameter', sizeref = 2,
                                line = list(color = 'white', width = 1))) %>%
        layout(title = "Bubble Chart - 3D Visualization",
               xaxis = list(title = "WT Concentration (copies/µL)"),
               yaxis = list(title = "Mutant Concentration (copies/µL)"),
               font = list(size = input$font_size, family = input$font_family))

    } else if (input$plot_type == "heatmap") {
      # Heatmap
      heat_data <- rv$results %>%
        select(sample_id, mutant_concentration, wt_concentration) %>%
        column_to_rownames("sample_id") %>%
        as.matrix() %>%
        t()

      p <- plot_ly(z = heat_data, x = colnames(heat_data), y = rownames(heat_data),
                   type = "heatmap", colors = viridis(100)) %>%
        layout(title = "Heatmap - Concentration Patterns",
               xaxis = list(title = "Sample ID"),
               yaxis = list(title = "Allele Type"),
               font = list(size = input$font_size, family = input$font_family))

    } else if (input$plot_type == "population") {
      # Population distribution (4 quadrants)
      pop_data <- rv$results %>%
        mutate(
          double_neg = total_partitions - mutant_positive - wt_positive,
          mutant_only = mutant_positive,
          wt_only = wt_positive,
          double_pos = 0
        ) %>%
        select(sample_id, double_neg, wt_only, mutant_only, double_pos) %>%
        pivot_longer(cols = c(double_neg, wt_only, mutant_only, double_pos),
                     names_to = "population", values_to = "count")

      colors <- c("double_neg" = "gray80",
                 "wt_only" = rv$color_wt,
                 "mutant_only" = rv$color_mutant,
                 "double_pos" = "#FFD700")

      p <- plot_ly(pop_data, x = ~sample_id, y = ~count, color = ~population,
                   colors = colors, type = 'bar') %>%
        layout(title = "Population Distribution (4 Quadrants)",
               xaxis = list(title = "Sample ID"),
               yaxis = list(title = "Partition Count"),
               barmode = 'stack',
               font = list(size = input$font_size, family = input$font_family))
    }

    p
  })

  # Plot descriptions
  output$plot_description <- renderUI({
    descriptions <- list(
      scatter = HTML("<b>Scatter Plot:</b> Shows the relationship between Mutant and WT concentrations. Points above the dashed line indicate higher mutant than WT. Perfect for identifying samples with different allelic ratios."),
      dot = HTML("<b>Dot Plot (Cleveland):</b> A clean, publication-ready alternative to bar charts. Shows individual data points for easy comparison between samples and alleles."),
      violin = HTML("<b>Violin Plot:</b> Displays the full distribution of concentration values. The width shows the probability density. Great for showing data spread and outliers."),
      bubble = HTML("<b>Bubble Chart:</b> 3D visualization where bubble size represents mutant fraction percentage. Color intensity also indicates the fraction. Perfect for showing three variables at once."),
      heatmap = HTML("<b>Heatmap:</b> Color-coded matrix showing concentration patterns across samples. Useful for identifying clusters and patterns in your data."),
      population = HTML("<b>Population Distribution:</b> Shows the 4 partition populations: Double Negative (gray), WT only (cyan), Mutant only (orange), and Double Positive (gold).")
    )

    box(
      width = 12,
      descriptions[[input$plot_type]]
    )
  })

  # Color preview
  output$color_preview <- renderPlotly({
    preview_data <- data.frame(
      type = c("Mutant", "WT"),
      value = c(100, 80)
    )

    p <- plot_ly(preview_data, x = ~type, y = ~value, type = 'bar',
                 marker = list(color = c(rv$color_mutant, rv$color_wt))) %>%
      layout(title = "Color Preview",
             xaxis = list(title = ""),
             yaxis = list(title = "Value"),
             font = list(size = input$font_size, family = input$font_family))

    p
  })

  # Download handlers
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("dPCR_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$results, file, row.names = FALSE)
    }
  )

  output$download_conc_plot <- downloadHandler(
    filename = function() {
      paste0("concentration_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- ggplot(rv$results %>%
                   select(sample_id, mutant_concentration, wt_concentration) %>%
                   pivot_longer(cols = c(mutant_concentration, wt_concentration),
                                names_to = "type", values_to = "concentration"),
                 aes(x = sample_id, y = concentration, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("mutant_concentration" = rv$color_mutant,
                                     "wt_concentration" = rv$color_wt)) +
        theme_minimal()
      ggsave(file, p, width = 10, height = 6, dpi = 300)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)
