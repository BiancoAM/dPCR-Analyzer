# dPCR Analyzer v1.0

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R Shiny](https://img.shields.io/badge/Built%20with-R%20Shiny-blue.svg)](https://shiny.posit.co/)
[![Platform: QIAcuity](https://img.shields.io/badge/Platform-QIAcuity%20(Qiagen)-green.svg)]()
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16767879.svg)](https://doi.org/10.5281/zenodo.16767879)

An open-source R/Shiny application for digital PCR (dPCR) data analysis and publication-quality visualization, optimized for the **QIAcuity** nanoplate system (Qiagen).

---

## Features

- 📊 **Two experiment types**: Standard (Single-Color) and Allele-Specific (Dual-Color)
- 🔢 **Poisson-based concentration calculation** with confidence intervals, CV%, and LOD
- 🔁 **Technical replicate handling**: automatic grouping with Mean ± SD
- 📈 **Publication-ready plots**: bar charts, scatter, dot, violin, bubble, heatmap
- 🎨 **Customizable**: colors, themes, fonts, resolution
- 💾 **Export**: PNG, PDF, SVG at 300 dpi
- 📥 **CSV import** or manual data entry
- 🌐 **No coding required**: fully browser-based interface

---

## Installation

### Requirements

- R (≥ 4.0)
- The following R packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "ggplot2",
  "plotly", "dplyr", "tidyr", "colourpicker", "viridis"
))
```

Or run the included installer:

```r
source("install_dependencies.R")
```

### Run the App

```r
setwd("/path/to/dPCR_Analyzer_v1.0")
shiny::runApp("app_publication.R")
```

> 💡 The app will open automatically in your browser.

> ⚠️ **Windows users**: when you extract the ZIP downloaded from GitHub, it creates a nested folder (`dPCR-Analyzer-main/dPCR-Analyzer-main/`). Make sure to point `setwd()` to the **inner** folder:
> ```r
> setwd("C:/Users/YourName/Downloads/dPCR-Analyzer-main/dPCR-Analyzer-main")
> shiny::runApp("app_publication.R")
> ```

---

## PDF to CSV Converter

If you have a QIAcuity nanoplate PDF report, you can convert it to CSV automatically using the included `pdf_to_csv.R` script:

1. Open `pdf_to_csv.R` in RStudio
2. Edit the two lines at the top:
```r
PDF_PATH    <- "/path/to/your/experiment.pdf"   # your PDF file
TARGET_NAME <- "GENE(c.variant)"                # e.g. "LZTR1(c.742C>T)"
```
3. Save the file (**Cmd+S** on Mac, **Ctrl+S** on Windows)
4. Click **Source** (top right of the editor) — **not** Run
5. The CSV is saved automatically in the same folder as the PDF
6. Import the CSV into the app (Data Input tab → Import CSV)

---

## Quick Start

### 1. Setup Tab
- Set **Partition Volume** (default: 0.33 nL for QIAcuity)
- Set **Total Partitions** (default: 26,000)
- Select **Experiment Type**:
  - *Standard (Single-Color)*: absolute quantification
  - *Allele-Specific (Dual-Color)*: mutant vs wild-type

### 2. Data Input Tab
Upload a CSV file or enter data manually.

**Standard (Single-Color) — required columns:**
```
sample_id, target, positive_partitions, negative_partitions, total_partitions
```

**Allele-Specific (Dual-Color) — required columns:**
```
sample_id, target, mutant_positive, wt_positive, total_partitions
```

> 📂 Ready-to-use example files are in the `examples/` folder.

### 3. Analysis Tab
Click **CALCULATE** to compute concentrations using Poisson statistics.

### 4. Results Tab
- View and download the results table (CSV)
- **Group Technical Replicates**: check if your sample IDs use `_1`, `_2`, `_3` suffixes — plots will show Mean ± SD
- View concentration and allelic fraction plots (dual-color)

### 5. Plot Settings Tab
Customize colors, themes, and fonts for publication.

---


## Example Data

The `examples/` folder contains ready-to-use datasets:

| File | Type | Description |
|------|------|-------------|
| `example_standard_singlecolor.csv` | Standard | Single values, 1-channel |
| `example_standard_triplicates.csv` | Standard | Technical triplicates |
| `example_dualcolor_allelic.csv` | Dual-Color | Mutant vs WT, single values |
| `example_dualcolor_triplicates.csv` | Dual-Color | Mutant vs WT, triplicates |
| `example_dualcolor_mosaicism.csv` | Dual-Color | Somatic mosaicism detection |

---

## Use Case

This tool was developed and validated using dPCR data from a MADD (Multiple Acyl-CoA Dehydrogenase Deficiency) family study, quantifying allelic expression of the *ETFDH* c.1798A>C (p.Asn600His) variant using the QIAcuity One system (Qiagen).

---

## Repository Structure

```
dPCR_Analyzer_v1.0/
├── app_publication.R          # Main Shiny application
├── R/
│   └── concentration_calculation.R  # Core analysis functions
├── examples/                  # Example CSV datasets
├── install_dependencies.R     # Package installer
├── AMPLITUDE_PLOT_GUIDE.md    # Guide for amplitude plots
└── README.md
```

---

## Citation

If you use this software, please cite:

> Bianco AM. *dPCR Analyzer v1.0: an R/Shiny application for digital PCR data analysis and publication-quality visualization*. Zenodo, 2026. doi: [10.5281/zenodo.16767879](https://doi.org/10.5281/zenodo.16767879)

```bibtex
@software{bianco_dpcr_analyzer_2025,
  author    = {Bianco, Anna Monica},
  title     = {dPCR Analyzer v1.0},
  year      = {2026},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.16767879},
  url       = {https://doi.org/10.5281/zenodo.16767879}
}
```

---

## License

This project is licensed under the MIT License — see [LICENSE](LICENSE) for details.

---

## Contact

For questions or feature requests, please open an [Issue](../../issues) on GitHub.
