# dPCR Analyzer - Publication Version 📊

## 🎨 New Features

### 1. **Interactive Color Selection**
Choose your plot colors in real-time with the color picker:
- Custom colors for Mutant and WT
- Pre-defined color schemes:
  - Green/Orange (publication-friendly)
  - Blue/Red (classic)
  - Purple/Yellow (high contrast)
  - Viridis (scientific palette)

### 2. **Advanced Plot Types**

**Scatter Plot**
- Shows Mutant vs WT concentration relationship
- 1:1 ratio reference line
- Perfect for identifying allelic imbalances

**Dot Plot (Cleveland)**
- Clean, publication-ready alternative to bar charts
- Shows individual data points
- Easy comparison between samples

**Violin Plot**
- Displays full distribution of concentrations
- Shows data density and spread
- Identifies outliers

**Bubble Chart**
- 3D visualization (Mutant, WT, Fraction)
- Bubble size = mutant fraction percentage
- Color intensity = fraction value

**Heatmap**
- Color-coded concentration patterns
- Identifies clusters in your data
- Great for multi-sample analysis

**Population Distribution**
- 4-quadrant partition analysis
- Shows: Double Negative, WT only, Mutant only, Double Positive
- Stacked bar visualization

### 3. **Customization Options**

**Themes:**
- Publication (Clean) - minimal, professional
- Scientific (Grid) - with reference grid
- Dark Mode - for presentations
- Minimal - ultra-clean

**Typography:**
- Font size: 8-20 pt
- Font family: Arial, Times New Roman, Helvetica, Courier

### 4. **All in English**
Complete translation for international publication

## 🚀 How to Use

### Installation

1. Install dependencies (including new packages):
```r
source("install_dependencies.R")
```

2. Run the new app:
```r
shiny::runApp("app_publication.R")
```

### Quick Start

1. **Setup Tab**: Choose experiment type (Dual-Color recommended)
2. **Data Input**: Load your CSV or enter manually
3. **Analysis**: Click CALCULATE
4. **Results**: View tables and basic plots
5. **Advanced Plots**: Choose your favorite plot type
6. **Plot Settings**: Customize colors and appearance

### Color Selection Workflow

1. Go to **Plot Settings** tab
2. Click on color boxes to open color picker
3. Choose your colors OR click preset buttons
4. See live preview at the bottom
5. All plots update automatically!

### Choosing the Right Plot

**For Publications:**
- Scatter Plot → showing correlation
- Dot Plot → clean comparison
- Violin Plot → showing distribution

**For Presentations:**
- Bubble Chart → impressive 3D effect
- Heatmap → pattern visualization
- Population → detailed breakdown

**For Reports:**
- All plots available as high-resolution PNG downloads
- Use Download buttons on each plot

## 📊 Example Workflow for Publication

1. Load your data from QIAcuity PDF extraction
2. Calculate concentrations
3. Go to **Plot Settings**
4. Select **Publication Theme**
5. Choose **Green/Orange** colors (preset 1)
6. Go to **Advanced Plots**
7. Try each plot type and download your favorites
8. Download results table as CSV

## 🎯 Plot Descriptions

### When to Use Each Plot Type

| Plot Type | Best For | Publication Quality |
|-----------|----------|-------------------|
| Scatter | Correlation analysis | ⭐⭐⭐⭐⭐ |
| Dot Plot | Clean comparisons | ⭐⭐⭐⭐⭐ |
| Violin | Distribution analysis | ⭐⭐⭐⭐ |
| Bubble | 3-variable visualization | ⭐⭐⭐⭐ |
| Heatmap | Pattern detection | ⭐⭐⭐⭐ |
| Population | Detailed breakdown | ⭐⭐⭐ |

## 💡 Tips for Best Results

1. **Colors**: Use high-contrast colors for visibility
2. **Font Size**: 12-14 for screen, 10-11 for publication
3. **Theme**: Publication theme for papers, Dark for presentations
4. **Download**: Always download as PNG at 300 dpi for publications

## 🔧 Troubleshooting

**Color picker not showing?**
- Install colourpicker: `install.packages("colourpicker")`

**Plots not rendering?**
- Check that viridis is installed: `install.packages("viridis")`

**Want to go back to Italian?**
- Use `app_fixed.R` instead of `app_publication.R`

## 📝 Citation

If you use this software in your publication, please acknowledge:
```
Digital PCR analysis performed using dPCR Analyzer Pro v1.0
```

## 🎨 Color Schemes Reference

**Preset 1 - Green/Orange:**
- Mutant: #FF6B35 (Orange)
- WT: #06FFA5 (Green)

**Preset 2 - Blue/Red:**
- Mutant: #E63946 (Red)
- WT: #118AB2 (Blue)

**Preset 3 - Purple/Yellow:**
- Mutant: #7209B7 (Purple)
- WT: #FFD60A (Yellow)

**Preset 4 - Viridis:**
- Mutant: #440154 (Dark Purple)
- WT: #FDE724 (Yellow)

---

**Version**: 1.0 Publication
**Date**: January 2025
**License**: Free for academic use
