This README provides an overview of the R scripts and markdown files used for the **GBD 2023 Brain and Central Nervous System Analysis**. These files facilitate the extraction, aggregation, formatting, and visualization of global health estimates, specifically focusing on incidence, mortality, and disability-adjusted life years.

Overview

The scripts in this repository interface with Institute for Health Metrics and Evaluation (IHME) central databases to pull results from the **Global Burden of Disease (GBD) 2023** study. The analysis highlights the disproportionate burden of brain cancer across GBD defined super regions and provides forecasts through 2050.

### **Core Versions Used**

-   **Compare Version:** 8352
-   **CodCorrect (Mortality):** 528
-   **COMO (Incidence):** 1762
-   **Dalynator (DALYs):** 102

## 📂 File Directory

### \*\*1. Analysis & Reporting

-   **`Brain_Table1.md`**: Generates a standardized table showing incident cases, deaths, and age-standardized rates (ASIR/ASMR), including percent changes from 1990–2023.

### **2. Visualization & Mapping**

-   **`Brain_Figure1.md`**: Produces line plots with uncertainty ribbons showing age-specific incidence and mortality rates and counts, stratified by males and females.
-   **`Brain_Figure2.md`**: Script for generating global choropleth maps, specifically binning Age-Standardized DALY rates into quintiles for geographic comparison.
-   **`Brain_Figure3.md`**: Script for generating a three-factor decomposition to visualize the drivers of change in brain and central nervous system cancer DALYs from 1990 to 2023.

### **3. Forecasting**

-   **`Brain_Figure4.md`**: Focuses on Future Health Scenarios (FHS). It formats forecasted incidence and mortality values, including ASIRs and ASMRs from 2023 to 2050, globally and by GBD super-region.

### **4. Sample Data Files (.csv)**

-   **`Brain_Table_1_sample_data`**: Sample input data for Brain_Table_1
-   **`Brain_Figure_1_sample_data`**: Sample input data for Brain_Figure_1
-   **`Brain_Figure_2_sample_data`**: Sample input data for Brain_Figure_2
-   **`Brain_Figure_3_sample_data`**: Sample input data for Brain_Figure_3
-   **`Brain_Figure_4_sample_data`**: Sample input data for Brain_Figure_4

------------------------------------------------------------------------

## 🛠 Setup & Requirements

### **Dependencies**

To run these scripts, you need **R** and the following libraries:

``` r
install.packages(c("data.table", "ggplot2", "dplyr", "matrixStats", "gridExtra", "patchwork", "yaml"))
```

### **System Access**

These scripts utilize IHME central functions (e.g., `get_outputs`, `make_aggregates`). All references to specific individual names or filepaths have been redacted.

------------------------------------------------------------------------

## 📊 Key Statistical Methods

-   **Uncertainty Intervals:** All estimates are reported with 95% Uncertainty Intervals (UIs) derived from the 2.5th and 97.5th percentiles of 1,000 draws.
-   **Age Standardization:** Rates are standardized using the GBD world standard population to allow for cross-country comparisons.
-   **Income Grouping:** Analysis is aggregated by World Bank Income Groups (Low, Lower-Middle, Upper-Middle, High) using location set ID 26 and GBD Super Regions using location set ID 91.
-   **Statistical Note:** These files represent final estimates derived from 1,000 draws. When using these samples, the scripts typically bypass the "draw-level" aggregation and move straight to visualization, as the UIs (upper/lower) are already computed.

------------------------------------------------------------------------

## 🧪 Demo

### **Instructions to run on sample data**

1.  Open any of the `.md` scripts in RStudio.

2.  Locate the data loading section (usually the first code chunk).

3.  Comment out the internal IHME database functions (e.g., `get_outputs()` or `make_aggregates()`).

4.  Replace them with a local file read command using the provided sample files:

    R

    ```         
    # Example for Figure 1 dt <- fread("path/to/Brain_Figure1_sample_data.csv") 
    ```

5.  Click **"Knit"** or run the code chunks sequentially.

### **Expected Output**

-   **Figures**: High-resolution plots (line graphs or maps) showing the burden of Brain cancer by age group or geography.

-   **Tables**: A formatted `.html` or `.csv` table containing means and 95% Uncertainty Intervals.

### **Expected Run Time**

-   **Platform**: Standard desktop computer (8GB RAM, Quad-core processor).

-   **Time**: \< 1 minute. (Since the sample data is already aggregated, the "Knit" process only involves rendering graphics and text).

## Instructions for Use

### **How to run the software on your own data**

To use these scripts with your own custom datasets, ensure your data follows the GBD standard structure or modify the scripts to match your headers:

1.  **Format your Data**: Your input `.csv` must include columns such as:

    -   `val`, `upper`, `lower` (numeric estimates).

    -   `location_name` or `location_id`.

    -   `measure_name` (e.g., "Deaths", "Incidence").

2.  **Modify Global Variables**: At the top of each script, update the `cause_name`, `year`, and `sex_id` variables to match your dataset.

3.  **Adjust Visualization Tiers**: If your data uses different regional groupings (e.g., WHO regions instead of World Bank Income Groups), update the `location_fill` and `location_colors` vectors in the `ggplot` sections to reflect your specific categories.

4.  **Execute**: Run the script. The helper functions `gbd_round_mean` and `gbd_round_val` will automatically format your raw numbers into publication-ready strings with parentheses.
