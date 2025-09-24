# 🦈 Shark Tank Deal Prediction & Strategy (R)

This repository analyzes the **Shark Tank** dataset to (1) predict the likelihood of landing a deal and (2) give **strategy advice** on which Sharks to target and when to pitch. It combines **EDA**, **feature engineering**, and **machine learning** (Decision Trees, Random Forest, GBM), plus a targeted clustering-based strategy module.

---

## 📂 Project Structure

- **`Final_Palummo_Julien.pdf`** → Full report with methodology, results, and visuals.  
- **`shark_tank.csv`** → Dataset of pitches and outcomes.  
- **`shark_tank_analysis.R`** → Main R script: EDA → preprocessing → modeling → strategy module.

> Update file paths inside the R script to match your local setup before running.

---

## ⚙️ Features and Workflow

### 1) Exploratory Data Analysis (EDA)
- Auto-plot for **every column** (histograms for numeric, bar charts for categorical).
- Focused visuals:
  - Deal distribution by **category** (stacked % bars)
  - **Valuation vs deal** scatter, **asked amount vs equity**, **seasonal patterns**
  - Boxplots and histograms for key drivers

### 2) Preprocessing & Feature Engineering
- Convert `deal` → binary (0/1) and `Multiple_Entreprenuers` → binary  
- Create **per-shark presence** indicators (Barbara, Lori, Mark, etc.)  
- **Category grouping** into broader themes: Technology, Food & Beverage, Fashion, Health & Wellness, Entertainment, Home, Other  
- **State extraction** from location → top 5 states one-hot + “Other”  
- Drop non-informative/free-text columns (e.g., description, website)  
- **Percentile outlier trimming** (1%–99%) on numeric features  
- **Correlation heatmap**; remove constant columns

### 3) Modeling
- **Decision Trees (rpart)**  
  - Baseline tree, overfitted tree, optimal cp-selected tree
- **Random Forest (500 trees)**  
  - Variable importance via `%IncMSE` & `IncNodePurity`
- **Gradient Boosting (GBM)**  
  - 10,000 trees, interaction depth = 4  
  - Relative influence table & plot

**Performance (test set):**  
- Accuracy ≈ **53.19%**, F1 ≈ **54.17%**, MSE ≈ **0.272**  
- Most influential predictors include **valuation**, **askedFor**, **episode**, plus specific **shark** and **state** indicators.

### 4) Strategy Module (Targeted Advice)
- Example filter: **Food & Beverage**, **solo entrepreneur**, **California**  
- Compute **per-shark acceptance rates** and rank **top 5 Sharks** to target  
- **K-means on episode** (k=3) to find when in the season deal rates peak  
- Output: **best time in the season** (beginning/middle/end) + **top Sharks** for the filtered profile  
- Probability estimation example using Random Forest on a crafted profile  
  *(Full walkthrough and example results in the report.)* 

---

## 📊 Visualizations
- Auto EDA plots (per-column)
- Deal % by **category**
- **Correlation** heatmap
- Decision tree diagrams (baseline/overfit/optimal)
- **Random Forest** variable importance
- **GBM** relative influence (plot + markdown table)
- **Episode clustering** scatter with deal rates

---

## 🛠️ Tech Stack
- **Language:** R  
- **Core Packages:** `dplyr`, `tidyr`, `ggplot2`, `stringr`, `caret`, `corrplot`, `reshape2`, `rpart`, `rpart.plot`, `randomForest`, `gbm`, `gridExtra`, `Metrics`
