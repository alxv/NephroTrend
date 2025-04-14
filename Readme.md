# NephroTrend

## A Deep Learning Framework for Worsening Renal Function Monitoring in Heart Failure Patients

This repository contains the code, clinical code lists, and R scripts for the Shiny application developed for the NephroTrend project.

## Overview

NephroTrend is a deep learning framework designed to predict serum creatinine trajectories in heart failure patients. The model aims to improve clinical decision-making by providing accurate forecasts of renal function changes, which can significantly impact heart failure outcomes.

The framework was developed and validated using electronic health record data from the Clinical Practice Research Datalink (CPRD) Aurum, with a cohort of 178,568 patients with heart failure. A novel geographical validation strategy was implemented to ensure generalizability across diverse populations.

## Repository Contents

- **`codelists`**: Standardized clinical code lists used for patient cohort identification and feature extraction from electronic health records
- **`shiny_app`**: R scripts for the interactive visualization tool that provides real-time risk alerts and visualization of predicted creatinine trajectories

## Data Source

The model was developed and validated using electronic health record data from the Clinical Practice Research Datalink (CPRD) Aurum. Due to data privacy regulations, the raw data cannot be shared, but the code for processing and analysis is available in this repository.

## Key Features

- **Advanced Prediction Capabilities**: Forecasting serum creatinine trajectories in heart failure patients
- **Cross-regional Validation**: Ensures model generalizability across diverse geographical regions
- **Interactive Visualization**: Shiny application for real-time risk alerts and trajectory visualization
- **Model Interpretability**: SHAP analysis for transparent insight into the model's decision-making process

## Getting Started

### Prerequisites
- R (version 4.0.0 or higher)
- Required R packages: shiny, shinydashboard, ggplot2, dplyr, etc.

#### Setting up the R environment
```R
# Install required packages
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "DT", "plotly"))
```
## Citation

If you use NephroTrend in your research, please cite our paper:
```
Citation information will be added upon publication
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact


## Acknowledgments

- Clinical Practice Research Datalink (CPRD) for providing the data
