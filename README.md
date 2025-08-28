# 🌍 Spatial Modelling & Clustering of Diabetes Prevalence in Kenya  

## 📌 Project Overview  
This repository presents an analysis of **diabetes prevalence in Kenya** using advanced spatial modelling and clustering techniques. Diabetes is a growing public health challenge in Kenya, projected to rise from **3.3% prevalence to 4.5% by 2025** without effective interventions.  

The project applies **Bayesian Hierarchical Models**, **Geographically Weighted Regression (GWR)**, and **Spatial Scan Statistics** to identify **diabetes hotspots**, assess determinants, and explore spatial heterogeneity across Kenyan counties.  

---

## 🎯 Objectives  

### General Objective  
- To identify distinct clusters, patterns, and determinants of diabetes prevalence in Kenya.  

### Specific Objectives  
- 🔍 Identify **diabetes hotspot counties** in Kenya using spatial scan statistics and mapping.  
- 📊 Link the occurrence of diabetes to its determinants using **Hierarchical Spatial Models**.  
- 🧮 Select appropriate **spatial smoothing techniques** (based on Deviance Information Criterion, DIC).  
- 🗺️ Apply **Geographically Weighted Regression (GWR)** to analyze varying relationships between diabetes and risk factors across counties.  

---

## 🏥 Background & Problem Statement  
- Globally, **422 million adults** live with diabetes; **14.2 million** in Africa (aged 20–79).  
- In Kenya, **prevalence is ~3.3%**, expected to rise to **4.5% by 2025**.  
- Diabetes poses economic and social burdens due to high **mortality, management costs, and lost productivity**.  
- County-level management is underfunded, understaffed, and under-resourced.  

This study provides critical data to guide **health policy and interventions** targeting non-communicable diseases in Kenya.  

---

## 📚 Literature Review (Highlights)  
- Spatial smoothing techniques help reveal distinct disease prevalence patterns (Ogunsakin & Ginindza, 2022; Fang et al., 2006).  
- Bayesian mapping applied in malaria (Kazembe, 2007) and cancer (Rosenger et al., 2002).  
- Spatial clustering of diabetes studied in the US (Shrestha et al., 2013; Hipp & Chalise, 2016).  

---

## ⚙️ Methodology  

- **Bayesian Hierarchical Models**  
  - Account for uncertainty and spatial correlation.  
  - Implemented via **INLA (Integrated Nested Laplace Approximation)**.  
  - Three models: structured, unstructured, and combined effects.  

- **Geographically Weighted Regression (GWR)**  
  - Captures spatially varying relationships between diabetes (dependent variable) and risk factors:  
    - Age  
    - Gender  
    - Obesity & Physical inactivity  
    - Cholesterol  
    - Alcohol consumption  

- **Multilevel Logistic Regression**  
  - Models binary diabetes outcome with individual- and cluster-level effects.  

- **Model Selection**  
  - **Deviance Information Criterion (DIC)** for comparing model fit and complexity.  

- **Spatial Clustering**  
  - Applied **Kulldorff’s spatial scan statistic** to detect hotspots of high diabetes prevalence.  
  - Significance tested using **Monte Carlo simulations**.  

---
