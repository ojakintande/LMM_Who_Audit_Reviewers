# LLM_Who_Audit_Reviewers
## Who Audits the Reviewers? A Multi-Model Consensus Framework for Characterizing Failures in LLM-Assisted Peer Review

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status: Research Prototype](https://img.shields.io/badge/Status-Active%20Research-blue)](https://github.com/ojakintande/LLM_Who_Audit_Reviewers)

This repository contains the full experimental pipeline, analysis scripts, and anonymized datasets for the study: *"Who Audits the Reviewers? A Multi-Model Consensus Framework for Characterizing Failures in LLM-Assisted Peer Review."*[cite: 1]

---

## 1. Research Overview
The academic community faces an escalating challenge: Large Language Models (LLMs) are routinely used to assist with peer review, yet their outputs are prone to systemic, hard-to-detect errors[cite: 1]. This research introduces a fully automated, triple-blind "LLM-as-a-Judge" consensus pipeline that leverages flagship generative models to audit review outputs, quantify error prevalence, and characterize structural failure modes[cite: 1].

## 2. Experimental Framework
Our pipeline executes a controlled generation matrix across 15 peer-reviewed papers (stratified across Computer Science, Medicine, and Social Sciences)[cite: 1].

*   **Generation Models:** GPT-4o, Claude 3.5 Sonnet, Gemini 1.5 Flash, Llama 3.1 70B, and Mistral Large[cite: 1].
*   **Evaluation Panel:** A tri-model consensus judge panel (GPT-4o, Claude 3.5 Sonnet, Gemini 1.5 Flash) utilizing Fleiss' Kappa for inter-rater reliability[cite: 1].
*   **Methodology:** A taxonomy of 11 specific failure modes (e.g., Hallucinated Citations, Unsupported Claims, Self-Correction Blind Spots)[cite: 1].

## 3. Repository Structure
*   `/data`: Anonymized experimental logs and judge-consensus results[cite: 1].
*   `/scripts`: Modular R scripts for ingestion, generation, evaluation, and statistical analysis[cite: 1].
*   `/manuscript`: LaTeX source files and bibliographic data[cite: 1].

## 4. Reproducibility & Quick Start
The pipeline is designed for full reproducibility[cite: 1].

1.  **Environment:** Ensure R (v4.x) is installed[cite: 1].
2.  **Dependencies:** Install the required R packages[cite: 1]:
```r
    install.packages(c("httr2", "jsonlite", "tidyverse", "irr"))
    ```
3.  **API Integration:** This pipeline uses the OpenRouter API. Set your API key as an environment variable in a local `.env` file[cite: 1]:
```text
    OPENROUTER_API_KEY='your-api-key-here'
    ```
4.  **Execution:** Run the scripts in order: `01_retrieval.R` $\rightarrow$ `02_generate.R` $\rightarrow$ `03_evaluation.R` $\rightarrow$ `04_analysis.R`[cite: 1].

## 5. Ethics and Transparency
This research does not involve human subjects or proprietary data[cite: 1]. All generated outputs were created in a controlled academic environment, and results are presented as aggregated, anonymized metrics[cite: 1].

## 6. Citation
If you use this framework or dataset in your research, please cite our manuscript:
> [Awaiting DOI/Publication info — Please update once available]

## 7. License
This project is licensed under the **MIT License**[cite: 1].
