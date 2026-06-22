# LMM_Who_Audit_Reviewers
Who Audits the Reviewers? A Multi-Model Consensus Framework for Characterizing Failures in LLM-Assisted Peer Review

Automated Multi-Judge Framework for Characterizing Failures in LLM-Assisted Peer ReviewThis repository contains the full experimental pipeline, analysis scripts, and anonymized datasets for the study titled: "Who Audits the Reviewers? 

A Multi-Model Consensus Framework for Characterizing Failures in LLM-Assisted Peer Review.

"1. Research Overview

The academic community faces an escalating challenge: Large Language Models (LLMs) are routinely used to assist with peer review, yet their outputs are prone to systemic, hard-to-detect errors. This research introduces a fully automated, triple-blind "LLM-as-a-Judge" consensus pipeline that leverages flagship generative models to audit review outputs, quantify error prevalence, and characterize structural failure modes.

2. Experimental Framework
Our pipeline executes a controlled generation matrix across 15 peer-reviewed papers (stratified across Computer Science, Medicine, and Social Sciences). It utilizes:Generation Models: GPT-4o, Claude 3.5 Sonnet, Gemini 1.5 Flash, Llama 3.1 70B, and Mistral Large.

Evaluation Panel: A tri-model consensus judge panel (GPT-4o, Claude 3.5 Sonnet, Gemini 1.5 Flash) utilizing Fleiss' Kappa for inter-rater reliability.

Methodology: A taxonomy of 11 specific failure modes (e.g., Hallucinated Citations, Unsupported Claims, Self-Correction Blind Spots).

4. Repository Structure/data: Anonymized experimental logs and judge-consensus results./scripts: Modular R scripts for ingestion, generation, evaluation, and statistical analysis./manuscript: LaTeX source files and bibliographic data.

5. Reproducibility

The pipeline is designed for full reproducibility. 

To run the analysis:

- Environment: Ensure R (v4.x) is installed.Dependencies: 

- Install required packages:install.packages(c("httr2", "jsonlite", "tidyverse", "irr"))  

**API Integration:** This pipeline uses the OpenRouter API. Set your API key as an environment variable in a local `.env` file:

OPENROUTER_API_KEY='your-api-key-here'

Execution: Run the scripts in order: 01_retrieval.R $\rightarrow$ 02_generate.R $\rightarrow$ 03_evaluation.R $\rightarrow$ 04_analysis.R.5. Ethics and TransparencyThis research does not involve human subjects or proprietary data. All generated outputs were created in a controlled academic environment, and results are presented as aggregated, anonymized metrics.6. CitationIf you use this framework or dataset in your research, please cite our manuscript. 

License

This project is licensed under the MIT License.
