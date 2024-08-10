# Mental Disorder Classification using Neural Networks

This repository contains a project aimed at exploring the potential of using a neural network to reverse the traditional diagnostic process for mental health disorders;specifically focusing on Mania Bipolar Disorder, Depressive Bipolar Disorder, Major Depressive Disorder, and individuals who have not been diagnosed. The project leverages a dataset from Kaggle that includes various mental disorder classifications based on participant symptoms.

## Key Features
- **GitHub Repository**: Organized project files with a clear structure and comprehensive README.
- **R Quarto**: Used for exploratory data analysis (EDA).
- **Jupyter Notebooks**: Documentation of data preprocessing, model training, and evaluation.
- **Overleaf Report**: A detailed report on the methodology, results, and findings.

## Data Overview

### Binary Variables (Yes or No)

- **mood_swing**: Does the patient have mood swings.
- **suicidal_thoughts**: Does the patient have suicidal thoughts.
- **anorexia**: Does the patient have anorexia.
- **authority_respect**: Does the patient have respect for authority.
- **try_explain**: Does the patient try and explain issues or symptoms.
- **aggressive_response**: Does the patient have an aggressive responses.
- **ignore_move_on**: Does the patient have try to ignore problems and move on.
- **nervous_break_down**: Does the patient have nervous breakdowns.
- **admit_mistakes**: Is the patient willing to admit their mistakes.
- **overthinking**: Does the patient tend to overthink.

### Multi-class Variables (Ranked: Most often, Usually, Sometimes, Seldom)

- **sadness**: Measure of the patient's level of sadness.
- **euphoric**: Measure of the patient's level of euphoria.
- **exhausted**: Measure of the patient's level of exhaustion.
- **sleep_disorder**: Measure of the patient's sleep disorder.

### Ordinal Categorical Variables (Ranked: 1-10, 1 is low 10 is high)

- **sexual_activity**: Level of the patient's sexual activity.
- **concentration**: Level of the patien't ability to concentrate.
- **optimism**: Level of the patient't optimism.

### Other Variables
- **patient_number**: Identifier for the patient.
- **expert_diagnose**: Expert diagnosis (or non-diagnosis) of the patient.

## Project Overview
The goal of this project is to develop, train, and evaluate a neural network model to classify depressive mental disorders: Mania Bipolar Disorder, Depressive Bipolar Disorder, Major Depressive Disorder, and 'Normal' Individuals. The project demonstrates the integrated use of multiple tools and techniques, including data preprocessing, neural network design, model training, documentation in R Quarto and a Jupyter notebook, as well as comprehensive reporting using Overleaf.