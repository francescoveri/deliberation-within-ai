# Deliberation Within AI — Replication Materials

This repository contains the replication code and supporting materials for:

**Francesco Veri. _Deliberation Within AI: A Reproducible Pipeline for Multi-Agent Deliberation Simulation._**  
*Digital Government: Research and Practice* (2026).

The repository provides the code used to generate the structured and direct multi-agent deliberation simulations, as well as the robustness analyses reported in the paper.

## Repository contents

```text
.
├── README.md
├── app_structured_pipeline_batch.R
├── app_direct_deliberation_batch.R
├── robustness_test_LLM_ai_within.R
├── data_new_group.xlsx
├── dataPnew_individual.xlsx
└── outputs/
    ├── supp_table_inference.csv
    ├── robustness_full_results.csv
    └── descriptives_by_condition.csv
```

### Main scripts

- **`app_structured_pipeline_batch.R`**  
  Runs the stage-structured multi-agent deliberation pipeline. The batch mode executes the dataset-generating stages of the structured pipeline and exports cumulative individual-level and group-level data.

- **`app_direct_deliberation_batch.R`**  
  Runs the direct-deliberation comparison condition. The batch mode generates the corresponding individual-level and group-level datasets without the intermediate structured deliberative stages.

- **`robustness_test_LLM_ai_within.R`**  
  Reproduces the main robustness analyses used in the final manuscript, including:
  - baseline-adjusted DRI analyses;
  - checks for deliberation-length confounding;
  - small-sample and multiplicity-robust inference;
  - effect-size estimates;
  - empirical verification of the bounded opinion-drift rule.

## Data files required for the robustness analysis

The robustness script expects the following files in the repository working directory:

- `data_new_group.xlsx` — run-level data (20 runs);
- `dataPnew_individual.xlsx` — agent-/turn-level data.

These files should be included in the public repository if they can be shared. If they cannot be made public, this README should be updated to explain the access restrictions and how qualified researchers can obtain them.

## Requirements

### R packages for the simulation applications

The simulation scripts use the following R packages:

```r
install.packages(c(
  "shiny",
  "httr2",
  "jsonlite",
  "shinyjs",
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales",
  "gridExtra",
  "DT"
))
```

The scripts also make optional use of:

```r
install.packages(c("pdftools", "rmarkdown"))
```

`grid` and `tools` are distributed with R.

### R package for the robustness analysis

```r
install.packages("readxl")
```

The HC3 covariance estimator, permutation tests, bootstrap routines, and effect-size calculations used in `robustness_test_LLM_ai_within.R` are implemented directly in the script.

## OpenRouter API

The simulation applications make model calls through the OpenRouter API.

To run either application:

1. Create an OpenRouter account and obtain an API key.
2. Launch the relevant Shiny application.
3. Enter the API key in the password field in the application interface.
4. Select the model and temperature corresponding to the desired replication.
5. Enter the policy issue, evidence, participant configuration, and other simulation settings.
6. Run the simulation or use the **Batch runs / concrete replications** section.

**Do not commit API keys to GitHub.** The repository should contain code only; users should supply their own key at runtime.

OpenRouter usage may incur API costs depending on the model and number of runs.

## Running the structured-pipeline application

Open `app_structured_pipeline_batch.R` in RStudio and run the application.

The structured batch routine executes the dataset-generating stages of the pipeline and then computes the post-run metrics. It intentionally skips generation of the final narrative report and appendix during batch replication.

The batch interface allows repeated runs using the same configuration. It produces:

- `structured_pipeline_individual_level_batch.csv`
- `structured_pipeline_group_level_batch.csv`
- `structured_pipeline_failed_runs.csv`

Each successful replication is identified by `run_id`.

## Running the direct-deliberation application

Open `app_direct_deliberation_batch.R` in RStudio and run the application.

The direct condition uses a single shared deliberation rather than the staged pipeline. Its dataset-only batch routine generates the corresponding post-run metrics and exports:

- `direct_deliberation_individual_level_batch.csv`
- `direct_deliberation_group_level_batch.csv`
- `direct_deliberation_failed_runs.csv`

Again, `run_id` identifies each replication.

## Reproducing the robustness analyses

The robustness script currently contains:

```r
INSPECT_ONLY <- TRUE
```

The first run is therefore diagnostic: it prints the structure of the two input datasets and stops.

After confirming that the column mapping in the script matches the deposited data, change this to:

```r
INSPECT_ONLY <- FALSE
```

and run:

```r
source("robustness_test_LLM_ai_within.R")
```

The script writes three output files:

- `supp_table_inference.csv`
- `robustness_full_results.csv`
- `descriptives_by_condition.csv`

For the final archived replication release, it is preferable to deposit a version in which the mapping has already been checked and `INSPECT_ONLY <- FALSE`, so that the analysis runs without manual editing.

## Reproducibility note

The simulation applications record model choice, temperature, participant/group configuration, and run identifiers. However, exact textual reproduction of LLM outputs should not be expected. Model APIs are stochastic and provider-side model versions may change over time.

The replication objective is therefore to reproduce the **documented simulation configuration, analytical pipeline, and reported outcome calculations**, rather than byte-for-byte identical generated text.

For a publication archive, the exact model identifier, model provider, temperature, date of data generation, number of runs, and any other relevant inference settings used for the reported analyses should be recorded here or in a separate `REPLICATION_NOTES.md` file.


## Suggested citation

If you use this code or the associated simulation framework, please cite:

> Veri, Francesco. _Deliberation Within AI: A Reproducible Pipeline for Multi-Agent Deliberation Simulation._ Digital Government: Research and Practice, forthcoming. DOI: **[ADD DOI WHEN AVAILABLE]**

A permanent archived release of this repository should also be cited once a DOI is assigned through Zenodo.

## Archiving and versioning

For the version associated with the published article:

1. Create a GitHub release (for example, `v1.0-paper`).
2. Connect the GitHub repository to Zenodo.
3. Archive the release in Zenodo to obtain a permanent DOI.
4. Add the Zenodo DOI to this README and to the paper's Data and Code Availability statement.

This preserves the exact replication package even if the GitHub repository is later updated.

## License

Add an explicit license before publication. For code, a permissive license such as **MIT** is commonly used. Data may require a separate license depending on its provenance and sharing conditions.

## Contact

Francesco Veri  
University of Zurich  
[francesco.veri@zda.uzh.ch]

## funding
This pubblication sustained by RIA Horizon project AI4Deliberation
Grant agreement ID: 101178806
https://cordis.europa.eu/project/id/101178806
https://www.ai4dproject.eu/
