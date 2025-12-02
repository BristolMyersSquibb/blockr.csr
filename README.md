# blockr.csr

Clinical Study Report blocks for blockr. Provides blocks for generating random CDISC ADaM datasets and example workflows for MMRM analyses.

## Installation

```r
# Install from GitHub
pak::pak("BristolMyersSquibb/blockr.csr")
```

## Getting Started

Load the package and start an empty workflow:

```r
library(blockr)
library(blockr.csr)

run_app()
```

This will open an empty blockr workspace. You can add blocks from the block menu, including:

- **Random ADSL block** - Generate random subject-level data
- **Random ADaM block** - Generate random analysis datasets (ADQS, ADAE, ADVS, etc.)

## Example Workflows

The package includes several example workflows in `inst/workflows/`:

- `mmrm_analysis.R` - Basic MMRM analysis
- `mmrm_subgroup.R` - Subgroup analysis
- `mmrm_biomarker.R` - Biomarker interaction analysis
- `mmrm_propensity.R` - Propensity-weighted MMRM
- `mmrm_biomarker_propensity.R` - Biomarker analysis with propensity weights
- `mmrm_demographics.R` - Demographics table
- `mmrm_descriptive.R` - Descriptive statistics
- `mmrm_vital_signs.R` - Vital signs summary
- `mmrm_combined.R` - All analyses combined in one workflow

To run an example workflow:
```r
source(system.file("workflows", "mmrm_analysis.R", package = "blockr.csr"))
```

## Dependencies

This package depends on `blockr`, which provides the core functionality along with dplyr, ggplot, and IO blocks. The example workflows also use `blockr.ai` for LLM-powered table generation.
