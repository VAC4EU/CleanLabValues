# CleanLabValuesDataset

This function ingests instructions to clean datasets containing results from laboratory analyses. Briefly, the instructions specify:

- which unit of measurement is desired for each laboratory analysis;
- what the conversion rules are;
- what to do if the unit of measurement is missing;
- which values should be considered absurd and discarded.

This program enacts the specifications developed by the VAC4EU Programming Task Force of the Working Group of Methods, Statistics and Programs.

## Input data

The input of the processing is a set of records that contain laboratory values. Mandatory fields are:

- `concept_id`: identifier of the type of laboratory value;
- `value`: value of the analysis;
- `unit`: unit of measurement.

Units of measurement may be more than one for the same laboratory value identifier, there may be missing units, and some units may contain typos.

Example of records to be cleaned:

| person_id | concept_id    | value | unit   |
| --------- | ------------- | ----: | ------ |
| P01       | LAB_BILIRUBIN |  71.0 | umol/L |
| P01       | LAB_BILIRUBIN |   990 | mg/dL  |
| P02       | WEIGHT        |  52.3 | kg     |
| P03       | HEIGHT        |   181 |        |
| P04       | WEIGHT        |  74.3 | kkg    |

## Arguments of the function

### Simple arguments

| Argument        | Description                                                                                                                                                                                          |
| --------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `dataset`       | Name of a `data.table` object in memory containing a dataset of laboratory-analysis results that needs cleaning.                                                                                     |
| `list_analyses` | Optional string vector containing the names of the laboratory analyses to be cleaned. If this argument is not specified, all laboratory analyses are cleaned.                                        |
| `datasource`    | Optional string containing the name of the datasource. This can be stored in `lab_unit_conversions` to produce a datasource-specific assumption on what to do if the unit of measurement is missing. |

### Arguments stored in CSV files

The following arguments contain paths to CSV files:

| Argument               | Description                                                                                                                                                    |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `lab_target_units`     | Path to a CSV file containing one record for each type of laboratory analysis in `list_analyses`, specifying the desired unit of measurement.                  |
| `lab_unit_conversions` | Path to a CSV file containing the specifications to convert values in the dataset to the target unit of measurement.                                           |
| `lab_thresholds`       | Path to a CSV file containing the specifications of which values should be considered absurd and discarded, possibly depending on other variables such as age. |

## Specification files

### 1. `lab_target_units`

For each `concept_id`, a target unit of measurement must be decided. The decision is stored in a CSV file. The data model of the file must be as follows:

- `concept_id`: the string stored in the input data;
- `target_unit`: a string representing the target unit. It may be one of the strings used in the input data, or not. If the measurement is an absolute number, this must be set to the string `NA`.

The primary unique key for this file is `concept_id`.

Example of input target unit:

| concept_id    | target_unit |
| ------------- | ----------- |
| LAB_BILIRUBIN | umol/L      |
| LAB_INR       | NA          |
| WEIGHT        | kg          |
| HEIGHT        | m           |

### 2. `lab_unit_conversions`

For each `concept_id`, a CSV file must store the conversion between the units of measurement found in the input data and the target unit of measurement. Two special cases must be considered:

- the case when the unit of measurement is missing in the data;
- the case when there are too many units of measurement in the data, consisting of variations of typos, uppercase, lowercase, etc., that are considered to be substantially equivalent to the same unit.

In both special cases, conversion rules may be conditional on rules assigned to the value.

The data model of this input must be as follows:

- `concept_id`: the string stored in the data;
- `target_unit`: the string assigned to `concept_id` in the target-unit input;
- `datasource`: optional identifier of a data source. The conversion may be conditional on this value, and the function accepts a corresponding string as an argument. If this field is missing, this conversion applies to all data sources;
- `origin_unit`: one of the units in the input data, or the keyword `MISSING`;
- `multiplication_factor_from_origin_to_target`: conversion factor from the value expressed in `origin_unit` to the value expressed in the target unit;
- `inverse_conversion`: the inverse of `multiplication_factor_from_origin_to_target`;
- `condition_on_value`: if `origin_unit` is the keyword `MISSING`, this field may contain a condition on the value, in R code;
- `next_attempt`: if the converted value is out of the threshold, as specified in the next section, the following actions are possible:
  - `0`: if the converted value does not meet the threshold, discard it;
  - `1`: if the converted value does not meet the threshold, try with the conversion of the same `concept_id` with the next rank; if this is the maximum, then discard;
  - `2`: if the converted value does not meet the threshold, try with the conversion of the same `concept_id` with the next rank; if this is the maximum, then discard;
  - `...`;
  - `99`: if the converted value does not meet the threshold, try again with `1`; if the converted value still does not meet the threshold, discard it.

The primary unique key for this file is the sequence:

```text
concept_id, datasource, origin_unit, condition_on_value
```

Example of a CSV file containing this specification:

| concept_id           | datasource | unit_origin | unit_target | multiplication_factor_from_origin_to_target | inverse_conversion | assumed_unit_if_missing_or_other | condition_on_values | next_attempt |
| -------------------- | ---------- | ----------- | ----------- | ------------------------------------------: | -----------------: | -------------------------------- | ------------------- | -----------: |
| LAB_BILIRUBIN        |            | mg/dL       | umol/L      |                                     17.0940 |             0.0585 |                                  |                     |           99 |
| LAB_URINE_CREATININE | DS_A       | MISSING     | g/dL        |                                      11.312 |                    | mmol/L                           | >= 1000             |           99 |
| LAB_URINE_CREATININE | DS_A       | MISSING     | g/dL        |                                 0.011309658 |                    | umol/L                           | < 1000              |           99 |
| WEIGHT               |            | cm          | m           |                                        0.01 |                100 |                                  |                     |           99 |
| HEIGHT               |            | kg          | kg          |                                           1 |                  1 |                                  |                     |           99 |
| WEIGHT               | DS_A       | MISSING     | kg          |                                           1 |                  1 | kg                               |                     |            1 |
| WEIGHT               | DS_A       | MISSING     | kg          |                                       0.001 |               1000 | kg                               |                     |            2 |

### 3. `lab_thresholds`

For each `concept_id`, threshold values may be stored. If the observed value, once converted using the previous file, is outside the threshold values, then it is considered an error and is discarded from the analysis.

The data model of this file is as follows:

- `concept_id`: the string stored in the input data;
- `target_unit`: the string assigned to `concept_id` in the file described above;
- `Min`: minimum acceptable value, once the value is converted to the target unit;
- `Max`: maximum acceptable value, once the value is converted to the target unit;
- `condition_on_variable`: sometimes the thresholds are conditional on the values of variable(s) in the input data. This field stores the condition as a string;
- `variable`: space-separated list of variable names that are included in the condition.

The primary unique key for this file is:

```text
concept_id, condition_on_variable, variable
```

Example:

| concept_id    |  Min |  Max | unit_target | condition_on_variable | variable |
| ------------- | ---: | ---: | ----------- | --------------------- | -------- |
| LAB_BILIRUBIN |  0.5 |  100 | umol/L      |                       |          |
| HEIGHT        |    1 |  2.4 | m           |                       |          |
| WEIGHT        |  0.5 |    8 | kg          | age < 2               | age      |
| WEIGHT        |    8 |  180 | kg          | age >= 2              | age      |

## Output

During cleaning, the program renames 'value' as 'value_origin' and 'unit' as 'unit_origin', then leaves all existing variables and adds the following variables:

 
- `included`: whether the value is considered valid after conversion:
  - `1`: included in the next steps;
  - `0`: discarded from the next steps.
- `value`: final value after conversion, missing if `included == 0`.
- `unit_target`: the target unit
- `conversion`: type of conversion from origin value to final value:
  - `0`: no conversion;
  - `1`: conversion from non-missing unit;
  - `2`: conversion from `OTHER` unit, where the unit is a non-empty string but is not listed among those with a conversion rule;
  - `3`: conversion from `MISSING` unit.
- `rule_applied`: evaluation of the conversion:
  - `0`: no conversion needed and result accepted;
  - `1`: conversion needed and result accepted;
  - `2`: more than one conversion needed before acceptance;
  - `90`: no conversion possible and result discarded;
  - `91`: one conversion attempted before discarding the result;
  - `92`: more than one conversion attempted before discarding the result;
  - `99`: discarded because value is non-numeric.

## Usage and validation

  The top-level function `CleanLabValuesDataset()` performs the required metadata checks (`check_dataset_model`, `check_lab_target_units`, `check_lab_unit_conversion`, `check_lab_thresholds`) before running the cleaning pipeline. You can call it directly; it will validate the `dataset` and the three CSV metadata files and then run the cleaning logic.

  For interactive use (to call lower-level functions or run tests), load the module files first:

  ```r
  # load modules once when developing or running interactively
  source("R/load_dependencies.R")
  load_cleanlab()
  ```

  Example: run cleaning with metadata files

  ```r
  cleaned <- CleanLabValuesDataset(
    dataset = dataset_lab_values,
    lab_target_units = "tests/data/Example 1/i_input/LAB_target_units.csv",
    lab_unit_conversion = "tests/data/Example 1/i_input/LAB_unit_conversion.csv",
    lab_thresholds = "tests/data/Example 1/i_input/LAB_threshold.csv"
  )
  ```

  The function returns a `data.table` containing all original input columns (renaming `value` -> `value_origin` and `unit` -> `unit_origin`), plus the cleaning result columns: `included`, `value` (cleaned), `unit_target`, `conversion`, `rule_applied`.

## Testing

A simple example harness is provided in `tests/test_clean_lab_main.R` which runs the pipeline on the example datasets under `tests/data/Example 1`, `Example 2` and `Example 3` and compares the output with the ground-truth CSVs.

From the project root you can run the harness directly with:

```bash
Rscript tests/test_clean_lab_main.R
```

For interactive debugging or development, load the modules and then source the test script from an R session:

```r
source("R/load_dependencies.R")
load_cleanlab()
source("tests/test_clean_lab_main.R")
```

Notes:
- Ensure your working directory is the project root when running these commands.
- The harness expects `data.table` to be installed.
