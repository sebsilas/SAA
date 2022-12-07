# Singing Ability Assessment (SAA)

The SAA is a test of melody singing ability which can be launched in R/Shiny via the [`psychTestR`](https://github.com/pmcharrison/psychTestR) package.

# Author

Seb Silas, sebsilas@gmail.com

## Installation

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

3. Open R.

4. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

5. Install the SAA:

`devtools::install_github('sebsilas/SAA')`


## Usage

Once you have completed the above steps, you can run the SAA using the code below. Make sure to replace the credentials with those you yielded in Step 1.

``` r

# Load the SAA package

library(SAA)

# Run a short test:

SAA_standalone(num_items = list(long_tones = 1L, arrhythmic = 2L, rhythmic = 2L), SNR_test = FALSE, get_range = FALSE, examples = 0)


# Run the test with default length, but different item bank

# SAA_standalone(item_bank = itembankr::WJD)

```

## Usage notes

- The SAA runs in your web browser. It is only recommended to run the test in Google Chrome. 
- The test requires internet connectivity. 



## Citation

We advise mentioning the software versions you used,
in particular the versions of the `SAA`, `musicassessr`, `psychTestR`, and `psychTestRCAT` packages.
You can find these version numbers from R by running the following commands:

```r
library(SAA)
library(psychTestR)
library(psychTestRCAT)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("SAA", "psychTestR", "psychTestRCAT"), ]
```

## References
 
