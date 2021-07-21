# Melody Singing Task (MST)

The MST is a test of melody singing ability.

 
## Installation

The Melody Singing Task (MST) uses the [`musicassessr`](https://github.com/syntheso/musicassessr) R package, which in turn relies on an Amazon Web Services (AWS) architecture, to record and process audio, which cannot be directly setup within R. In step 1, you run some scripts to setup this architecture for you. You will receive some credentials which you input into the MST R function (or other relevant [`musicassessr`](https://github.com/syntheso/musicassessr) functionality you would like to use). You will only need to do this once.

### Instructions

1. Setup your AWS architecture: https://github.com/mcetn/shiny-app-aws

2. If you don't have R installed, install it from here: https://cloud.r-project.org/

3. Open R.

4. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

5. Install the MST:

`devtools::install_github('syntheso/MST')`


## Usage

Once you have completed the above steps, you can run the MST using the code below. Make sure to replace the credentials with those you yielded in Step 1.

``` r
# Load the MST package
library(MST)

# Inputting your AWS parameters generated above, run a test with default parameters (6 long note trials, 10 arrhythmic melody trials, 10 rhythmic melody trials and the Berkowitz item bank).
MST(aws_credentials = list("api_url" = "your_url",
                           "bucket_name" = "your_bucket_name",
                           "bucket_region" = "your_bucket_region",
                           "identity_pool_id" = "your_identity_pool_id",
                           "destination_bucket" = "your_destination_bucket"))

# Change the default parameters e.g by using fewer trials and a different item bank
MST(aws_credentials = list("api_url" = "your_url",
                           "bucket_name" = "your_bucket_name",
                           "bucket_region" = "your_bucket_region",
                           "identity_pool_id" = "your_identity_pool_id",
                           "destination_bucket" = "your_destination_bucket"),
    num_items = list("long_tones" = 4L,
                     "arrhythmic" = 5L,
                      "rhythmic" = 5L), 
    item_bank = itembankr::WJD)
```

## Usage notes

- The MST runs in your web browser.
- By default, some image files are hosted online on our servers.
The test therefore requires internet connectivity.

## Citation

We advise mentioning the software versions you used,
in particular the versions of the `MST`, `psychTestR`, and `psychTestRCAT` packages.
You can find these version numbers from R by running the following commands:

``` r
library(MST)
library(psychTestR)
library(psychTestRCAT)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("MST", "psychTestR", "psychTestRCAT"), ]
```

## References:

D. Müllensiefen, K. Frieler, S. Silas, E.  Tsigeman, M.  Likhanov, Y. Kovas (submitted). *Jack and Jill: construction, calibration and validation of an adaptive visuospatial working memory test*
 
