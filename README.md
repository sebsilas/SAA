# Singing Ability Assessment (SAA)

The SAA is a test of singing ability which can be launched in R/Shiny via the [`psychTestR`](https://github.com/pmcharrison/psychTestR) package.

# Author

Seb Silas, sebsilas@gmail.com

## Installation

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4. Install the `SAA`:

`devtools::install_github('sebsilas/SAA')`

5. Install *nodeJS* from here: https://nodejs.org/en/download/


## Usage

Once you have completed the above steps, you can run the `SAA` by doing the following:

1. Create a folder which will contain your app. The name of your folder will become the `app_name` argument when you run the `SAA` function below.

2. In the folder, create an *R* file called `app.R` and paste your code to run the `SAA` there, like below. Remember, the `app_name` argument should match the name of your folder.


``` r

# Load the SAA package

library(SAA)

# Run a short test with feedback:

SAA_standalone(app_name = "short_test",
               num_items = list(long_tones = 2L, arrhythmic = 2L, rhythmic = 2L),
               feedback = TRUE,
               SNR_test = FALSE,
               get_range = FALSE,
               musicassessr_aws = FALSE,
               examples = 0)

```

3. Before you run this script, make sure that the directory the script runs from is one above your app folder. If your `SAA` app is in */Users/musicassessr/my_project/my_SAA_app/* then make sure that the current directory is */Users/musicassessr/my_project/*. You can do this using an *.Rproj* file in */Users/musicassessr/my_project/* or placing `setwd('/Users/musicassessr/my_project/')` at the beginning of your app file.

NB. If you run the script from *RStudio*, for best results, run the script using the shortcut `Command + Shift + Enter` on Mac (`Ctrl + Shift + Enter` on Windows).

4. If the above steps were successful, the app should load in a browser and you should no errors and receive feedback after each trial. If not, go to our [Troubleshooting]() page or [raise an issue on Github](https://github.com/sebsilas/SAA/issues).

## Usage notes

- The test requires internet connectivity. 

- The *SAA* runs in your web browser. It is only recommended to run the test in [Google Chrome](https://www.google.com/chrome/) or [Opera](https://www.opera.com/download). 



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
 
 
Silas, S., Müllensiefen, D., & Kopiez, R. (2022). Singing Ability Assessment: Development and validation of an open-source testing environment for singing data. In prep.

