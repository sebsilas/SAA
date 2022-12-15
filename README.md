# Singing Ability Assessment (SAA)

The `SAA` is a test of singing ability which can be launched in R/Shiny via the [`psychTestR`](https://github.com/pmcharrison/psychTestR) package.

# Author

Seb Silas, sebsilas@gmail.com

## Installation

1. Install *nodeJS*: https://nodejs.org/en/download/

2. Install *R*: https://cloud.r-project.org/

3. Install *RStudio*: https://posit.co/downloads/

4. Launch *RStudio*

5. Install the `devtools` *R* package by pasting the following command into the *RStudio* terminal:

`install.packages('devtools')`

6. Install the `SAA` package:

`devtools::install_github('sebsilas/SAA')`


## Usage

Once you have completed the above steps, you can run the `SAA` test by doing the following:

1. Create a folder which will contain your app. The name of this folder will become the `app_name` argument when you run the `SAA` or `SAA_standalone` functions later.

2. In the folder, create an *R* file called `app.R`. In this file, paste your code to run the `SAA` there, like below:


``` r

# Load the SAA package

library(SAA)

# Run a short test with feedback:

SAA_standalone(app_name = "my_SAA_app",
               num_items = list(long_tones = 2L, arrhythmic = 2L, rhythmic = 2L),
               feedback = TRUE,
               SNR_test = FALSE,
               get_range = FALSE,
               musicassessr_aws = FALSE,
               examples = 0)

```

Remember that the `app_name` argument should match the name of your folder.

3. Before you run this app script, make sure that the directory the script runs from is one above your app folder. So, if your `SAA` *app.R* file is contained in */Users/musicassessr/my_project/my_SAA_app/*, then make sure that the current directory of the app is */Users/musicassessr/my_project/*. 

You can do this by either a) using an *.Rproj* file in */Users/musicassessr/my_project/* or b) placing `setwd('/Users/musicassessr/my_project/')` at the beginning of your app file. Option a) is recommended. NB: You can check the current directory by using `getwd()`.

4. Launch the app from *RStudio*. To do this, it is recommended to use the shortcut `Command + Shift + Enter` on Mac (`Ctrl + Shift + Enter` on Windows).

4. If the above steps were successful, the `SAA` test should load in your web browser. As you progress through the test, you should receive feedback after each trial. If not, go to our [Troubleshooting]() page or [raise an issue on Github](https://github.com/sebsilas/SAA/issues).

## Usage notes

- The test requires internet connectivity. 

- The *SAA* runs in your web browser. It is only recommended to run the test in [Google Chrome](https://www.google.com/chrome/) or [Opera](https://www.opera.com/download). You will need to set one of these to be your default browser for *RStudio* to launch the test there (restart *RStudio* after doing this).



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

