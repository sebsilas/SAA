# Singing Ability Assessment (SAA)

The SAA is a test of melody singing ability which can be launched in R/Shiny via the [`psychTestR`](https://github.com/pmcharrison/psychTestR) package.

# Author

Seb Silas, sebsilas@gmail.com

## Installation

## Local Testing

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

3. Open R.

4. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

5. Install the SAA:

`devtools::install_github('syntheso/SAA')`


## Usage

Once you have completed the above steps, you can run the SAA using the code below. Make sure to replace the credentials with those you yielded in Step 1.

``` r

# Load the SAA package

library(SAA)

# Run a short test:

# SAA_standalone(num_items = list(long_tones = 1L, arrhythmic = 2L, rhythmic = 2L),
                 SNR_test = FALSE, get_range = FALSE, examples = 0)


# Run the test with default length, but different item bank

# SAA_standalone(item_bank = itembankr::WJD)

```

## Usage notes

- The SAA runs in your web browser. It is only recommended to run the test in Google Chrome. 
- The test requires internet connectivity. 


## Online testing

To test online, you will need your own domain name. You can use our scripts which setup an Amazon server with everything required for you:

- Follow steps 1-3 [here](https://github.com/sebsilas/musicassessr_aws). Be sure to make a note of the credentials outputted in step 1.

Once your server is setup, to run the app, log into your server (via Command Prompt or Terminal) using your ssh command e.g.,

```
ssh  -i your_key.pem ubuntu@ec2-1-23-45-67.eu-central-1.compute.amazonaws.com

```

Navigate to the Shiny apps folder:

```
cd  /srv/shiny-server

```

Create a new folder for your app (NB. Avoid underscores in app names):

```
sudo mkdir my-app-name

```

Go into that folder and create a file for your app (NB. must be called *app.R*)

```
cd my-app-name
sudo nano app.R

```

Load R:

```
sudo R
```

Install the SAA:

```{r}



```






Please note, you don't have to use AWS to host your experiment. You could host the application on another server. To do that, you would need to install Shiny Server and NodeJS on your server. For the former, we recommend Anthony Chimiel's [guide](https://s3-eu-west-1.amazonaws.com/research.pmcharrison.com/psychTestR/psychTestR-server-docs-latest.pdf
).


## Citation

We advise mentioning the software versions you used,
in particular the versions of the `SAA`, `musicassessr`, `psychTestR`, and `psychTestRCAT` packages.
You can find these version numbers from R by running the following commands:

``` r
library(SAA)
library(psychTestR)
library(psychTestRCAT)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("SAA", "psychTestR", "psychTestRCAT"), ]
```

## References:
 
