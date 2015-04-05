<!-- README.md is generated from README.Rmd. Please edit that file -->
Parse API Client for R
======================

`rparse` provides API functions for the cloud backend service [parse.com](https://parse.com/), including:

-   creating and updating objects (`parse_save`)
-   deleting objects (`parse_delete`)
-   signing up (`parse_signup`) and logging in (`parse_login`) a user

### Installation and Setup

Install using [devtools](https://github.com/hadley/devtools):

``` r
devtools::install_github("dgrtwo/rparse")
```

Before use, add lines to your R profile setting up your Parse API key:

``` r
Sys.setenv(PARSE_APPLICATION_ID = "YOUR_APPLICATION_ID")
Sys.setenv(PARSE_API_KEY = "YOUR_REST_API_KEY")
```

These can be found in your [Account/App keys](https://parse.com/account/keys) page on Parse.
