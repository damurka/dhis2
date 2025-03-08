
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{cd2030.extractor}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{cd2030.extractor}` like so:

``` r
devtools::install_github('aphrcwaro/dhis2')
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo aphrcwaro/dhis2@HEAD
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> rlang      (1.1.4  -> 1.1.5 ) [CRAN]
#> glue       (1.7.0  -> 1.8.0 ) [CRAN]
#> cli        (3.6.2  -> 3.6.4 ) [CRAN]
#> digest     (0.6.35 -> 0.6.37) [CRAN]
#> commonmark (1.9.1  -> 1.9.2 ) [CRAN]
#> jsonlite   (1.8.9  -> 1.9.1 ) [CRAN]
#> xfun       (0.50   -> 0.51  ) [CRAN]
#> purrr      (1.0.2  -> 1.0.4 ) [CRAN]
#> openssl    (2.3.1  -> 2.3.2 ) [CRAN]
#> curl       (6.2.0  -> 6.2.1 ) [CRAN]
#> httr2      (1.1.0  -> 1.1.1 ) [CRAN]
#> parallelly (1.41.0 -> 1.42.0) [CRAN]
#> Installing 12 packages: rlang, glue, cli, digest, commonmark, jsonlite, xfun, purrr, openssl, curl, httr2, parallelly
#> Installing packages into 'C:/Users/Murage/AppData/Local/Temp/RtmpkLcsL8/temp_libpath64086d8e4d89'
#> (as 'lib' is unspecified)
#> 
#>   There is a binary version available but the source version is later:
#>       binary source needs_compilation
#> httr2  1.1.0  1.1.1             FALSE
#> 
#> package 'rlang' successfully unpacked and MD5 sums checked
#> package 'glue' successfully unpacked and MD5 sums checked
#> package 'cli' successfully unpacked and MD5 sums checked
#> package 'digest' successfully unpacked and MD5 sums checked
#> package 'commonmark' successfully unpacked and MD5 sums checked
#> package 'jsonlite' successfully unpacked and MD5 sums checked
#> package 'xfun' successfully unpacked and MD5 sums checked
#> package 'purrr' successfully unpacked and MD5 sums checked
#> package 'openssl' successfully unpacked and MD5 sums checked
#> package 'curl' successfully unpacked and MD5 sums checked
#> package 'parallelly' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\Murage\AppData\Local\Temp\RtmpY7YyZ4\downloaded_packages
#> installing the source package 'httr2'
#> Adding 'cli_3.6.4.zip' to the cache
#> Adding 'commonmark_1.9.2.zip' to the cache
#> Adding 'curl_6.2.1.zip' to the cache
#> Adding 'digest_0.6.37.zip' to the cache
#> Adding 'glue_1.8.0.zip' to the cache
#> Adding 'httr2_1.1.1.tar.gz' to the cache
#> Adding 'jsonlite_1.9.1.zip' to the cache
#> Adding 'openssl_2.3.2.zip' to the cache
#> Adding 'parallelly_1.42.0.zip' to the cache
#> Adding 'purrr_1.0.4.zip' to the cache
#> Adding 'rlang_1.1.5.zip' to the cache
#> Adding 'xfun_0.51.zip' to the cache
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\Murage\AppData\Local\Temp\RtmpY7YyZ4\remotes4bf4177c7abb\aphrcwaro-dhis2-3e6c210/DESCRIPTION' ...  ✔  checking for file 'C:\Users\Murage\AppData\Local\Temp\RtmpY7YyZ4\remotes4bf4177c7abb\aphrcwaro-dhis2-3e6c210/DESCRIPTION' (379ms)
#>       ─  preparing 'cd2030.extractor': (403ms)
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>       ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>       ─  building 'cd2030.extractor_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/Murage/AppData/Local/Temp/RtmpkLcsL8/temp_libpath64086d8e4d89'
#> (as 'lib' is unspecified)
#> Adding 'cli_3.6.4.zip' to the cache
#> Adding 'commonmark_1.9.2.zip' to the cache
#> Adding 'curl_6.2.1.zip' to the cache
#> Adding 'digest_0.6.37.zip' to the cache
#> Adding 'glue_1.8.0.zip' to the cache
#> Adding 'httr2_1.1.1.tar.gz' to the cache
#> Adding 'jsonlite_1.9.1.zip' to the cache
#> Adding 'openssl_2.3.2.zip' to the cache
#> Adding 'parallelly_1.42.0.zip' to the cache
#> Adding 'purrr_1.0.4.zip' to the cache
#> Adding 'rlang_1.1.5.zip' to the cache
#> Adding 'xfun_0.51.zip' to the cache
```

## Run

You can launch the application by running:

``` r
cd2030.extractor::run_app()
```

## Code of Conduct

Please note that the khisr project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
