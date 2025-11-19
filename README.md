---
title: "Code and Data for: Reproducibility and Open Science in Economics"
author:
  - Lars Vilhuber
date: 2025-06-19
pubdate: "2025"
pubdoi: "10.1234/something"
publoc: "Zenodo"
pubversion: "V1"
output:
  html_document: 
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_depth: 1
  word_document: default
  pdf_document: 
    toc: true
    number_sections: true
    toc_depth: 1
editor_options: 
  chunk_output_type: console
bibliography: 
  - combined-references.bib
  - grateful-refs.bib
csl: _readme/chicago-author-date.csl
tutorial: false
github: https://github.com/social-science-data-editors/readme-rmarkdown
---









> File prepared for **Revue Economique**.


# Cite as

> Lars Vilhuber, 2025, "Code and Data for: Reproducibility and Open Science in Economics", Zenodo, V1, https://doi.org/10.1234/something


[![DOI:10.1234/something](https://zenodo.org/badge/DOI/10.1234/something.svg)](https://doi.org/10.1234/something)


# Overview





The code in this replication package constructs most tables and figures from outside data sources.  The code is in R. The replication package also includes PNG and PDF copies of screenshots and external figures, as noted below.

# Data Availability and Provenance Statements





All data are public, and can be redistributed. 

## Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 
- [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permission are documented in the [LICENSE.txt](LICENSE.txt) file.


## License for Data





- Data from @openalex-data are obtained under a [CC0](https://creativecommons.org/publicdomain/zero/1.0/) Public Domain attribution.
- Data fom @crossref-data is "is open and available for reuse without restriction" ([https://www.crossref.org/documentation/retrieve-metadata/](https://www.crossref.org/documentation/retrieve-metadata/))

All derivative data contained herein, if not otherwise encumbered, is available under a [CC-BY-NC-4.0](https://creativecommons.org/licenses/by-nc/4.0/legalcode) license. Usage by commercial entities is permitted, reselling the data is not.


## Summary of Availability

- [x] All data **are** publicly available.
- [ ] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.







```
## mutate: new variable 'Data Source' (logical) with one unique value and 100% NA
##         new variable 'Provided' (character) with one unique value and 0% NA
##         new variable 'Order' (logical) with one unique value and 100% NA
## mutate: changed 82 values (100%) of 'Filename' (0 new NAs)
## mutate: new variable 'Provided.real' (logical) with one unique value and 0% NA
##         new variable 'Provided.norm' (logical) with 2 unique values and 0% NA
##         new variable 'congruence' (logical) with 2 unique values and 0% NA
## filter: removed one row (50%), one row remaining
```

```
## Warning: 19 files fail check - are present but should be absent
```

```
## distinct: no rows removed
```
### Files failing check
 
> This list is only printed out when the normative provision of the files (i.e., whether the file should be present in the public replication package) does not match the actual presence of the file.
> 
> This entire section disappears when everything is in order!
> 
> The normative list is maintainaed MANUALLY in /home/rstudio/economics-open-science/_readme/datafiles.xlsx, and should be updated whenever the normative provision of files changes.

```
## filter: removed 3 rows (14%), 19 rows remaining
## select: dropped 3 variables (Order, Provided, congruence)
```



|Data Source                                                       |Filename                                          |Provided.real |Provided.norm |
|:-----------------------------------------------------------------|:-------------------------------------------------|:-------------|:-------------|
|Crossref (2023)                                                   |./data/crossref/crossref_aejdois.Rds              |FALSE         |TRUE          |
|Crossref (2023)                                                   |./data/crossref/crossref_info.csv                 |FALSE         |TRUE          |
|Crossref (2023)                                                   |./data/crossref/crossref_info.Rds                 |FALSE         |TRUE          |
|OurResearch (2023)                                                |./data/openalex/citations-per-paper.Rds           |FALSE         |TRUE          |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae-authors.Rds        |FALSE         |TRUE          |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae.Rds                |FALSE         |TRUE          |
|OurResearch (2023)                                                |./data/openalex/openalex-hindex.Rds               |FALSE         |TRUE          |
|OurResearch (2023)                                                |./data/openalex/openalex-institutions-aejae.Rds   |FALSE         |TRUE          |
|Reuters (2016), Clarivate (2018) and manual edits                 |./data/h_index_data/h-index-assignment1.2019.csv  |FALSE         |TRUE          |
|Reuters (2016), Clarivate (2018) and manual edits                 |./data/h_index_data/h-index-assignment1.2019.xlsx |FALSE         |TRUE          |
|Generated during acquisition of Crossref data                     |./data/crossref/crossref_timing.Rds               |FALSE         |TRUE          |
|Generated for data quality audit purposes                         |./data/crossref/audit-exp.xlsx                    |FALSE         |TRUE          |
|Hand-generated by authors based on AEA websites                   |./data/crossref/issns.Rds                         |FALSE         |TRUE          |
|Generated from openAlex data                                      |./data/openalex/affiliations.csv                  |FALSE         |TRUE          |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.csv            |FALSE         |TRUE          |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.xlsx           |FALSE         |TRUE          |
|Machine-imputed institutions using auxiliary openAlex information |./data/openalex/affiliations-imputed.Rds          |FALSE         |TRUE          |
|Records to remove from extracted openAlex data                    |./data/openalex/blacklist.xlsx                    |FALSE         |TRUE          |
|Procedural file to map short names to long variable names         |./data/auxiliary/mainOA-mapping.xlsx              |FALSE         |TRUE          |

### Files present
 
> This list is only printed out when the normative provision of the files (i.e., whether the file should be present in the public replication package) does not match the actual presence of the file.
> 
> This entire section disappears when everything is in order!
> 
> The normative list is maintainaed MANUALLY in /home/rstudio/economics-open-science/_readme/datafiles.xlsx, and should be updated from the draft file.

```
## select: columns reordered (Data Source, Filename, Provided, Order)
```



|Data Source |Filename                                                                  |Provided |Order |
|:-----------|:-------------------------------------------------------------------------|:--------|:-----|
|NA          |./data/christensen-2019-fig1-legend.png                                   |Yes      |NA    |
|NA          |./data/christensen-2019-fig1b.png                                         |Yes      |NA    |
|NA          |./data/christensen-2019-fig7-legend.png                                   |Yes      |NA    |
|NA          |./data/christensen-2019-fig7.png                                          |Yes      |NA    |
|NA          |./data/confidential/Jira Export CSV (my defaults) 20250218210036.csv      |Yes      |NA    |
|NA          |./data/confidential/jira-search-84765891-47e0-43ec-874d-365f527ef997.xlsx |Yes      |NA    |
|NA          |./data/confidential/readme.txt                                            |Yes      |NA    |
|NA          |./data/crdcn-Publications-Export-2025-February-11-0901.csv                |Yes      |NA    |
|NA          |./data/crossref_dois_enhanced.Rds                                         |Yes      |NA    |
|NA          |./data/crossref_dois.Rds                                                  |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-01-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-02-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-03-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-04-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-05-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-06-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-07-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-08-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-09-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-10-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-11-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-12-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-13-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-14-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-15-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-16-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-17-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-18-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-19-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-20-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-21-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-22-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-23-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-24-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-25-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-26-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-27-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-02-28-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/2025-03-01-r.csv.gz                             |Yes      |NA    |
|NA          |./data/interwrk/cran_logs/downloaded_files_select.rds                     |Yes      |NA    |
|NA          |./data/interwrk/geolocations.rds                                          |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_country_select_nochina.csv                 |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_country_select_nochina.rds                 |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_country_select.csv                         |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_country_select.rds                         |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_global_select_nochina.csv                  |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_global_select_nochina.rds                  |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_global_select.csv                          |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_global_select.rds                          |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_region_select_nochina.csv                  |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_region_select_nochina.rds                  |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_region_select.csv                          |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_by_region_select.rds                          |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_select_combined.csv                           |Yes      |NA    |
|NA          |./data/interwrk/r_downloads_select_combined.rds                           |Yes      |NA    |
|NA          |./data/interwrk/ssclogs.parquet                                           |Yes      |NA    |
|NA          |./data/interwrk/ssclogs.rds                                               |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_country_select_nochina.csv             |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_country_select_nochina.rds             |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_country_select.csv                     |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_country_select.rds                     |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_global_select_nochina.csv              |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_global_select_nochina.rds              |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_global_select.csv                      |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_global_select.rds                      |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_region_select_nochina.csv              |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_region_select_nochina.rds              |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_region_select.csv                      |Yes      |NA    |
|NA          |./data/interwrk/stata_downloads_by_region_select.rds                      |Yes      |NA    |
|NA          |./data/issns.Rds                                                          |Yes      |NA    |
|NA          |./data/jira_access_cleaned.csv                                            |Yes      |NA    |
|NA          |./data/jira_access_cleaned.Rds                                            |Yes      |NA    |
|NA          |./data/nsf24336-tab001-005.xlsx                                           |Yes      |NA    |
|NA          |./data/ProjectsAllMetadata_25JUN2024.xlsx                                 |Yes      |NA    |
|NA          |./data/PSID Bibliography Search.pdf                                       |Yes      |NA    |
|NA          |./data/raw/WebSTAR.log.gz                                                 |Yes      |NA    |
|NA          |./data/restrictions.xlsx                                                  |Yes      |NA    |
|NA          |./data/Screenshot 2025-02-08 at 19-49-29 PSID Bibliography Search.png     |Yes      |NA    |
|NA          |./data/Screenshot 2025-05-16 at 14-49-46 CASD - Centre d'accès[...].png   |Yes      |NA    |
|NA          |./data/signal-2025-06-27-10-20-09-831.jpg                                 |Yes      |NA    |
|NA          |./data/sources.txt                                                        |Yes      |NA    |
|NA          |./data/stata-licenses-by-country.xlsx                                     |Yes      |NA    |



## Complete List of Data Files by Data Source






The following table lists the complete set of data files **used** by the code, and whether they are provided as part of the replication package.



```
## select: dropped 4 variables (Order, Provided.real, Provided.norm, congruence)
```



|Data Source                                                       |Filename                                          |Provided |
|:-----------------------------------------------------------------|:-------------------------------------------------|:--------|
|Kingi et al (2019)                                                |./data/replication_data/entryQ_pub.Rds            |FALSE    |
|Kingi et al (2019)                                                |./data/replication_data/exitQ_pub.Rds             |FALSE    |
|Kingi et al (2019)                                                |./data/replication_data/replication_list_pub.Rds  |FALSE    |
|Crossref (2023)                                                   |./data/crossref/crossref_aejdois.Rds              |FALSE    |
|Crossref (2023)                                                   |./data/crossref/crossref_info.csv                 |FALSE    |
|Crossref (2023)                                                   |./data/crossref/crossref_info.Rds                 |FALSE    |
|OurResearch (2023)                                                |./data/openalex/citations-per-paper.Rds           |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae-authors.Rds        |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae.Rds                |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-hindex.Rds               |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-institutions-aejae.Rds   |FALSE    |
|Reuters (2016), Clarivate (2018) and manual edits                 |./data/h_index_data/h-index-assignment1.2019.csv  |FALSE    |
|Reuters (2016), Clarivate (2018) and manual edits                 |./data/h_index_data/h-index-assignment1.2019.xlsx |FALSE    |
|Generated during acquisition of Crossref data                     |./data/crossref/crossref_timing.Rds               |FALSE    |
|Generated for data quality audit purposes                         |./data/crossref/audit-exp.xlsx                    |FALSE    |
|Hand-generated by authors based on AEA websites                   |./data/crossref/issns.Rds                         |FALSE    |
|Generated from openAlex data                                      |./data/openalex/affiliations.csv                  |FALSE    |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.csv            |FALSE    |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.xlsx           |FALSE    |
|Machine-imputed institutions using auxiliary openAlex information |./data/openalex/affiliations-imputed.Rds          |FALSE    |
|Records to remove from extracted openAlex data                    |./data/openalex/blacklist.xlsx                    |FALSE    |
|Procedural file to map short names to long variable names         |./data/auxiliary/mainOA-mapping.xlsx              |FALSE    |



## Details on each Data Source






### @raw-data-2019 

The data were collected through the methods described in the paper. Data were collected over several years, and deposited at @raw-data-2019 in 2019, with replicator names replaced by random identifiers. 


```
## filter: removed 19 rows (86%), 3 rows remaining
## select: dropped 4 variables (Order, Provided.real, Provided.norm, congruence)
```



|Data Source        |Filename                                         |Provided |
|:------------------|:------------------------------------------------|:--------|
|Kingi et al (2019) |./data/replication_data/entryQ_pub.Rds           |FALSE    |
|Kingi et al (2019) |./data/replication_data/exitQ_pub.Rds            |FALSE    |
|Kingi et al (2019) |./data/replication_data/replication_list_pub.Rds |FALSE    |


### Crossref data

Crossref were extracted as needed to obtain bibliographic information (author names, article titles, publication dates) [@crossref-data], using the `rcrossref` package to query the API `. The database itself is free to access. More information about it can be read in @crossref-paper . Note that data can and is updated, so running the query again 


```
## filter: removed 16 rows (73%), 6 rows remaining
## select: dropped 4 variables (Order, Provided.real, Provided.norm, congruence)
```



|Data Source                                     |Filename                             |Provided |
|:-----------------------------------------------|:------------------------------------|:--------|
|Crossref (2023)                                 |./data/crossref/crossref_aejdois.Rds |FALSE    |
|Crossref (2023)                                 |./data/crossref/crossref_info.csv    |FALSE    |
|Crossref (2023)                                 |./data/crossref/crossref_info.Rds    |FALSE    |
|Generated during acquisition of Crossref data   |./data/crossref/crossref_timing.Rds  |FALSE    |
|Generated for data quality audit purposes       |./data/crossref/audit-exp.xlsx       |FALSE    |
|Hand-generated by authors based on AEA websites |./data/crossref/issns.Rds            |FALSE    |


### openAlex data

Data were accessed in 2023 to increase the time series covered by the bibliometric analysis [@openalex-data]. The openAlex database is described in @openalex2022. OpenAlex data are accessed via an API, and computed statistics, such as the h-index, are limited to the past 10 years. We therefore had to recompute some numbers. We also saved our extract, as future extracts may have different numbers, due to improvements in entity disambiguation (author names) and other factors outside of our control.

Not all data elements are complete in the openAlex data. We output data for various quality checks, and did manual research to "impute" attributes such as affiliations. All "overrides" are captured in a separate file. 

Thus, the openAlex data has (a) the raw data as downloaded; (b) the problematic data, as output for manual review (c) the edited/imputed data as used to complement the downloaded data.


```
## filter: removed 12 rows (55%), 10 rows remaining
## select: dropped 4 variables (Order, Provided.real, Provided.norm, congruence)
```



|Data Source                                                       |Filename                                        |Provided |
|:-----------------------------------------------------------------|:-----------------------------------------------|:--------|
|OurResearch (2023)                                                |./data/openalex/citations-per-paper.Rds         |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae-authors.Rds      |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae.Rds              |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-hindex.Rds             |FALSE    |
|OurResearch (2023)                                                |./data/openalex/openalex-institutions-aejae.Rds |FALSE    |
|Generated from openAlex data                                      |./data/openalex/affiliations.csv                |FALSE    |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.csv          |FALSE    |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.xlsx         |FALSE    |
|Machine-imputed institutions using auxiliary openAlex information |./data/openalex/affiliations-imputed.Rds        |FALSE    |
|Records to remove from extracted openAlex data                    |./data/openalex/blacklist.xlsx                  |FALSE    |


# Computational requirements





## Software Requirements



```
## Warning in readLines(file.path(".myconfig.sh")): incomplete final line found on
## '.myconfig.sh'
```

```
## filter: removed 12 rows (92%), one row remaining
## filter: removed 12 rows (92%), one row remaining
## filter: removed 12 rows (92%), one row remaining
```

- [x] The replication package contains one or more programs to install all dependencies and set up the necessary directory structure. 

- R version 4.5.1 (2025-06-13) on x86_64, linux-gnu
  - Docker image is used (see appendix), with system libraries defined by the relevant image (rocker/geospatial:4.5.1) (optional, but recommended)
  - RSPM (now [Posit Package Manager, PPM](https://packagemanager.posit.co/client/)) is used, set to the time-stamped date defined by the container image (rocker/geospatial:4.5.1)
  - Libraries are managed through `renv`.






```
## filter: removed 17 rows (1%), 1,141 rows remaining
## rename: renamed one variable (Libraries are defined in)
```



|Libraries are defined in                                                                                                     |
|:----------------------------------------------------------------------------------------------------------------------------|
|_readme/libraries.R                                                                                                          |
|code/libraries.R                                                                                                             |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/examples/apps/news.R                                   |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/examples/apps/outdated.R                               |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/examples/apps/search.R                                 |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/examples/apps/up.R                                     |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/exec/news.R                                            |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/exec/outdated.R                                        |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/exec/search.R                                          |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/exec/up.R                                              |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/shiny/along/app.R                                      |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/shiny/format/app.R                                     |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/shiny/nested/app.R                                     |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/shiny/output/app.R                                     |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/cli/shiny/simple/app.R                                     |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/curl/doc/intro.R                                           |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/curl/doc/windows.R                                         |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/httr/demo/oauth2-reddit.R                                  |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/httr/demo/oauth2-yelp.R                                    |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/httr/demo/service-account.R                                |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/httr/doc/api-packages.R                                    |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/httr/doc/quickstart.R                                      |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/httr/doc/secrets.R                                         |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/jsonlite/doc/json-aaquickstart.R                           |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/openalexR/R/openalexR                                      |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/openssl/doc/bignum.R                                       |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/openssl/doc/crypto_hashing.R                               |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/openssl/doc/keys.R                                         |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/openssl/doc/secure_rng.R                                   |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/pillar/doc/debugme.R                                       |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/pillar/doc/extending.R                                     |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/pillar/doc/numbers.R                                       |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/pillar/doc/printing.R                                      |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/ci.R                                              |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/docker.R                                          |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/faq.R                                             |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/package-install.R                                 |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/package-sources.R                                 |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/packages.R                                        |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/packrat.R                                         |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/profiles.R                                        |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/python.R                                          |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/renv.R                                            |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/doc/rsconnect.R                                       |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/resources/activate.R                                  |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/resources/vendor/renv.R                               |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/renv/resources/watchdog-process.R                          |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/rgeolocate/doc/Introduction_to_rgeolocate.R                |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/tibble/doc/digits.R                                        |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/tibble/doc/extending.R                                     |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/tibble/doc/formats.R                                       |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/tibble/doc/invariants.R                                    |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/tibble/doc/numbers.R                                       |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/tibble/doc/tibble.R                                        |
|renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/tibble/doc/types.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/brand.yml/app.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/brand.yml/deploy.R                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/deploy.R                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/code_modal.R                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/colors.R                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/mod-global-controls.R           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/mod-selextra.R                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/mod-value-box-ui.R              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/random_plot.R                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/random_values.R                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/shuffleButton.R                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/card/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/card/deploy.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/flights/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/flights/deploy.R                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/value_box/app.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/examples-shiny/value_box/deploy.R                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/themer-demo/app.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/themer-demo/deploy.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/themer-demo/deploy/app.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/themer-demo/global.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/bslib/themer-demo/R/tips.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/examples/apps/news.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/examples/apps/outdated.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/examples/apps/search.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/examples/apps/up.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/exec/news.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/exec/outdated.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/exec/search.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/exec/up.R                                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/shiny/along/app.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/shiny/format/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/shiny/nested/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/shiny/output/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cli/shiny/simple/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/cvdemulator/prepareStaticContent.R                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/cvdemulator/server.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/cvdemulator/ui.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/demo/brewer.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/demo/carto.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/demo/scico.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/demo/viridis.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/doc/colorspace.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/doc/hcl-colors.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/hclcolorpicker/prepareStaticContent.R                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/hclcolorpicker/server.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/hclcolorpicker/ui.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/hclwizard/prepareStaticContent.R                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/hclwizard/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/colorspace/hclwizard/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cpp11/doc/converting.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cpp11/doc/cpp11.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cpp11/doc/FAQ.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cpp11/doc/internals.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/cpp11/doc/motivations.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/curl/doc/intro.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/curl/doc/windows.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/demo/vectorised.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/doc/sha1.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_aes.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_blake3.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_crc32.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_digest.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_digest2int.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_encoding.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_hmac.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_misc.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_new_matrix_behaviour.R                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_num2hex.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_raw.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/digest/tinytest/test_sha1.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/base.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/colwise.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/dplyr.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/grouping.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/in-packages.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/programming.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/rowwise.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/two-table.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/dplyr/doc/window-functions.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/ajax-shiny.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/datatable.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-click/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-click/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-crosstalk/plotly-persist.R                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-deleteRows/server.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-deleteRows/ui.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-edit/app.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-filter/server.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-filter/ui.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-info/server.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-info/ui.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-proxy/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-proxy/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-radio/app.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-reload/app.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-rows/server.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-rows/ui.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-scroller/server.R                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-scroller/ui.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-searchExact/server.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-searchExact/ui.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-selection/server.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-selection/ui.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-shiny/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-shiny/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/DT/examples/DT-updateFilters/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/fansi/doc/sgr-in-rmd.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/fontawesome/apps/138-icon-fontawesome/app.R                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/fs/doc/function-comparisons.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/ggplot2/doc/extending-ggplot2.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/ggplot2/doc/ggplot2-in-packages.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/ggplot2/doc/ggplot2-specs.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/ggplot2/doc/ggplot2.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/glue/doc/engines.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/glue/doc/glue.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/glue/doc/transformers.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/glue/doc/wrappers.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/gridExtra/doc/arrangeGrob.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/gridExtra/doc/gtable.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/gridExtra/doc/ngonGrob.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/gridExtra/doc/tableGrob.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/gridExtra/tests/testthat/test-arrangeGrob.R                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/gridExtra/tests/testthat/test-tableGrob.R                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/gtable/doc/profiling.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/here/demo-project/prepare/penguins.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/here/doc/here.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/here/doc/rmarkdown.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/highr/doc/highr-custom.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/highr/doc/highr-internals.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/htmlwidgets/doc/develop_advanced.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/htmlwidgets/doc/develop_intro.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/httpuv/demo/daemon-echo.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/httpuv/demo/echo.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/httpuv/demo/json-server.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/isoband/doc/isoband1.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/isoband/doc/isoband3.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/jsonlite/doc/json-aaquickstart.R                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/kableExtra/doc/awesome_table_in_html.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/kableExtra/doc/awesome_table_in_pdf.R                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/kableExtra/doc/best_practice_for_newline_in_latex_table.R          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/kableExtra/doc/legacy_features.R                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/kableExtra/doc/use_kable_in_shiny.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/demo/gwidgets.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/demo/notebook.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/doc/datatables.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/doc/knit_expand.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/doc/knitr-intro.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/doc/knitr-markdown.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/examples/knit-all.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/examples/knitr-spin.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/misc/gWidgetsWWW2-knitr.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/misc/stitch-test.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/shiny/server.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/knitr/shiny/ui.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/lazyeval/doc/lazyeval-old.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/lazyeval/doc/lazyeval.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/lifecycle/doc/communicate.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/lifecycle/doc/manage.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/lifecycle/doc/stages.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/magrittr/doc/magrittr.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/magrittr/doc/tradeoffs.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch01.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch02.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch03.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch04.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch05.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch06.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch07.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch08.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch09.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch10.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch11.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch12.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch13.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch14.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch15.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/MASS/scripts/ch16.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/data/CAex.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/data/KNex.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/data/USCounties.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/data/wrld_1deg.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/doc/Comparisons.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/doc/Design-issues.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/doc/Intro2Matrix.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/doc/Introduction.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/doc/sparseModels.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/test-tools-1.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/test-tools-Matrix.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Matrix/test-tools.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/munsell/raw/getmunsellmap.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/mlbook/ch04.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/mlbook/ch05.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/ch01.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/ch02.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/ch03.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/ch04.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/ch05.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/ch06.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/ch08.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/nlme/scripts/runme.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/pillar/doc/debugme.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/pillar/doc/extending.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/pillar/doc/numbers.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/pillar/doc/printing.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/promises/doc/promises_05_future_promise.R                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/purrr/doc/base.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/discovery/cxx0x.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/Attributes/cppFunction.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/Attributes/sourceCpp.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/FastLM/lmArmadillo.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/FastLM/lmGSL.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/OpenMP/check.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/performance/extractors.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/performance/performance.R                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/RcppGibbs/RcppGibbs_Updated.R                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/RcppGibbs/RcppGibbs.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/RcppGibbs/timeRNGs.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/examples/SugarPerformance/sugarBenchmarks.R                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/skeleton/rcpp_hello_world.R                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/skeleton/zzz.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_algorithm.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_as.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_attribute_package.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_attributes.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_binary_package.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_client_package.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_coerce.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_dataframe.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_date.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_dispatch.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_embedded_r.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_environments.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_exceptions_nocall.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_exceptions.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_expose_class.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_function.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_global_rostream.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_interface.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_internal_function_cpp11.R                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_internal_function.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_language.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_listof.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_matrix.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_misc.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_modref.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_module_client_package.R                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_module.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_na.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_packageversion.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_quickanddirty.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_rcpp_package_skeleton.R                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_reference.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_rmath.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_robject.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_s4.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_stack.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_stats.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_string.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_subset.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_sugar_var.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_sugar.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_support.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_system.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_table.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_vector_old.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_vector.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_wrap.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_wstring.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/test_xptr.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppClass/R/load.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppClass/R/rcpp_hello_world.R                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppClass/tests/classes.R                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceExporter/R/exporter.R               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceExporter/R/RcppExports.R            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceUser/R/user.R                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceUser/tests/tests.R                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppModule/R/rcpp_hello_world.R                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppModule/R/zzz.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppModule/tests/modules.R                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppPackage/R/rcpp_hello_world.R                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rcrossref/ignore/cr_types_draft.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rcrossref/ignore/other_fulltext.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/readxl/doc/cell-and-column-types.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/readxl/doc/sheet-geometry.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/remotes/install-github.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/ci.R                                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/docker.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/faq.R                                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/package-install.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/package-sources.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/packages.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/packrat.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/profiles.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/python.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/renv.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/doc/rsconnect.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/resources/activate.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/resources/vendor/renv.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/renv/resources/watchdog-process.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rmarkdown/doc/lua-filters.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rmarkdown/doc/rmarkdown.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rpart/doc/longintro.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rpart/doc/usercode.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rprojroot/doc/rprojroot.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rstudioapi/doc/dialogs.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rstudioapi/doc/document-manipulation.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rstudioapi/doc/projects.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rstudioapi/doc/r-session.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rstudioapi/doc/terminal.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/rstudioapi/doc/visual-mode.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/sass/doc/sass.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/sass/sass-color/app.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/sass/sass-font/app.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/sass/sass-size/app.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/sass/sass-theme/app.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/app.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/R/example-module.R                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/R/example.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/setup-shinytest2.R               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-examplemodule.R             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-server.R                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-shinytest2.R                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-sort.R                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/01_hello/app.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/02_text/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/03_reactivity/app.R                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/04_mpg/app.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/05_sliders/app.R                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/06_tabsets/app.R                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/07_widgets/app.R                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/08_html/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/09_upload/app.R                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/10_download/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples-shiny/11_timer/app.R                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/01_hello/app.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/02_text/app.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/03_reactivity/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/04_mpg/app.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/05_sliders/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/06_tabsets/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/07_widgets/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/09_upload/app.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/10_download/app.R                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/shiny/examples/11_timer/app.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/stringr/doc/from-base.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/stringr/doc/regular-expressions.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/stringr/doc/stringr.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/adjcurve.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/approximate.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/compete.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/concordance.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/matrix.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/methods.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/population.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/redistribute.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/splines.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/survival.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/tiedtimes.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/timedep.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/survival/doc/validate.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/svglite/doc/fonts.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/systemfonts/doc/c_interface.R                                      |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tibble/doc/digits.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tibble/doc/extending.R                                             |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tibble/doc/formats.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tibble/doc/invariants.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tibble/doc/numbers.R                                               |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tibble/doc/tibble.R                                                |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tibble/doc/types.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidylog/doc/benchmarks.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyr/doc/in-packages.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyr/doc/nest.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyr/doc/pivot.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyr/doc/programming.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyr/doc/rectangle.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyr/doc/tidy-data.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyselect/doc/syntax.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/tidyselect/doc/tidyselect.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/triebeard/doc/r_radix.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/triebeard/doc/rcpp_radix.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/urltools/doc/urltools.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/vctrs/doc/pillar.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/vctrs/doc/s3-vector.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/vctrs/doc/stability.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/vctrs/doc/type-size.R                                              |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/viridis/doc/intro-to-viridis.R                                     |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/withr/doc/withr.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/xfun/doc/xfun.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/xfun/scripts/call-fun.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/xml2/doc/modification.R                                            |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/xtable/doc/listOfTablesGallery.R                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/xtable/doc/margintable.R                                           |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/xtable/doc/OtherPackagesGallery.R                                  |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/xtable/doc/xtableGallery.R                                         |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/yaml/tests/test_as_yaml.R                                          |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/yaml/tests/test_read_yaml.R                                        |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/yaml/tests/test_write_yaml.R                                       |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/yaml/tests/test_yaml_load_file.R                                   |
|renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu/yaml/tests/test_yaml_load.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bit/doc/bit-demo.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bit/doc/bit-performance.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bit/doc/bit-usage.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/broom/doc/available-methods.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/broom/doc/bootstrapping.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/broom/doc/broom_and_dplyr.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/broom/doc/broom.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/brand.yml/app.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/brand.yml/deploy.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/deploy.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/code_modal.R                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/colors.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/mod-global-controls.R           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/mod-selextra.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/mod-value-box-ui.R              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/random_plot.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/random_values.R                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/build-a-box/R/shuffleButton.R                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/card/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/card/deploy.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/flights/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/flights/deploy.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/value_box/app.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/examples-shiny/value_box/deploy.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/themer-demo/app.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/themer-demo/deploy.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/themer-demo/deploy/app.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/themer-demo/global.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/bslib/themer-demo/R/tips.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/classInt/doc/headtailsR.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/classInt/tinytest/test_box.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/classInt/tinytest/test_largeN.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/classInt/tinytest/test_quantile_probs.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/examples/apps/news.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/examples/apps/outdated.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/examples/apps/search.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/examples/apps/up.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/exec/news.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/exec/outdated.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/exec/search.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/exec/up.R                                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/shiny/along/app.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/shiny/format/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/shiny/nested/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/shiny/output/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cli/shiny/simple/app.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/clipr/doc/developing-with-clipr.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/cvdemulator/prepareStaticContent.R                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/cvdemulator/server.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/cvdemulator/ui.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/demo/brewer.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/demo/carto.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/demo/scico.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/demo/viridis.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/doc/colorspace.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/doc/hcl-colors.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/hclcolorpicker/prepareStaticContent.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/hclcolorpicker/server.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/hclcolorpicker/ui.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/hclwizard/prepareStaticContent.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/hclwizard/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/colorspace/hclwizard/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cpp11/doc/converting.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cpp11/doc/cpp11.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cpp11/doc/FAQ.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cpp11/doc/internals.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/cpp11/doc/motivations.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/curl/doc/intro.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/curl/doc/windows.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-faq.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-intro.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-joins.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-keys-fast-subset.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-programming.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-reference-semantics.R                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-reshape.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-sd-usage.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/data.table/doc/datatable-secondary-indices-and-auto-indexing.R     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DBI/doc/backend.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DBI/doc/DBI-advanced.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DBI/doc/DBI-arrow.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DBI/doc/DBI.R                                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DBI/doc/spec.R                                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dbplyr/doc/backend-2.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dbplyr/doc/dbplyr.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dbplyr/doc/new-backend.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dbplyr/doc/reprex.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dbplyr/doc/sql.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dbplyr/doc/translation-function.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dbplyr/doc/translation-verb.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/diffobj/doc/diffobj.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/diffobj/doc/embed.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/demo/vectorised.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/doc/sha1.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_aes.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_blake3.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_crc32.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_digest.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_digest2int.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_encoding.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_hmac.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_misc.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_new_matrix_behaviour.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_num2hex.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_raw.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/digest/tinytest/test_sha1.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/base.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/colwise.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/dplyr.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/grouping.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/in-packages.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/programming.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/rowwise.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/two-table.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dplyr/doc/window-functions.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/ajax-shiny.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/datatable.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-click/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-click/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-crosstalk/plotly-persist.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-deleteRows/server.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-deleteRows/ui.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-edit/app.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-filter/server.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-filter/ui.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-info/server.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-info/ui.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-proxy/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-proxy/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-radio/app.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-reload/app.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-rows/server.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-rows/ui.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-scroller/server.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-scroller/ui.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-searchExact/server.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-searchExact/ui.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-selection/server.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-selection/ui.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-shiny/server.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-shiny/ui.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/DT/examples/DT-updateFilters/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/dtplyr/doc/translation.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/e1071/doc/svmdoc.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/fansi/doc/sgr-in-rmd.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/fontawesome/apps/138-icon-fontawesome/app.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/forcats/doc/forcats.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/fs/doc/function-comparisons.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/discovery-doc-ingest/discover-discovery.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/discovery-doc-ingest/drive-example.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/discovery-doc-ingest/ingest-functions.R                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/auth-from-web.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/gargle-auth-in-client-package.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/get-api-credentials.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/how-gargle-gets-tokens.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/non-interactive-auth.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/oauth-client-not-app.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/request-helper-functions.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gargle/doc/troubleshooting.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gert/doc/gert.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggpattern/doc/developing-patterns.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggpattern/doc/patterns-noise.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggpattern/doc/patterns-points.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggpattern/doc/patterns-stripes.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggplot2/doc/extending-ggplot2.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggplot2/doc/ggplot2-in-packages.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggplot2/doc/ggplot2-specs.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ggplot2/doc/ggplot2.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gh/doc/managing-personal-access-tokens.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gitcreds/doc/package.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/glue/doc/engines.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/glue/doc/glue.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/glue/doc/transformers.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/glue/doc/wrappers.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridExtra/doc/arrangeGrob.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridExtra/doc/gtable.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridExtra/doc/ngonGrob.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridExtra/doc/tableGrob.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridExtra/tests/testthat/test-arrangeGrob.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridExtra/tests/testthat/test-tableGrob.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridpattern/doc/developing-patterns.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gridpattern/doc/tiling.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/gtable/doc/profiling.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/haven/doc/semantics.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/here/demo-project/prepare/penguins.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/here/doc/here.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/here/doc/rmarkdown.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/highr/doc/highr-custom.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/highr/doc/highr-internals.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/htmlwidgets/doc/develop_advanced.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/htmlwidgets/doc/develop_intro.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httpuv/demo/daemon-echo.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httpuv/demo/echo.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httpuv/demo/json-server.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httr/demo/oauth2-reddit.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httr/demo/oauth2-yelp.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httr/demo/service-account.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httr/doc/api-packages.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httr/doc/quickstart.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httr/doc/secrets.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/httr2/doc/httr2.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ids/doc/ids.R                                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/isoband/doc/isoband1.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/isoband/doc/isoband3.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/jsonlite/doc/json-aaquickstart.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/kableExtra/doc/awesome_table_in_html.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/kableExtra/doc/awesome_table_in_pdf.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/kableExtra/doc/best_practice_for_newline_in_latex_table.R          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/kableExtra/doc/legacy_features.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/kableExtra/doc/use_kable_in_shiny.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/knitr/doc/datatables.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/knitr/doc/knit_expand.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/knitr/doc/knitr-intro.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/knitr/doc/knitr-markdown.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/knitr/examples/knit-all.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/knitr/examples/knitr-spin.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/knitr/misc/stitch-test.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lattice/demo/intervals.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lattice/demo/labels.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lattice/demo/lattice.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lattice/demo/panel.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lattice/doc/grid.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lazyeval/doc/lazyeval-old.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lazyeval/doc/lazyeval.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lifecycle/doc/communicate.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lifecycle/doc/manage.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lifecycle/doc/stages.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/litedown/doc/slides.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/lubridate/doc/lubridate.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/magrittr/doc/magrittr.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/magrittr/doc/tradeoffs.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/markdown/examples/render-options.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch01.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch02.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch03.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch04.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch05.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch06.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch07.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch08.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch09.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch10.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch11.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch12.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch13.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch14.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch15.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/MASS/scripts/ch16.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/data/CAex.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/data/KNex.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/data/USCounties.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/data/wrld_1deg.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/doc/Comparisons.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/doc/Design-issues.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/doc/Intro2Matrix.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/doc/Introduction.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/doc/sparseModels.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/test-tools-1.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/test-tools-Matrix.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Matrix/test-tools.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/munsell/raw/getmunsellmap.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/mlbook/ch04.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/mlbook/ch05.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/ch01.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/ch02.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/ch03.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/ch04.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/ch05.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/ch06.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/ch08.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/nlme/scripts/runme.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/openalexR/R/openalexR                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/openssl/doc/bignum.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/openssl/doc/crypto_hashing.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/openssl/doc/keys.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/openssl/doc/secure_rng.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/patchwork/doc/patchwork.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pillar/doc/debugme.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pillar/doc/extending.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pillar/doc/numbers.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pillar/doc/printing.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/accessibility.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/customise.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/how-to-update-released-site.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/linking.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/metadata.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/pkgdown.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/quarto.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/pkgdown/doc/translations.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/animation-tour-basic.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/animation-tour-USArrests.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-filter-dynamic-axis.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-filter-lines.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-binned-target-a.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-binned-target-b.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-binned-target-c.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-epl-2.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-epl.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-ggpairs.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-ggplotly.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-intro.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-leaflet.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-pipeline.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/crosstalk-highlight-subplot.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/custom-javascript.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/rotate.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-dt.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-geo.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-ggplot2.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-mapbox-data.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-mapbox-layout.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-mapbox-style.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-plotly-3D-globe.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/sf-plotly-storms.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/demo/ternary.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/docs.R                                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/async/app.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/crossfilter_compare/app.R                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/crossfilter_kde/app.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/crossfilter_scatter/app.R                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/crossfilter/app.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/Diamonds/server.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/Diamonds/ui.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/drag_brush/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/drag_lines/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/drag_markers/app.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/drill_down/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/DT/app.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_3D/app.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_annotation/app.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_click_map/app.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_click/app.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_legends/app.R                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_modules/app.R                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_parcoords/app.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_persist/app.R                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data_select/app.R                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data/tests/testthat.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data/tests/testthat/setup-shinytest2.R |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_data/tests/testthat/test-shinytest2.R  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/event_priority/app.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/ggplotly_sizing/app.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/lmGadget/app.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/MathJax/app.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/Movies/server.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/Movies/ui.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/proxy_mapbox/app.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/proxy_relayout/app.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/proxy_restyle_canada/app.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/proxy_restyle_economics/app.R                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/stream/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/UN_Advanced/global.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/UN_Advanced/server.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/UN_Advanced/ui.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/UN_Simple/global.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/UN_Simple/server.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/examples/shiny/UN_Simple/ui.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/plotly/stars.R                                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/profvis/doc/profvis.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/promises/doc/promises_05_future_promise.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/proxy/doc/overview.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ps/tools/error-codes.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/ps/tools/winver.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/purrr/doc/base.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/discovery/cxx0x.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/Attributes/cppFunction.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/Attributes/sourceCpp.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/FastLM/lmArmadillo.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/FastLM/lmGSL.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/OpenMP/check.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/performance/extractors.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/performance/performance.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/RcppGibbs/RcppGibbs_Updated.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/RcppGibbs/RcppGibbs.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/RcppGibbs/timeRNGs.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/examples/SugarPerformance/sugarBenchmarks.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/skeleton/rcpp_hello_world.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/skeleton/zzz.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_algorithm.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_as.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_attribute_package.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_attributes.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_binary_package.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_client_package.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_coerce.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_dataframe.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_date.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_dispatch.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_embedded_r.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_environments.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_exceptions_nocall.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_exceptions.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_expose_class.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_function.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_global_rostream.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_interface.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_internal_function_cpp11.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_internal_function.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_language.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_listof.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_matrix.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_misc.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_modref.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_module_client_package.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_module.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_na.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_packageversion.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_quickanddirty.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_rcpp_package_skeleton.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_reference.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_rmath.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_robject.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_s4.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_stack.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_stats.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_string.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_subset.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_sugar_var.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_sugar.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_support.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_system.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_table.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_vector_old.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_vector.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_wrap.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_wstring.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/test_xptr.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppClass/R/load.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppClass/R/rcpp_hello_world.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppClass/tests/classes.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceExporter/R/exporter.R               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceExporter/R/RcppExports.R            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceUser/R/user.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppInterfaceUser/tests/tests.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppModule/R/rcpp_hello_world.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppModule/R/zzz.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppModule/tests/modules.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/Rcpp/tinytest/testRcppPackage/R/rcpp_hello_world.R                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rcrossref/ignore/cr_types_draft.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rcrossref/ignore/other_fulltext.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/readr/doc/column-types.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/readr/doc/locales.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/readr/doc/readr.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/readxl/doc/cell-and-column-types.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/readxl/doc/sheet-geometry.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/remotes/install-github.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/ci.R                                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/docker.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/faq.R                                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/package-install.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/package-sources.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/packages.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/packrat.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/profiles.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/python.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/renv.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/doc/rsconnect.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/resources/activate.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/resources/vendor/renv.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/renv/resources/watchdog-process.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/reprex/doc/reprex-dos-and-donts.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/reprex/templates/BETTER_THAN_NOTHING.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rgeolocate/doc/Introduction_to_rgeolocate.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rmarkdown/doc/lua-filters.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rmarkdown/doc/rmarkdown.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/extending.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/index-crossref.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/namespace.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/rd-formatting.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/rd-other.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/rd.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/reuse.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/roxygen2/doc/roxygen2.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rprojroot/doc/rprojroot.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rstudioapi/doc/dialogs.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rstudioapi/doc/document-manipulation.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rstudioapi/doc/projects.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rstudioapi/doc/r-session.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rstudioapi/doc/terminal.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rstudioapi/doc/visual-mode.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rvest/demo/tripadvisor.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rvest/demo/united.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rvest/demo/zillow.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rvest/doc/rvest.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/rvest/doc/starwars.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sass/doc/sass.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sass/sass-color/app.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sass/sass-font/app.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sass/sass-size/app.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sass/sass-theme/app.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/demo/affine.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/demo/basic.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/demo/ggplot.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/demo/meuse_sf.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/demo/nc.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/demo/twitter.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/doc/sf1.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/doc/sf2.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/doc/sf3.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/doc/sf4.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/doc/sf5.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/doc/sf6.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/sf/doc/sf7.R                                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/app.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/R/example-module.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/R/example.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/setup-shinytest2.R               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-examplemodule.R             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-server.R                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-shinytest2.R                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/app_template/tests/testthat/test-sort.R                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/01_hello/app.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/02_text/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/03_reactivity/app.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/04_mpg/app.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/05_sliders/app.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/06_tabsets/app.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/07_widgets/app.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/08_html/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/09_upload/app.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/10_download/app.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples-shiny/11_timer/app.R                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/01_hello/app.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/02_text/app.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/03_reactivity/app.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/04_mpg/app.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/05_sliders/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/06_tabsets/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/07_widgets/app.R                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/09_upload/app.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/10_download/app.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/shiny/examples/11_timer/app.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/skimr/doc/extending_skimr.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/skimr/doc/Skimr_defaults.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/skimr/doc/skimr.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/skimr/doc/Using_fonts.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/stringr/doc/from-base.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/stringr/doc/regular-expressions.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/stringr/doc/stringr.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/svglite/doc/fonts.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/systemfonts/doc/c_interface.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/doc/custom-expectation.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/doc/parallel.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/doc/skipping.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/doc/snapshotting.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/doc/special-files.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/doc/test-fixtures.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/doc/third-edition.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/examples/test-failure.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/examples/test-success.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/resources/catch-routine-registration.R                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/testthat/resources/test-cpp.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/textshaping/doc/c_interface.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tibble/doc/digits.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tibble/doc/extending.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tibble/doc/formats.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tibble/doc/invariants.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tibble/doc/numbers.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tibble/doc/tibble.R                                                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tibble/doc/types.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidylog/doc/benchmarks.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyr/doc/in-packages.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyr/doc/nest.R                                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyr/doc/pivot.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyr/doc/programming.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyr/doc/rectangle.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyr/doc/tidy-data.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyselect/doc/syntax.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyselect/doc/tidyselect.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/tidyverse/doc/paper.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/triebeard/doc/r_radix.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/triebeard/doc/rcpp_radix.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/units/demo/cf.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/units/demo/ggplot2.R                                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/units/demo/year.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/units/doc/measurement_units_in_R.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/units/doc/units.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/urlchecker/tools/urltools.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/urlchecker/tools/utils.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/urltools/doc/urltools.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/citation-template.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/junit-testthat.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/packagename-data-prep.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/packagename-package.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/pipe.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/test-example-2.1.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/testthat.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/usethis/templates/vscode-debug.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vctrs/doc/pillar.R                                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vctrs/doc/s3-vector.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vctrs/doc/stability.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vctrs/doc/type-size.R                                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/viridis/doc/intro-to-viridis.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-long/data.table-data.table.R             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-long/input.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-long/read.delim-base.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-long/readr-dplyr.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-long/vroom_no_altrep-dplyr.R             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-long/vroom-base.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-long/vroom-dplyr.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-wide/data.table-data.table.R             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-wide/input.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-wide/read.delim-base.R                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-wide/readr-dplyr.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-wide/vroom_no_altrep-dplyr.R             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-wide/vroom-base.R                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_character-wide/vroom-dplyr.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/data.table-data.table.R               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/input.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/read.delim-base.R                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/readr-dplyr.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/vroom_no_altrep-base.R                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/vroom_no_altrep-dplyr.R               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/vroom-base.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-long/vroom-dplyr.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/data.table-data.table.R               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/input.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/read.delim-base.R                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/readr-dplyr.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/vroom_no_altrep-base.R                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/vroom_no_altrep-dplyr.R               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/vroom-base.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/all_numeric-wide/vroom-dplyr.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/fwf/read.delim-base.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/fwf/readr-dplyr.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/fwf/vroom_no_altrep-dplyr.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/fwf/vroom-base.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/fwf/vroom-dplyr.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/run-bench-fwf.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/run-bench.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/session_info.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/summarise-benchmarks.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_multiple/data.table-data.table.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_multiple/readr-dplyr.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_multiple/vroom_no_altrep-dplyr.R                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_multiple/vroom-base.R                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_multiple/vroom-dplyr.R                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/base-gzip.R                               |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/base-multithreaded_gzip.R                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/base-uncompressed.R                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/base-zstandard.R                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/data.table-gzip.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/data.table-multithreaded_gzip.R           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/data.table-uncompressed.R                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/readr-gzip.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/readr-multithreaded_gzip.R                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/readr-uncompressed.R                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/readr-zstandard.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/vroom-gzip.R                              |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/vroom-multithreaded_gzip.R                |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/vroom-uncompressed.R                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi_writing/vroom-zstandard.R                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi/data.table-data.table.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi/read.delim-base.R                                 |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi/readr-dplyr.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi/vroom_no_altrep-dplyr.R                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi/vroom-base.R                                      |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/bench/taxi/vroom-dplyr.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/doc/benchmarks.R                                             |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/vroom/doc/vroom.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/specs/convert.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/tests/testComments.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/tests/testdelimiters.R                                     |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/tests/testinterpolation.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/tests/testinverted.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/tests/testName.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/tests/testpartials.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/whisker/tests/testsections.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/withr/doc/withr.R                                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/xfun/doc/xfun.R                                                    |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/xfun/scripts/call-fun.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/xml2/doc/modification.R                                            |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/xtable/doc/listOfTablesGallery.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/xtable/doc/margintable.R                                           |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/xtable/doc/OtherPackagesGallery.R                                  |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/xtable/doc/xtableGallery.R                                         |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/yaml/tests/test_as_yaml.R                                          |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/yaml/tests/test_read_yaml.R                                        |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/yaml/tests/test_write_yaml.R                                       |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/yaml/tests/test_yaml_load_file.R                                   |
|renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/yaml/tests/test_yaml_load.R                                        |

### Software citations




Only directly loaded libraries are cited. For all libraries used in the runtime environment, see the Appendix.


|Package     |Version  |Citation                                       |
|:-----------|:--------|:----------------------------------------------|
|arrow       |20.0.0.2 |@arrow                                         |
|base        |4.5.1    |@base                                          |
|countrycode |1.6.1    |@countrycode                                   |
|devtools    |2.4.6    |@devtools                                      |
|ggpattern   |1.1.4    |@ggpattern                                     |
|here        |1.0.1    |@here                                          |
|kableExtra  |1.4.0    |@kableExtra                                    |
|knitr       |1.50     |@knitr2014; @knitr2015; @knitr2025             |
|mapdata     |2.3.1    |@mapdata                                       |
|maps        |3.4.3    |@maps                                          |
|patchwork   |1.3.1    |@patchwork                                     |
|rcrossref   |1.2.0    |@rcrossref                                     |
|remotes     |2.5.0    |@remotes                                       |
|renv        |1.1.5    |@renv                                          |
|rgeolocate  |1.4.2    |@rgeolocate                                    |
|rmarkdown   |2.30     |@rmarkdown2018; @rmarkdown2020; @rmarkdown2025 |
|rprojroot   |2.1.1    |@rprojroot                                     |
|scales      |1.3.0    |@scales                                        |
|skimr       |2.1.5    |@skimr                                         |
|tidylog     |1.1.0    |@tidylog                                       |
|tidyverse   |2.0.0    |@tidyverse                                     |
|viridis     |0.6.5    |@viridis                                       |
|xtable      |1.8.4    |@xtable                                        |


## Controlled Randomness





- [ ] Random seed is set at line _____ of program ______
- [x] No Pseudo random generator is used in the analysis described here.

Note that re-running the API queries (turned off by default) **will** generate different data, which is likely to affect the regression output.

## Memory, Runtime, Storage Requirements

### Summary

Approximate time needed to reproduce the analyses on a standard (CURRENT YEAR) desktop machine (when not running API queries):

- [ ] <10 minutes
- [x] 10-60 minutes
- [ ] 1-2 hours
- [ ] 2-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [ ] 3-14 days
- [ ] > 14 days

API queries can take a long time, are not guaranteed to work, and are not guaranteed to return the same results. All API queries are stored as of the last run, and made available in the replication package.

Approximate storage space needed:

- [ ] < 25 MBytes
- [x] 25 MB - 250 MB
- [ ] 250 MB - 2 GB
- [ ] 2 GB - 25 GB
- [ ] 25 GB - 250 GB
- [ ] > 250 GB

- [ ] Not feasible to run on a desktop machine, as described below.

### Details




The code was last run on 

- OS: "Ubuntu 24.04.3 LTS"
- Processor:  13th Gen Intel(R) Core(TM) i7-1365U, 12 cores
- Memory available: 30GB memory

- R version 4.5.1 (2025-06-13) on x86_64, linux-gnu



# Description of programs/code





Each numbered R program can be run independently, in the sequence implied by the numbering scheme. 
A convenience bash main script (`run.sh`) to run all programs is provided in the root of the project, and will run all data cleaning and analysis programs.

Of note, several programs leverage APIs, which can yield different results, and might take  long time. The programs will detect previously downloaded data files, and skip the download part if those files are present. To start with a fresh download, delete the following files:


Internet access is required to run the programs, 

- to download the key input files 


##  License for Code


The code (all files ending in `.R`, `.Rmd`, and `.sh`) is licensed under a BSD license. See [LICENSE.txt](LICENSE.txt) for details.

# Instructions to Replicators

If using Docker image on Linux or macOS system:

- run `start_rstudio.sh` and connect to [https://localhost:8787](https://localhost:8787)
- in the "Terminal" of the RStudio app, run `bash ./run.sh`

or equivalently,

- in the terminal of a computer with Docker installed, run `bash ./run_docker.sh ./run.sh`

Alternative ways to run this (these were not tested):

- optionally, before running project code, run `Rscript -e "renv::init()"` (on Windows, `Rscript.exe -e "renv::init()"` ) to isolate the project libraries from your system (assumes `renv` is installed, see [renv](https://rstudio.github.io/renv/articles/renv.html)).
- Using the same R version as described above, run each program individually as desired, in the order indicated above.

## Details

> INSTRUCTIONS: The following list needs to be manually maintained!


```
## mutate: new variable 'File.exists' (logical) with one unique value and 0% NA
## filter: removed all rows (100%)
## select: dropped one variable (File.exists)
```



|Filename |Note |
|:--------|:----|


# List of tables and programs


The provided code reproduces:

- [x] All numbers provided in text in the paper
- [x] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below.

The code also produces numerous tables which were not included in the paper. 


|Table number |Program                |LaTeX file                                |
|:------------|:----------------------|:-----------------------------------------|
|1            |programs/10_analysis.R |text/includes/table_article_selection.tex |
|2            |NA                     |No code                                   |
|3            |programs/31_results2.R |text/analysis/table_code.tex              |
|4            |programs/31_results2.R |text/analysis/table_code_year.tex         |
|A1           |programs/20_appendix.R |text/analysis/table_appendix.tex          |
|A2           |programs/10_analysis.R |text/includes/table_absence.tex           |

---

# Acknowledgements

This README based on the template (V1.1) created by @template-readme. 

# References






<div id="refs"></div>

#  Appendix {-} 

## Appendix: System and package info

Packages are listed here if they are loaded by the code.




```
## ─ Session info ───────────────────────────────────────────────────────────────
##  setting  value
##  version  R version 4.5.1 (2025-06-13)
##  os       openSUSE Leap 15.6
##  system   x86_64, linux-gnu
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  ctype    en_US.UTF-8
##  tz       America/Montreal
##  date     2025-08-13
##  pandoc   3.1.11.1 @ /usr/bin/pandoc
##  quarto   1.6.40 @ /home/vilhuber/bin/quarto
## 
## ─ Packages ───────────────────────────────────────────────────────────────────
##  ! package     * version date (UTC) lib source
##  P cachem        1.1.0   2024-05-16 [?] CRAN (R 4.5.0)
##    cli           3.6.5   2025-04-23 [1] RSPM (R 4.5.1)
##  P devtools      2.4.5   2022-10-11 [?] RSPM
##  P digest        0.6.37  2024-08-19 [?] RSPM (R 4.5.0)
##  P ellipsis      0.3.2   2021-04-29 [?] RSPM
##  P fastmap       1.2.0   2024-05-15 [?] CRAN (R 4.5.0)
##  P fs            1.6.5   2024-10-30 [?] RSPM (R 4.5.0)
##  P glue          1.8.0   2024-09-30 [?] CRAN (R 4.5.0)
##  P here          1.0.1   2020-12-13 [?] RSPM (R 4.5.0)
##  P htmltools     0.5.8.1 2024-04-04 [?] RSPM (R 4.5.0)
##  P htmlwidgets   1.6.4   2023-12-06 [?] RSPM (R 4.5.0)
##  P httpuv        1.6.15  2024-03-26 [?] RSPM (R 4.5.0)
##  P later         1.4.1   2024-11-27 [?] RSPM (R 4.5.0)
##  P lifecycle     1.0.4   2023-11-07 [?] CRAN (R 4.5.0)
##  P magrittr      2.0.3   2022-03-30 [?] CRAN (R 4.5.0)
##  P memoise       2.0.1   2021-11-26 [?] CRAN (R 4.5.0)
##    mime          0.13    2025-03-17 [1] RSPM (R 4.5.0)
##  P miniUI        0.1.1.1 2018-05-18 [?] RSPM (R 4.5.0)
##  P pkgbuild      1.4.8   2025-05-26 [?] RSPM
##  P pkgload       1.4.0   2024-06-28 [?] RSPM
##  P profvis       0.4.0   2024-09-20 [?] RSPM
##  P promises      1.3.2   2024-11-28 [?] RSPM (R 4.5.0)
##  P purrr         1.0.4   2025-02-05 [?] CRAN (R 4.5.0)
##  P R6            2.6.1   2025-02-15 [?] CRAN (R 4.5.0)
##  P Rcpp          1.0.14  2025-01-12 [?] RSPM (R 4.5.0)
##  P remotes       2.5.0   2024-03-17 [?] RSPM (R 4.5.0)
##    renv          1.1.1   2025-02-07 [1] RSPM (R 4.5.0)
##    rlang         1.1.6   2025-04-11 [1] RSPM (R 4.5.1)
##  P rprojroot     2.0.4   2023-11-05 [?] RSPM (R 4.5.0)
##  P sessioninfo   1.2.3   2025-02-05 [?] RSPM
##  P shiny         1.10.0  2024-12-14 [?] RSPM (R 4.5.0)
##  P urlchecker    1.0.1   2021-11-30 [?] RSPM
##  P usethis       3.1.0   2024-11-26 [?] RSPM
##  P vctrs         0.6.5   2023-12-01 [?] CRAN (R 4.5.0)
##  P xtable        1.8-4   2019-04-21 [?] RSPM (R 4.5.0)
## 
##  [1] /home/vilhuber/Workspace/Github/economics-open-science/renv/library/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu
##  [2] /home/vilhuber/.cache/R/renv/sandbox/linux-opensuse-leap-15.6/R-4.5/x86_64-suse-linux-gnu/60c4e220
## 
##  P ── Loaded and on-disk path mismatch.
## 
## ──────────────────────────────────────────────────────────────────────────────
##                _                           
## platform       x86_64-suse-linux-gnu       
## arch           x86_64                      
## os             linux-gnu                   
## system         x86_64, linux-gnu           
## status                                     
## major          4                           
## minor          5.1                         
## year           2025                        
## month          06                          
## day            13                          
## svn rev        88306                       
## language       R                           
## version.string R version 4.5.1 (2025-06-13)
## nickname       Great Square Root
```

## Appendix: R Libraries Available on This System

This lists the packages that are installed. If you are not using a package environment manager, this list might be very long and useless.

### renv Environment Detected


Table: List of installed packages (185 total)

|              |Package       |Version  |Source     |Installed |
|:-------------|:-------------|:--------|:----------|:---------|
|arrow         |arrow         |20.0.0.2 |Repository |TRUE      |
|askpass       |askpass       |1.2.1    |Repository |TRUE      |
|assertthat    |assertthat    |0.2.1    |Repository |TRUE      |
|backports     |backports     |1.5.0    |Repository |TRUE      |
|base64enc     |base64enc     |0.1-3    |Repository |TRUE      |
|bit           |bit           |4.6.0    |Repository |TRUE      |
|bit64         |bit64         |4.6.0-1  |Repository |TRUE      |
|blob          |blob          |1.2.4    |Repository |TRUE      |
|brew          |brew          |1.0-10   |Repository |TRUE      |
|brio          |brio          |1.1.5    |Repository |TRUE      |
|broom         |broom         |1.0.8    |Repository |TRUE      |
|bslib         |bslib         |0.9.0    |Repository |TRUE      |
|cachem        |cachem        |1.1.0    |Repository |TRUE      |
|callr         |callr         |3.7.6    |Repository |TRUE      |
|cellranger    |cellranger    |1.1.0    |Repository |TRUE      |
|class         |class         |7.3-22   |Repository |TRUE      |
|classInt      |classInt      |0.4-11   |Repository |TRUE      |
|cli           |cli           |3.6.5    |Repository |TRUE      |
|clipr         |clipr         |0.8.0    |Repository |TRUE      |
|clisymbols    |clisymbols    |1.2.0    |Repository |TRUE      |
|colorspace    |colorspace    |2.1-1    |Repository |TRUE      |
|commonmark    |commonmark    |1.9.2    |Repository |TRUE      |
|conflicted    |conflicted    |1.2.0    |Repository |TRUE      |
|countrycode   |countrycode   |1.6.1    |Repository |TRUE      |
|cpp11         |cpp11         |0.5.2    |Repository |TRUE      |
|crayon        |crayon        |1.5.3    |Repository |TRUE      |
|credentials   |credentials   |2.0.2    |Repository |TRUE      |
|crosstalk     |crosstalk     |1.2.1    |Repository |TRUE      |
|crul          |crul          |1.5.0    |Repository |TRUE      |
|curl          |curl          |6.4.0    |Repository |TRUE      |
|data.table    |data.table    |1.17.6   |Repository |TRUE      |
|DBI           |DBI           |1.2.3    |Repository |TRUE      |
|dbplyr        |dbplyr        |2.5.0    |Repository |TRUE      |
|desc          |desc          |1.4.3    |Repository |TRUE      |
|devtools      |devtools      |2.4.5    |Repository |TRUE      |
|diffobj       |diffobj       |0.3.6    |Repository |TRUE      |
|digest        |digest        |0.6.37   |Repository |TRUE      |
|downlit       |downlit       |0.4.4    |Repository |TRUE      |
|dplyr         |dplyr         |1.1.4    |Repository |TRUE      |
|DT            |DT            |0.33     |Repository |TRUE      |
|dtplyr        |dtplyr        |1.3.1    |Repository |TRUE      |
|e1071         |e1071         |1.7-16   |Repository |TRUE      |
|ellipsis      |ellipsis      |0.3.2    |Repository |TRUE      |
|evaluate      |evaluate      |1.0.3    |Repository |TRUE      |
|fansi         |fansi         |1.0.6    |Repository |TRUE      |
|farver        |farver        |2.1.2    |Repository |TRUE      |
|fastmap       |fastmap       |1.2.0    |Repository |TRUE      |
|fontawesome   |fontawesome   |0.5.3    |Repository |TRUE      |
|forcats       |forcats       |1.0.0    |Repository |TRUE      |
|fs            |fs            |1.6.5    |Repository |TRUE      |
|gargle        |gargle        |1.5.2    |Repository |TRUE      |
|generics      |generics      |0.1.3    |Repository |TRUE      |
|gert          |gert          |2.1.5    |Repository |TRUE      |
|ggpattern     |ggpattern     |1.1.4    |Repository |TRUE      |
|ggplot2       |ggplot2       |3.5.1    |Repository |TRUE      |
|gh            |gh            |1.5.0    |Repository |TRUE      |
|gitcreds      |gitcreds      |0.1.2    |Repository |TRUE      |
|glue          |glue          |1.8.0    |Repository |TRUE      |
|googledrive   |googledrive   |2.1.1    |Repository |TRUE      |
|googlesheets4 |googlesheets4 |1.1.1    |Repository |TRUE      |
|gridExtra     |gridExtra     |2.3      |Repository |TRUE      |
|gridpattern   |gridpattern   |1.3.1    |Repository |TRUE      |
|gtable        |gtable        |0.3.6    |Repository |TRUE      |
|haven         |haven         |2.5.5    |Repository |TRUE      |
|here          |here          |1.0.1    |Repository |TRUE      |
|highr         |highr         |0.11     |Repository |TRUE      |
|hms           |hms           |1.1.3    |Repository |TRUE      |
|htmltools     |htmltools     |0.5.8.1  |Repository |TRUE      |
|htmlwidgets   |htmlwidgets   |1.6.4    |Repository |TRUE      |
|httpcode      |httpcode      |0.3.0    |Repository |TRUE      |
|httpuv        |httpuv        |1.6.15   |Repository |TRUE      |
|httr          |httr          |1.4.7    |Repository |TRUE      |
|httr2         |httr2         |1.2.1    |Repository |TRUE      |
|ids           |ids           |1.0.1    |Repository |TRUE      |
|ini           |ini           |0.3.1    |Repository |TRUE      |
|isoband       |isoband       |0.2.7    |Repository |TRUE      |
|jquerylib     |jquerylib     |0.1.4    |Repository |TRUE      |
|jsonlite      |jsonlite      |2.0.0    |Repository |TRUE      |
|kableExtra    |kableExtra    |1.4.0    |Repository |TRUE      |
|KernSmooth    |KernSmooth    |2.23-21  |Repository |TRUE      |
|knitr         |knitr         |1.49     |Repository |TRUE      |
|labeling      |labeling      |0.4.3    |Repository |TRUE      |
|later         |later         |1.4.1    |Repository |TRUE      |
|lattice       |lattice       |0.22-6   |Repository |TRUE      |
|lazyeval      |lazyeval      |0.2.2    |Repository |TRUE      |
|lifecycle     |lifecycle     |1.0.4    |Repository |TRUE      |
|lubridate     |lubridate     |1.9.4    |Repository |TRUE      |
|magrittr      |magrittr      |2.0.3    |Repository |TRUE      |
|mapdata       |mapdata       |2.3.1    |Repository |TRUE      |
|maps          |maps          |3.4.3    |Repository |TRUE      |
|MASS          |MASS          |7.3-64   |Repository |TRUE      |
|Matrix        |Matrix        |1.7-2    |Repository |TRUE      |
|memoise       |memoise       |2.0.1    |Repository |TRUE      |
|mgcv          |mgcv          |1.9-1    |Repository |TRUE      |
|mime          |mime          |0.13     |Repository |TRUE      |
|miniUI        |miniUI        |0.1.1.1  |Repository |TRUE      |
|modelr        |modelr        |0.1.11   |Repository |TRUE      |
|munsell       |munsell       |0.5.1    |Repository |TRUE      |
|nlme          |nlme          |3.1-167  |Repository |TRUE      |
|openssl       |openssl       |2.3.3    |Repository |TRUE      |
|patchwork     |patchwork     |1.3.1    |Repository |TRUE      |
|pillar        |pillar        |1.11.0   |Repository |TRUE      |
|pkgbuild      |pkgbuild      |1.4.8    |Repository |TRUE      |
|pkgconfig     |pkgconfig     |2.0.3    |Repository |TRUE      |
|pkgdown       |pkgdown       |2.1.3    |Repository |TRUE      |
|pkgload       |pkgload       |1.4.0    |Repository |TRUE      |
|plyr          |plyr          |1.8.9    |Repository |TRUE      |
|png           |png           |0.1-8    |Repository |TRUE      |
|praise        |praise        |1.0.0    |Repository |TRUE      |
|prettyunits   |prettyunits   |1.2.0    |Repository |TRUE      |
|processx      |processx      |3.8.6    |Repository |TRUE      |
|profvis       |profvis       |0.4.0    |Repository |TRUE      |
|progress      |progress      |1.2.3    |Repository |TRUE      |
|promises      |promises      |1.3.2    |Repository |TRUE      |
|proxy         |proxy         |0.4-27   |Repository |TRUE      |
|ps            |ps            |1.9.1    |Repository |TRUE      |
|purrr         |purrr         |1.0.4    |Repository |TRUE      |
|R6            |R6            |2.6.1    |Repository |TRUE      |
|ragg          |ragg          |1.4.0    |Repository |TRUE      |
|rappdirs      |rappdirs      |0.3.3    |Repository |TRUE      |
|rcmdcheck     |rcmdcheck     |1.4.0    |Repository |TRUE      |
|RColorBrewer  |RColorBrewer  |1.1-3    |Repository |TRUE      |
|Rcpp          |Rcpp          |1.0.14   |Repository |TRUE      |
|rcrossref     |rcrossref     |1.2.0    |Repository |TRUE      |
|readr         |readr         |2.1.5    |Repository |TRUE      |
|readxl        |readxl        |1.4.3    |Repository |TRUE      |
|rematch       |rematch       |2.0.0    |Repository |TRUE      |
|rematch2      |rematch2      |2.1.2    |Repository |TRUE      |
|remotes       |remotes       |2.5.0    |Repository |TRUE      |
|renv          |renv          |1.1.1    |Repository |TRUE      |
|repr          |repr          |1.1.7    |Repository |TRUE      |
|reprex        |reprex        |2.1.1    |Repository |TRUE      |
|rgeolocate    |rgeolocate    |1.4.2    |Repository |TRUE      |
|rlang         |rlang         |1.1.6    |Repository |TRUE      |
|rmarkdown     |rmarkdown     |2.29     |Repository |TRUE      |
|roxygen2      |roxygen2      |7.3.2    |Repository |TRUE      |
|rprojroot     |rprojroot     |2.0.4    |Repository |TRUE      |
|rstudioapi    |rstudioapi    |0.17.1   |Repository |TRUE      |
|rversions     |rversions     |2.1.2    |Repository |TRUE      |
|rvest         |rvest         |1.0.4    |Repository |TRUE      |
|s2            |s2            |1.1.9    |Repository |TRUE      |
|sass          |sass          |0.4.9    |Repository |TRUE      |
|scales        |scales        |1.3.0    |Repository |TRUE      |
|selectr       |selectr       |0.4-2    |Repository |TRUE      |
|sessioninfo   |sessioninfo   |1.2.3    |Repository |TRUE      |
|sf            |sf            |1.0-21   |Repository |TRUE      |
|shiny         |shiny         |1.10.0   |Repository |TRUE      |
|skimr         |skimr         |2.1.5    |Repository |TRUE      |
|sourcetools   |sourcetools   |0.1.7-1  |Repository |TRUE      |
|stringi       |stringi       |1.8.4    |Repository |TRUE      |
|stringr       |stringr       |1.5.1    |Repository |TRUE      |
|svglite       |svglite       |2.1.3    |Repository |TRUE      |
|sys           |sys           |3.4.3    |Repository |TRUE      |
|systemfonts   |systemfonts   |1.2.1    |Repository |TRUE      |
|testthat      |testthat      |3.2.3    |Repository |TRUE      |
|textshaping   |textshaping   |1.0.1    |Repository |TRUE      |
|tibble        |tibble        |3.3.0    |Repository |TRUE      |
|tidylog       |tidylog       |1.1.0    |Repository |TRUE      |
|tidyr         |tidyr         |1.3.1    |Repository |TRUE      |
|tidyselect    |tidyselect    |1.2.1    |Repository |TRUE      |
|tidyverse     |tidyverse     |2.0.0    |Repository |TRUE      |
|timechange    |timechange    |0.3.0    |Repository |TRUE      |
|tinytex       |tinytex       |0.56     |Repository |TRUE      |
|triebeard     |triebeard     |0.4.1    |Repository |TRUE      |
|tzdb          |tzdb          |0.5.0    |Repository |TRUE      |
|units         |units         |0.8-7    |Repository |TRUE      |
|urlchecker    |urlchecker    |1.0.1    |Repository |TRUE      |
|urltools      |urltools      |1.7.3    |Repository |TRUE      |
|usethis       |usethis       |3.1.0    |Repository |TRUE      |
|utf8          |utf8          |1.2.6    |Repository |TRUE      |
|uuid          |uuid          |1.2-1    |Repository |TRUE      |
|vctrs         |vctrs         |0.6.5    |Repository |TRUE      |
|viridis       |viridis       |0.6.5    |Repository |TRUE      |
|viridisLite   |viridisLite   |0.4.2    |Repository |TRUE      |
|vroom         |vroom         |1.6.5    |Repository |TRUE      |
|waldo         |waldo         |0.6.2    |Repository |TRUE      |
|whisker       |whisker       |0.4.1    |Repository |TRUE      |
|withr         |withr         |3.0.2    |Repository |TRUE      |
|wk            |wk            |0.9.4    |Repository |TRUE      |
|xfun          |xfun          |0.51     |Repository |TRUE      |
|xml2          |xml2          |1.3.7    |Repository |TRUE      |
|xopen         |xopen         |1.0.1    |Repository |TRUE      |
|xtable        |xtable        |1.8-4    |Repository |TRUE      |
|yaml          |yaml          |2.3.10   |Repository |TRUE      |
|zip           |zip           |2.3.3    |Repository |TRUE      |

