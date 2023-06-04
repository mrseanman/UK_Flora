# UK_Flora
Cleaning and compiling various work on trait correlations across species of UK flora.

Script for pipeline is in `src/`. The script `main.R` will scrape all necessary data from the ecoflora, save that data, determine fertilization mode for that data (written to a new column `myFert3`), then finally save that dataframe including `myFert3` to a different file.

Datasets can be found in `data/`. These include *old data*, including:

- Old raw data scraped from the ecoflora in June 2020.
- That old scrape data which was processed to determine myFert3 using an old Python script.

I am trying to reproduce the *scrape data and determine characteristics of that data* functionality of old Python scripts written in the summer of 2020.
I am not able to test this for the scrape aspect, since the website from which the data was scraped has changed in the intervening time. However I can test that the Python scripts to determine `myFert3` are reproduced. In `sketch_test/check_old_vs_new_data_and_methods.R` I test two things:

- The new R script for determining `myFert3` acts identically to the old Python script on the old scrape data.
- Similarly for the new scrape data.

These tests were passed, but in doing so, a couple of bugs and areas for obvious improvement were identified in the old Python code.
At the moment these have been reproduced in the R scripts.

The spread of `myFert3` has changed quite significantly:

| Fert Mode     | n (June 2020) | n (June 2023) |
| ------------- | ------------- |---------------|
| selfing       | 60            | 57            |
| mixed         | 787           | 643           |
| outcrossing   | 235           | 185           |
| NA            | 2192          | 2390          |



The important aspects of determining `myFert3` are in the file `src/determine_fertilization_characteristics.R`. The main function of this file is `assign_3_group_fertilization()`. In this file, characteristics of species are determined atomically in functions. These characteristics are combined to determine `myFert3` in `assign_3_group_fertilization()`.


