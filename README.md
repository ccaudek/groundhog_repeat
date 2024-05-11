# Groundhog Project

The R workflow scripts can be run with `make`. The master Makefile includes several specific Makefile, each having a different goal.

1. **Import the raw data**: `makefiles/Makefile_read_data` creates the file `data/prep/prl/groundhog_raw.csv`, which includes the complete unprocessed raw data.

2. **Data cleaning**: `makefiles/Makefile_clean_data` creates the file `data/prep/prl/groundhog_clean.csv`, with the basic data wrangling on the raw data.


