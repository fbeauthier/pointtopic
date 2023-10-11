## pointtopic
Repo for all the code produced during internship at Point-Topic.

There are 3 main parts:
- `BII` (Broadband Infrastructure Index)
- `BDDI` (Broadband Digital Deprivation Index) - The BII is a component of the BDDI.
- `upc_prems` - Separate study on population, households and premises data in the UPC database.


### BII
This folder contains all the queries and code to create (and eventually update) the new BII (starting in 2023) at LSOA level.

1. The `BII_query_all_in_one.rtf` file contains the full query to obtain all the input variables from Point-Topic's Snowflake tables in **July 2023** to construct the BII.
2. The `build_BII.R` file contains the code to construct the BII in R for each nation using the output .csv file from the query
3. Run the last 4 "write_csv" lines to obtain the four separate BII files for each nation.

### BDDI
This folder contains all the code to create (and evenutally update) the new BDDI (starting in 2023) for LSOAs.

1. The `build_BDDI.R` file contains the code to construct the BDDI in R for each nation
2. The `translate_geos_fn.ipynb` file contains the code in python to translate data with old boundaries to new 2021 boundaries, where necessary.

### UPC Demographics
tbc
