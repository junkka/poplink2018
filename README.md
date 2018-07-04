POPLINK Data
----------
  
Data preperation of POPLINK data for analysis of fertility in the Skellefteå region

# Data

The source files are not included in this package. They can be retreived from: Demografiska databasen, CEDAR, Umeå University. (2017). U16003 [Data set]. https://doi.org/10.17197/U16003

1. U16006
    * FIL1_MOR_PERSON.csv
    * FIL2_BARN.csv
    * FIL3_NARVARO.csv
    * FIL4_VIGSEL.csv
    * FIL5_PARTNER_PERSON.csv
    * U16006_export_ort_bef.csv
2. U16003
    * mor_person.csv
    * partner_person.csv
3. nyaortkoder.csv

# Reproduce

The process is reproduced by running

    make all
    
and then within R

    devtools::build()

