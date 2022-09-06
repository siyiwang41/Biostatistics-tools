# Biostatistics-tools
Some useful functions for biostatistics analyses. Current tools are for COVID-19 analysis about mobility and social determinants of health (SDOH).
In R file, since mobility and COVID-19 data are updated from time to time, there are some easy adapted codes for further relative analysis.
1. QuintileTertile contains functions for people quickly quintile/tertile census tracts (CT) by SDOH, adjusted by CT populations or ranking within public health unit (PHU).
2. AggregatedMobility helps aggregate small area-level mobility (e.g. CT level) to larger area-level (e.g. PHU level or province level) or aggregate mobility by SDOH. 
3. AggregatedCOVID_byCT aggregates linelist COVID-19 comfirmed cases data by CT. We have daily or weekly versions.
4. SummaryTable contains a function to quickly generate a summary table by quintile/tertile/decile variables.
