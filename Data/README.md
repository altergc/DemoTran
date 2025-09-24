Data files

-- NSO_list.xlsx  -- List of NSOs to search for data

- Combine_UN_IDB_Palg.xlsx  -- This is the input data file with data from the UN, US Census IDB, and the Palgrave "International Historical Statistics"

- compare_sources.accdb -- Access database used to select which series to analyze; selections extracted to Excel

- 00C1_Export_all_sorted.xlsx -- CBR and CDR data sorted by country and year; exported from compare_sources.accdb

- 00C2_country_region.xlsx -- region and country codes; exported from compare_sources.accdb


- dfOut_2.rdata -- This is a file of summary statistics for each country created by the runSegs.R program

- dfPred.rdata -- This file contains annual data on CBR, CDR, and RNI plus predicted values for CBR and CDR from the segmented regressions

