# super-duper-journey
Using Economic Factors to Predict Gubernatorial Election Results

This project consists of 7 different files: 6 of which contain data and one of which is an R script that cleans the data, runs analysis, and outputs tables and figures.  Below, I will describe what each file does.

6 Data Files:

SAGDP1__ALL_AREAS_1997_2019 2.csv -> Contains GDP Data

gov_elec_1999.csv -> Contains election data from 1997 to 1999

gov_elec_2009.csv -> Contains election data from 2000 to 2009

gov_elections_2019.csv -> Contains election data from 2010 to 2019

la.data.3.AllStatesS.txt -> Contains state unemployment rates

la.state_region_division.txt -> Contains a matching of states to numbers

1 Cleaning File:

This file cleans and combines all of the data above.  It creates a figure the number of elections per year.  Next, it creates two linear regressions.  One uses GDP data, and one does not.  It outputs that data as a latex file that can be copy/pasted into a latex document and viewed.



