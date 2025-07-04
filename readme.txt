This is the replication package for the paper "The Protective Role of Parental Work Intensity for Children in Poverty” (García-Sierra, 2025, Demographic Research)
Software
All analyses were conducted using R Studio (R version 4.4.1).
Data
The files children.dat, mothers.dat, childrentwo.dat, and education.dat can be used to retrieve the necessary data from the NLSY-CYA Investigator platform. To access the data:
1 Register and log in at https://www.nlsinfo.org/investigator/pages/home.
2 Use the “Upload Tagset” option to upload the .dat file of interest.
3 The corresponding raw variables will then appear, along with the option to download the raw data.
Code
• 1_MASTER_NLS.R – Prepares the relevant data (previously downloaded from NLS), prepares and cleans variables, and defines the analytical sample.
• 2_DESCRIPTIVES.R – Produces Figure 1, Figure A1, as well as Table A1.
• 3_ANALYSES.R – Runs the main models and generates Table 1 and Figure 2.
• 4_SENSITIVITY.R – Produces all the Figures and Tables in the Supplementary Materials. 
