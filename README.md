# overnight fires: algorithms and analysis 
R Code in support of "Drought triggers and sustains overnight fires in North America". 

This code uses active fire detection data from geostationary satellite images and terrestrial fire records to characterize the hourly fire diurnal cycle of each large fire (≥1000 Ha) and identify overnight burning events (OBEs, i.e., fire activity was detected during every nighttime hour in a single day within a known fire perimeter) in North America during the 2017-2020 period. The distribution, characteristics, and implications of OBEs are further analyzed. A systematic examination of fire weather metrics is conducted to assess underlying drivers and explore the prediction of OBEs.

The scripts include the Data processing and algorithms and the analysis as shown in the workflow below. The source code for display items is also included. The datasets required for the code are listed in the data requirements and are all publicly available.

### Workflow
#### 1. Data processing and algorithms
- GOES active fire products download (Data processing and algorithms/01_Download.R), processing (Data processing and algorithms/02_Processing.R), and projecting (Data processing and algorithms/03_Projecting.R)
- Hourly fire diurnal cycle identification and corresponding hourly fire weather extraction and calculation (Data processing and algorithms/04_hrly fire cycle_hrly fire weather.R)
  - Fire records and biome preprocessing 
  - Lifetime determination for each fire event
  - GOES active fire detections extraction and summarization
  - Biome extraction
  - Hourly fire weather time series extraction and calculation
  - Time zone conversion
  - Exact sunset and Sunrise time calculation and daytime or nighttime hour assignment
- Daily fire weather extraction and calculation (Data processing and algorithms/05_daily fire weather extraction.R)
- OBEs identification and fire weather feature calculation (Data processing and algorithms/06_OBEs identification and feature calculation.R)
  - OBEs and non-OBEs identification
  - Daytime and nighttime extremes of hourly fire weather and their range calculation
#### 2. Analysis
- Spatiotemporal distribution and statistics of OBEs (including Figure 1; Analysis/01_distribution and statistics.R)
  - Fire with and without OBEs, total OBEs, OBEs by seasons and by fire classes 
- Extreme characteristics of OBEs (including Figure 2; Analysis/02_extreme characteristics.R)
  - Number of days between ignition and the occurrence of the first OBE
  - Number of days between two adjacent OBEs in multi-OBE fires 
  - Fire size comparison and correlation with the number of OBEs
- Fire weather comparison (including Figure 3a, 3b and Supplementary Figure S2, S3; Analysis/03_fire weather comparison.R)
- Assessment of and increasing trend in fire weather extremes of OBEs (including Figure 3c and Supplementary Figure S4; Analysis/04_trend and extremes.R)
- Importance analysis of fire weather variables (including Figure 4; Analysis/05_importance ML.R)
- Prediction of OBEs (including Figure 5; Analysis/06_prediction LR.R)
#### 3. Visualization
- code for cases study and Supplementary Figure S5 and S6 (case_study_figs5_s6.R)

### Data requirements
- GOES active fire images (nc) from Amazon Web Service S3 Explorer 
- Fire perimeters (shapefile) from NBAC and MTBS
- Daily and hourly fire weather (nc)
- Biome categorization (shapefile) 
- Processed yearly GOES active fire summarization (csv)
- Processed hourly fire diurnal cycle and hourly and daily fire weather combination (csv)
- Processed OBEs and non-OBEs and fire weather feature (csv)

### Software and required packages requirements
- R version 4.2.0
- rgdal 1.5-32
- raster 3.5-15
- sp 1.5-0
- dplyr 1.0.9
- tidyr 1.2.0
- reshape2 1.4.4
- lutz 0.3.1
- suncalc 0.5.0
- gtools 3.9.2.1
- foreach 1.5.2
- doParallel 1.0.17
- parallel 4.2.0
- tcltk 4.2.0
- doSNOW 1.0.20
- ggplot2 3.3.6
- ggsci 2.9
- caret 6.0-92
- MKinfer 0.6
- pROC 1.18.0
