# Malefix_Biodiversity

## _Extreme_ Research Program and _MaLeFix_ Project
The WSL research program "Extremes" (2021-2024) is an interdisciplinary and transdisciplinary that explores the impacts of rare, extreme weather events on both environment and society. Due to climate change and globalization, such occurrences are predicted to become increasingly common. To address this challenge, this program seeks to unite researchers and practitioners from a range of sectors and disciplines to provide Swiss stakeholders with effective decision-making tools and coping strategies.

The Extreme Program includes five sub-projects: ALANex, EMERGE, ExtremeThaw, MaLeFix, and MountEx. My work contributed to the MaLeFix project, which focuses on **Ma**chine **Le**arning aided **F**orecast**i**ng drought-related e**x**tremes. MaLeFix bridges gaps between different research units by combining diverse impact models with sub-seasonal weather and hydrological forecasts, aims to develop a user-friendly decision tool that enables stakeholders to anticipate drought-related extremes weeks in advance, giving them time to prevent or mitigate severe environmental and socio-economic impacts.

My work centered on the biodiversity aspect of the project. Using physiological modeling, I investigated the impact of extreme temperatures and water availability on species fitness in the Swiss fauna under climate change scenarios.

## This repository contains code and data needed to reproduce the results in the report. 

You can find the short descrption of each folder in the following table

| Folder | Content |
| ------------- | ------------- |
| BAFU  | Contains all the **daily maximum and daily mean water temperature data** in water monitoring stations in Switzerland from **2014-01-01 to 2021-12-31**. All the data is requested from BAFU. Depending on data availability, we also have measured water temperature data for the first few months of 2022 in some stations. The R script file is an example of reading the daily mean temperature for each station from the excel data provided by BAFU.  |
| Climate Data  | Contains all the **meteorological data** (a total of 9 variables are included: precipitation, radiation, relative humidity, sunshine duration, mean, maximum & minimum temperature, vapor pressure, and wind speed) that has been downloaded from the server of Dr. Zappa’s group. Since it is too big to uplaod, it is stored on the hard drive.  |
| Swiss species  | Contains all the **species' thermal tolerance information**. They are stored in three subfolders. You can find clean datasets in the folder [Tables], **original papers** that have been used to extract data are stored in [Reference], and **raw data and code for extracting them** are stored in [Data]   |
| CTi   | All the **code, results and plots** for **Critical Temperature index (CTi)** calculation.  |
| water temperature   | All the data is needed to calculate the **water temperature in rivers Doubs and Rhone**. Including the **raster files** of water temperature downloaded from Google Earth Engine, the **shapefile** of water monitoring stations, the correlation between measured water temperature and land surface temperature /brightness temperature calculated by remote sensing.    |

