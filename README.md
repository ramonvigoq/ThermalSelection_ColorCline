# ThermalSelection_ColorCline
Raw data files and analysis codes from the study 'Thermal selection is not a major contributor to the maintenance of a shell color cline in a marine snail', by Ramón Vigo, Sarah L. Y. Lau, Juan Gefaell, Juan Galindo, Gray A. Williams, Manuela Truebano and Emilio Rolán-Alvarez, submitted in _Marine Biology_

## Description of the data and file structure
We conducted various experiments to study the role of thermal selection in the _Littorina saxatilis_ shell color cline described in the Rias Baixas by comparing its two dominant color morphs 
Our design includes (1) in situ rock temperature monitoring to identify a thermal gradient along the cline and comparative experiments for (2) thermal performance and tolerance, (3) body heating rates, and (4) shell heating and recovery rates under more extreme temperatures

## Temperature data along the cline
### Files: TS_CC_RData_RockSurfaceTemperature_North.xlsx and TS_CC_RData_RockSurfaceTemperature_South.xlsx
These files are divided into 6 sheets, corresponding with the raw data for each location from the North and South coast, respectively, where in situ temperature loggers were installed

#### Variables description
| Variable                    | Description                                                                                                                            |
| --------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| Coast                       | Coast of the Ria de Vigo                                                                                                               |
| LocName                     | Name of the location (Gefaell et al. 2024)                                                                                             |
| Latitude                    | Latitude of the location in DD (Gefaell et al. 2024)                                                                                   |
| Longitude                   | Longitude of the location in DD (Gefaell et al. 2024)                                                                                  |
| NumGefaellEtAl              | Code number assigned in Gefaell et al. (2024)                                                                                          |
| Distance                    | Distance from the river mouth in m                                                                                                     |
| LoggerNum                   | Logger number                                                                                                                          |
| LoggerLatitude              | Logger latitude, provided by the logger itself in DD                                                                                   |
| LoggerLongitude             | Logger longitude, provided by the logger itself in DD                                                                                  |
| DateInstallation_Loggers    | Date on which the logger was installed in yyyy-mm-dd                                                                                   |
| TimeInstallation_Loggers    | Time at which the logger was installed in HH:MM:SS, provided by the logger itself                                                      |
| WeatherInstallation_Loggers | Weather at the locality during the installation of the logger; 1 = Sunny, 2 = Sun and cloud, 3 = Cloudy, 4 = Soft rain, 5 = Heavy rain |
| July2023                    | Whether the logger includes 5-min interval recordings during July 2023; 0 = false, 1 = true                                            |
| DateSwitching_Loggers       | When July2023 = 1, date on which the logger was reconfigured to 30-minute intervals, in yyyy-mm-dd                                     |
| TimeSwitching_Loggers       | When July2023 = 1, time at which the logger was reconfigured to 30-minute intervals, in HH:mm:ss                                       |
| DateCollection_Loggers      | Date on which logger data was collected in yyyy-mm-dd                                                                                  |
| TimeCollection_Loggers      | Time at which logger data was collected in HH:MM:SS                                                                                    |
| WeatherCollection_Loggers   | Weather at the locality during the collection of  logger data; 1 = Sunny, 2 = Sun and cloud, 3 = Cloudy, 4 = Soft rain, 5 = Heavy rain |
| time                        | Time of the temperature recording                                                                                                      |
| temp                        | Recorded temperature in °C                                                                                                             |
| EcolPC1                     | PC1 value retrieved from Gefaell et al. (2024)                                                                                         |
| Ecosystem_Type              | Wave exposure category (Exposed, Intermediate, Sheltered) of the location, based on the division of EcolPc1 values into tertiles       |

### File: TS_CC_SeaLevel_PORTUS.xlsx
This file corresponds with hourly sea level data during our temperature recordings, directly retrieved from Puertos del Estado tide gauge for Vigo (portus.puertos.es)

#### Variables description
| Variable  | Description                                      |
| --------- | ------------------------------------------------ |
| time_GMT  | Time of the sea level recording in yyyy/mm/dd/HH |
| sea_level | Recorded sea level                               |

All these three datasets were treated and analysed in R, following  *Scripts_DataAnalysis_TS_CC_RockSurfaceTemperature.R*
## Thermal tolerance experiment (Vigo)

### File: TS_CC_RData_ThermalTolerance.xlsx
This file is divided into 2 sheets, corresponding with the raw data and the codes of the variables for the thermal tolerance and performance experiment in the Ria de Vigo:
- 'DATA': Raw data of the experiment
- 'CODES': Codes of the variables

#### Variables description
| Variable | Description                                                   |
| -------- | ------------------------------------------------------------- |
| ID       | Snail ID                                                      |
| position | Spatial position of the vial within the water bath            |
| temp     | Experimental temperature set in the water bath                |
| col      | Shell color morph determined by eye                           |
| treat    | Exposure duration                                             |
| size     | Shell length in mm                                            |
| sex      | Sex of the snail, based on the presence or absence of a penis |
| fattach  | Whether the snail remained attached after the exposure        |
| survival | Whether the snail was still alive after the exposure          |

##### Qualitative variables levels
| Levels | temp |       col |   treat | sex | fattach | survival |
| -----: | ---: | --------: | ------: | --: | ------: | -------: |
|      0 |      |           |         |     |   false |    false |
|      1 |   41 |   *fulva* | Chronic |  35 |    true |     true |
|      2 |   42 | *lineata* |   Acute |  40 |         |          |
|      3 |   43 |           |         |  45 |         |          |
|      4 |   44 |           |         |  50 |         |          |
|      5 |   45 |           |         |     |         |          |
|      6 |   46 |           |         |     |         |          |
|      7 |   47 |           |         |     |         |          |

This dataset was treated and analysed in R, following *Scripts_DataAnalysis_TS_CC_ThermalTolerance.R*

## Shell heating experiment (Arousa and Pontevedra)
### File: TS_CC_RData_ThermalTolerance.xlsx
This file is divided into 2 sheets, corresponding with the raw data and the codes of the variables for the shell heating and recovery experiment in the Ria de Arousa and the Ria de Pontevedra:
- 'DATA': Raw data of the experiment
- 'CODES': Codes of the variables

#### Variables description
| Variable         | Description                                                                              |
| :--------------- | :--------------------------------------------------------------------------------------- |
| sampling_date    | Date on which the snail was collected in mm/dd/yyyy                                      |
| location_name    | Name of the location                                                                     |
| coordinates      | Coordinates of the location in DD                                                        |
| measure_date     | Date on which the snail was tested in mm/dd/yyyy                                         |
| acclimation_time | Time lapse prior to the experimental measurements in hours                               |
| pop              | Population type                                                                          |
| ria              | Ria from which the snail was collected                                                   |
| col              | Shell color morph determined by eye                                                      |
| temp             | Experimental temperature set in the heat lamp                                            |
| rep              | Snail replica                                                                            |
| tech             | Technical measurement                                                                    |
| recovery         | Whether the snail recovered in a lapse of 30 min after its shell temperature measurement |
| shell_temp       | Maximum shell temperature measured in ºC                                                 |
| snail            | Whether the sample was a living snail or an empty shell                                  |

##### Qualitative variables levels
| Levels |        pop |        ria |       col | temp | recovery | snail |
| -----: | ---------: | ---------: | --------: | ---: | -------: | ----: |
|      0 |            |            |           |      |    false | false |
|      1 | allopatric |     Arousa |   *fulva* |   35 |     true |  true |
|      2 |  sympatric | Pontevedra | *lineata* |   40 |          |       |
|      3 |            |            |           |   45 |          |       |
|      4 |            |            |           |   50 |          |       |

This dataset was treated and analysed in R, following *Scripts_DataAnalysis_TS_CC_ShellHeating.R*
