# Airports and Carriers Flight Performance 2018
##### Janice Cessna, Rachel Regberg, Jorid Topi


## Shiny App
This Shiny app is intended to allow the average flyer to make more informed decisions about which flights to take. The app visualizes the flight delays in 2018 to have a better understanding of how often a delay occured based on various variables, such as the given airport, route, airline, region, and airport operation size. 

The app consists of six tabs: Overall Delays, Airline Delays, Temporal Delays, Daily Regressions, Airport Delay Table, and Visualizations.

- The Overall Delays tab shows the first step in exploratory data analysis and the overall trend of flight delay distributions.

- The Airline Delays tab explores the airport performance related to carriers and whether certain carriers perform better on the ground than others. Or is the performance simply related to the airport?

- The Temporal Delays tab explores the airport performance at different times of the day and whether the pattern is consistent throughout the year. 

- The Delay Regressions tab explores the relationship between airport performance and regions. 

- The Airport Delay Table tab is highlighting the relationship based the top origins and selections in the Delay Regressions tab.

- The Visualization tab shows straightforward charts for yearly, monthly, and daily flights by airline. It also shows the percentage of flights cancelled and delayed. 


## Data Set:
The dataset derived from the Bureau of Transportation Statistics: Reporting Carrier On-Time Performance for the year 2018. (https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236). The Bureau of Transportation Statistics has Carrier On-Time Performance data from 1987 to the present. Due to the sheer volume of data we decided to analyze just the year 2018. We took a random sample of 10,000 flights from each month for a final dataset containing information on 120,000 flights. 

The Delay types from (https://www.bts.gov/topics/airlines-and-airports/airline-time-performance-and-causes-flight-delays):

- **Weather:** "Significant meteorological conditions (actual or forecasted) that, in the judgment of the carrier, delays or prevents the operation of a flight such as tornado, blizzard or hurricane."

- **Carrier:** "The cause of the cancellation or delay was due to circumstances within the airline's control (e.g. maintenance or crew problems, aircraft cleaning, baggage loading, fueling, etc.)."

- **National Aircraft Systems:** "Delays and cancellations attributable to the national aviation system that refer to a broad set of conditions, such as non-extreme weather conditions, airport operations, heavy traffic volume, and air traffic control."

- **Late Aircraft:** "A previous flight with same aircraft arrived late, causing the present flight to depart late."

- **Security:** "Delays or cancellations caused by evacuation of a terminal or concourse, re-boarding of aircraft because of security breach, inoperative screening equipment and/or long lines in excess of 29 minutes at screening areas."










