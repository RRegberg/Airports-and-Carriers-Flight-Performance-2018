# Current Status:  
Initial EDA complete. Meeting on Saturday, Nov 23 to:  
1. Review EDA results for general relationships.  
2. Discuss what output suggests about initial analysis questions (below).  
3. Refine/add/delete analysis questions as appropriate.  
4. Decide if/what additional EDA is needed.   
5. Decide what statistical tests needed to answer analysis questions.   
6. Decide how best to summarize in Shiny app.  


# Initial Analysis Questions:
1. How does the airport performance look at different times of the day? Is this pattern consistent throughout the year? How does it look throughout the year?
2. Is airport performance related to the carriers? Do certain carriers perform better on the ground than others? Or is it simply related to the airport?
3. Is airport performance related to the airport state in any way. I.e. is there a correlation to region?
4. Are there any airports that perform better in departures than in arrivals?
5. How does performance relate to the airport traffic and which airports perform the best at high traffic?


# Initial Shiny concept: 
Allow users to impact different variables or combinations of variables that can influence departure delay (e.g. day of week, hour of day, origin airport) to see their probability of experiencing an actual delay. 


# Bonus tasks: 
If all goes super smoothly, may tackle additional items including:  
1. Adding in cargo and international traffic by airport to compare performance against total air traffic volume.  
2. Evaluate how delays impact loss of revenue via https://www.transtats.bts.gov/TableInfo.asp.   
3. Merge airport construction dates to evaluate if age influences performance.  
4. Merge airport city's per capita GDP to evaluate possible relationship with performance.  
