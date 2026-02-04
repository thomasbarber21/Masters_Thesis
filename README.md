# Master_Thesis
Title: The New York City Congestion Charge: Air Quality, Traffic, and Crime

Research Question: Is the New York City congestion charge working, and what impact 
does it have on crime in the city?

Motivation: We all know the main reason NYC enacted a congestion 
charge: to reduce traffic and pollution from commuters. Could there actually be other 
implications as well? There is motivation that a congestion charge could influence crime 
rates in the city, and even potentially help NYPD catch criminals. This concept is very 
new, as the charge was recently enacted on January 3, 2025 and thus research into this 
area is original and important.

Description of Datasets Used:

1. The following link is to the public EZ Pass toll dataset. This source is 
updated until 2026 and will help display any changes in speeds traveled
before and after enacting a congestion charge.
https://data.cityofnewyork.us/Transportation/EZ-Pass-Readers-July-2024-current/6a2s-2t65/about_data

2. This data from the NYC Open Portal is an Air Quality dataset, containing information on 
PM 2.5 levels and other necessary information before and after the policy was implemented.
https://data.cityofnewyork.us/Environment/Air-Quality/c3uy-2p5r/about_data

3. Using data provided by the NYPD on the NYC Open Data portal, we can look at both historic 
and year to date data on arrests. This dataset is updated through 2026.
https://data.cityofnewyork.us/browse?q=NYPD+crime+data&sortBy=relevance&pageSize=20&page=1 

4. Subway Stop Locations: https://data.ny.gov/Transportation/MTA-Subway-Entrances-and-Exits-2024/i9wp-a4ja/about_data 

5. Bus Stop Locations: https://data.ny.gov/Transportation/Capital-District-Bus-Stops/wgnh-hpq9/about_data 

Strategies / Methods: After organizing and cleaning the data a generalized synthetic control is implemented 
on air quality, travel speeds, and each crime type. Placebo tests are also run for each synthetic control. 
Within the crime data parallel trends are tested, and from this a difference-in-difference model is used. 
Subway station and bus stop locations are then mapped to arrest data and regressions are run to explore their 
relationship. Finally, PDS Lasso is utilized on the higher dimensional data to account for any missing controls 
in the hand picked regressions and to come to final conclusions.

Future Projections: While the results of this thesis did not yield any real substantial results, it was necessary 
work. Little to no research had been done previously on the potential relationship between congestion charges and 
crime, and at the beginning of this process little research had been done into the NYC congestion charge. Further 
research can be done on the impact of the policy on businesses within the CBD, foot traffic in the area, and the 
demand on other modes of transportation. Research on the impact on local residents and the effects of increasing 
the cost of living is also important work that must be done in order to fully understand the implications of such 
a policy.

Steps to Reproduce Current Work:
1. Create folders named code, data, and output within an umbrella folder named Thesis. Open the code folder
2. Create and Open the "Thesis.Rproj" within the code folder
3. Once within the project, add each of the R scripts from this Github. Open the R script file "master.R"
4. Run all code in the master.R script file
5. Navigate back to the Thesis folder and open the output folder. The output 
plots are stored here.
6. The regression output tables are printed into the console, simply run the outputs.R 
file for the Latex ready output.
