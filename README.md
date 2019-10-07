# FPDynamicsDiagram_Shiny

This app creates interactive diagrams on contraceptive continuation, switching, and discontinuation over time, based on 'twelve-month discontinuation rates' from Demographic and Health Surveys conducted in 60 countries. The app is available at: https://yj-choi.shinyapps.io/FPDynamicsDiagram_Shiny/

There are two files:

1. dhsapi.csv	: DHS API indicator data for the app. It includes ten discontinuation. Data access is separated from the main app code, since it contains DHS API user code - until I figure out how to deal with such cases. An example code without my user information is available in Appendix A of this paper: http://rpubs.com/YJ_Choi/PAA2020. 

2. app.R:	
There are four section:
- 0. Database update: tidying data, dhsapi.csv, for the app
- 1. USER INTERFACE 
- 2. SERVER
- 3. CREATE APP 
