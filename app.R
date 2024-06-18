library(shiny)
library(dplyr)
library(tidyverse)
library(networkD3)
library(RCurl)

date<-as.Date(Sys.time(	), format='%d%b%Y')

# There are four parts in this document:
# 0. Database update: tidying DHS API data for the app
# 1. USER INTERFACE 
# 2. SERVER
# 3. CREATE APP 

#******************************
# 0. Database update 
#******************************

# First, knit "FPDynamicsDiagram_callAPI.rmd" in the folder which creates "dhsapi.csv" 
# Second, knit "FPDynamicsDiagram_callAPI_check.rmd" in the folder which checks data 

url<-"https://raw.githubusercontent.com/yoonjoung/Shiny_FPDynamicsDiagram/master/dhsapi.csv"
dtaapi<-read.csv(url)

# 1. rename var and tidy var names
dtaall<-dtaapi %>%
    rename ( xprg=	FP_DISR_W_PRG) %>%
    rename ( xdes=	FP_DISR_W_DES) %>%
    rename ( xfrt=	FP_DISR_W_FRT) %>%
    rename ( xsid=	FP_DISR_W_SID) %>%
    rename ( xwme=	FP_DISR_W_WME) %>%
    rename ( xmet=	FP_DISR_W_MET) %>%
    rename ( xoth=	FP_DISR_W_OTH) %>%
    rename ( xany=	FP_DISR_W_ANY) %>%
    rename ( xswh=	FP_DISR_W_SWH) %>%
    rename ( denom=	FP_DISR_W_NUM) %>%
    rename (country	=	CountryName) %>%
    rename (group	=	CharacteristicCategory) %>% 
    rename (grouplabel	=	CharacteristicLabel) 

colnames(dtaall)<-tolower(names(dtaall))

# 2. keep only estimates by contraceptive methods (drop the total row)
dtaall<-dtaall %>% filter(group=="Contraceptive method") 

# 3. create effectiveness order and sort by it
dtaall<-dtaall %>% mutate(
    order=0,
    order= ifelse(grouplabel == "Male sterilization",1, order),
    order= ifelse(grouplabel == "Female sterilization",2, order),
    order= ifelse(grouplabel == "IUD",3, order),
    order= ifelse(grouplabel == "Implants",4, order),
    order= ifelse(grouplabel == "Injectables",5, order),
    order= ifelse(grouplabel == "Pill",6, order),
    order= ifelse(grouplabel == "Condom",7, order),

    order= ifelse(grouplabel == "Female condom",8, order),
    order= ifelse(grouplabel == "Standard days method",9, order),
    order= ifelse(grouplabel == "Lactational amenorrhea",10, order),
    order= ifelse(grouplabel == "Emergency contraception",11, order),
    order= ifelse(grouplabel == "Other modern methods",12, order),
    
    order= ifelse(grouplabel == "Periodic abstinence",13, order),
    order= ifelse(grouplabel == "Withdrawal",14, order),
    order= ifelse(grouplabel == "Other traditional methods",15, order),
    
    zzmodern=0, 
    zzmodern=ifelse(order>=1 & order<=12,1, zzmodern),
    zztraditional=0,
    zztraditional=ifelse(order>=13 & order<=15,1, zztraditional))     

# 4. create type and survey year variables
dtaall<-dtaall %>% 
    mutate(
    	year=as.numeric(substr(surveyid,3,6)), 
    	type=substr(surveyid,7,9)) %>%
    filter(type=="DHS") 

# 5. identify and keep the latest survey year per country 
dta<-dtaall %>% 
    group_by(country) %>% 
    mutate(maxyear = max(year)) %>%
    filter(year==maxyear) 
    
# 6. GENERALIZE the dataframe so that the below diagram code works for all surveys
dta<-dta %>%

    # Keep sure only rows with non-missing reasons 
    filter(is.na(xany)==FALSE) %>%

    # count the number of methods per survey 
    group_by(surveyid) %>% 
    mutate(
        nummethods = n(),
        nummodern=sum(zzmodern),
        numtraditional=sum(zztraditional)) %>% 

    mutate (
        # Calculate number of episodes disoncinuted 
        Discontinuation=denom*(xany-xswh)/100,
        DiscontinuationNotInNeed=denom*(xdes+xfrt)/100,
        DiscontinuationFailure=denom*(xprg)/100,
        DiscontinuationInNeed=Discontinuation - DiscontinuationNotInNeed - DiscontinuationFailure,
        
        # Calculate number of episodes switched To each method 
        MaleSterilization =((denom*xswh/100) / (nummethods-1) ),
        FemaleSterilization =((denom*xswh/100) / (nummethods-1) ),
        IUD =((denom*xswh/100) / (nummethods-1) ),
        Implants =((denom*xswh/100) / (nummethods-1) ),
        Injectables =((denom*xswh/100) / (nummethods-1) ),
        Pill =((denom*xswh/100) / (nummethods-1) ),
        Condom =((denom*xswh/100) / (nummethods-1) ),

        FemaleCondom =((denom*xswh/100) / (nummethods-1) ),
        SDM =((denom*xswh/100) / (nummethods-1) ),
        LAM =((denom*xswh/100) / (nummethods-1) ),
        EC =((denom*xswh/100) / (nummethods-1) ),
        OtherModern =((denom*xswh/100) / (nummethods-1) ),
        
        Rhythm =((denom*xswh/100) / (nummethods-1) ),
        Withdrawal =((denom*xswh/100) / (nummethods-1) ),
        OtherTraditional =((denom*xswh/100) / (nummethods-1) ),         

        # Calculate number of episodes continued
        continue=denom*(100-xany)/100,
        MaleSterilization= ifelse(order==1,continue, MaleSterilization),
        FemaleSterilization= ifelse(order==2,continue, FemaleSterilization),
        IUD= ifelse(order==3, continue, IUD),
        Implants= ifelse(order==4, continue, Implants),
        Injectables= ifelse(order==5, continue, Injectables),
        Pill= ifelse(order==6, continue, Pill),
        Condom= ifelse(order==7, continue, Condom),

        FemaleCondom= ifelse(order==8, continue, FemaleCondom),
        SDM= ifelse(order==9, continue, SDM),
        LAM= ifelse(order==10, continue, LAM),
        EC= ifelse(order==11, continue, EC),
        OtherModern= ifelse(order==12, continue, OtherModern),
        
        Rhythm= ifelse(order==13, continue, Rhythm),
        Withdrawal= ifelse(order==14, continue, Withdrawal),
        OtherTraditional= ifelse(order==15, continue, OtherTraditional) ) %>%        
    
    group_by (surveyid) %>%   
    mutate (
        # check if the method is included by each survey 
        ifMaleSterilization= (1 %in% order),
        ifFemaleSterilization= (2 %in% order),
        ifIUD= (3 %in% order),
        ifImplants= (4 %in% order),
        ifInjectables= (5 %in% order),
        ifPill= (6 %in% order),
        ifCondom= (7 %in% order),

        ifFemaleCondom= (8 %in% order),
        ifSDM= (9 %in% order),
        ifLAM= (10 %in% order),
        ifEC= (11 %in% order),
        ifOtherModern= (12 %in% order),
        
        ifRhythm= (13 %in% order),
        ifWithdrawal= (14 %in% order),
        ifOtherTraditional= (15 %in% order)) %>%            
    
    mutate (    
        #recode to 0 for methods that do not exist in the survey 
        MaleSterilization= ifelse(ifMaleSterilization==FALSE,0, MaleSterilization),
        FemaleSterilization= ifelse(ifFemaleSterilization==FALSE,0, FemaleSterilization),
        IUD= ifelse(ifIUD==FALSE,0, IUD),
        Implants= ifelse(ifImplants==FALSE,0, Implants),
        Injectables= ifelse(ifInjectables==FALSE,0, Injectables),
        Pill= ifelse(ifPill==FALSE,0, Pill),
        Condom= ifelse(ifCondom==FALSE,0, Condom),

        FemaleCondom= ifelse(ifFemaleCondom==FALSE,0, FemaleCondom),
        SDM= ifelse(ifSDM==FALSE,0, SDM),
        LAM= ifelse(ifLAM==FALSE,0, LAM),
        EC= ifelse(ifEC==FALSE,0, EC),
        OtherModern= ifelse(ifOtherModern==FALSE,0, OtherModern),
        
        Rhythm= ifelse(ifRhythm==FALSE,0, Rhythm),
        Withdrawal= ifelse(ifWithdrawal==FALSE,0, Withdrawal),
        OtherTraditional= ifelse(ifOtherTraditional==FALSE,0, OtherTraditional),        

        # check if test==denom
        test=DiscontinuationNotInNeed+DiscontinuationInNeed+DiscontinuationFailure+MaleSterilization+FemaleSterilization+IUD+Implants+Injectables+Pill+Condom+FemaleCondom+SDM+LAM+EC+OtherModern+Rhythm+Withdrawal+OtherTraditional,
        confirm=test-denom) %>%
    ungroup()
    
    #mutate_if(is.numeric, round(., 1)) 
    
    #mutate_at(vars(starts_with("x")), 
    #          ~replace(., is.na(.)==TRUE, 0))
    
    #mutate_at(vars(starts_with("xequip_malfunction_reason_")), 
    #          funs(ifelse(xequip_anymalfunction!=1, NA, .)))%>%        
    

dta<-arrange(dta, country, order)
countrylist<-unique(as.vector(dta$country))
obs<-length(unique(dta$country))

#******************************
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    tags$head(includeScript("googleanalytics.js")),
    
    # Header panel 
    headerPanel("Interactive Visualization of Contraceptive Dynamics: Twelve-month discontinuation rates from Demographic and Health Surveys"),
    
    # Title panel 
    #titlePanel("Based on discontinuration rate data from DHS"),
    
    # Side panel: define input and output   
    sidebarLayout(
        
        # Side panel for inputs: only ONE in this case
        sidebarPanel(
            selectInput("country", 
                        "Select a country", 
                        choices = countrylist),
            h5(strong("Hover over each box on the left"), "and see how the method use continued over the period, switched to other methods, or discontinued."),
            h5(strong("Hover over each box on the right"), "and see where the flows come from.")

        ),
        
        # Main page for ourput display 
        mainPanel(
            h4("Throughout a woman's life, her use of contraceptive methods keeps changing - in other words,",strong("it is dynamic.")),
            
            h4("The interactive diagram below shows",
               strong("contraceptive continuation, switching, and discontinuation"), 
               "over time, based on 'twelve-month discontinuation rates' from",
               a("Demographic and Health Surveys", href="https://www.dhsprogram.com/"),
               "conducted in",
               obs,
               "countries."),

            hr(),
            h4("To get started",strong("select a country on the left.")),
            
            hr(),
            h5("Data source (latest DHS survey in the selected country):"),
            verbatimTextOutput("Source"),# Output: latest survey in the selected country
            hr(),
            h5("Total number of contraceptive use episodes (among women age 15-49 who started an episode of contraceptive use within the 5 years before the survey):"),
            verbatimTextOutput("sum"),

            hr(),
            h4(strong("Twelve-month contraceptive continuation, switching, and discontinuation")),            
            sankeyNetworkOutput("Sankey"),#output: Sankey diagram 
            
            h4(strong("Summary")),
            h5("Overall, most episodes continued for 12 months since beginning of the use."), 
            h5("Meanwhile, some episodes discontinued",strong("(yellow, orange, and red boxes)."),"Out of all use episodes, discontinuation happened in the following percentage:"),
            verbatimTextOutput("dis"),
            h5("Broadly there are three types of discontinuation:"),  
            h5("-- discontinuation because of no more need for family planning (e.g., wanting to get pregnant)",strong("(yellow)."),"Percent of discontinuation due to these reasons is:"),  
            verbatimTextOutput("disnotinneed"),
            h5("-- discontinuation while there still was need for contraception",strong("(orange)."),"Percent of discontinuation due to these reasons is:"),
            verbatimTextOutput("disinneed"),
            h5("-- discontinuation due to method failure",strong("(red)."),"Percent of discontinuation due to this reason is:"),
            verbatimTextOutput("disfailure"),
            h5("By individual method, there was higher discontinuation among short-acting methods than among long-acting methods."),  
    
            hr(),
            h4(strong("So what?")),
            h5("It is important to recognize discontinuation 'while in need' and discontinuation due to method failure. Such discontinuation can be reduced by addressing the reasons - including ensuring women's ability to switch to other methods effectively, when desired."),

            hr(),
            h5(strong("Footnote on the figure:")),
            h5("1. Right side looks longer only because of more number of categories, and total height is same between source and target."),   
            h5("2. Caution: the flow/chord thickness is nearly impossible to differentiate the volume when it is low. For small volume flow, refer to the table,",strong("'Twelve-month contraceptive discontinuation rates',"),"in the survey's final report."),
            
            hr(),
            h5(strong("Footnote on the data and methods:")),        
            h5("1. Source: Estimated", strong("discontinuation rates"), "from",a("Demographic and Health Surveys API.", href="http://api.dhsprogram.com/#/index.html"),"API data may have more detailed categories of contraceptive methods than the table in the final report."),  
            h5("2. The data are at the episode level, and, thus, the distribution on the left side is NOT the 'method mix' distribution."),
            h5("3. In the figures, discontinuation refers to 'not using any methods' and thus excludes switching episodes."),   
            h5("4. Discontinuation is split into three groups: (1) discontinuation while 'not in need'; (2) discontinuation while 'in need'; and (3) discontinuation due to method failure. 'Not in need' includes two reasons: desire to become pregnant, and other fertility related reasons (e.g., infrequent sex/husband away, difficult to get pregnant/menopausal, and marital dissolution/separation). In addition, it is assumed, for this exercise, that all who discontinued because they wanted more effective methods indeed switched to another method."), 
            h5("5. Sum of percentage across three discontinuation types may not be 100, due to rounding"),
            h5("6. Switching to other methods was calculated, with a simple assumption that new switched method is selected randomly.", 
               strong("Please see", a("this working paper.", href="http://rpubs.com/YJ_Choi/FPDynamicsData"),"about the background, detailed methods, and potential implications."),
               "For those interested, actual distribution of switching across methods can be calculated using", a("the women-level calendar data.",href="https://www.dhsprogram.com/data/calendar-tutorial/")), 
            
            hr(),
            h6("Application initially published on June 27, 2019."),
            h6("DHS API data last accessed and the application updated on: June 18, 2024."),

            hr(),
            h6(span("For more information on the calendar data in DHS, discontinuation tabulation, and the diagram, please", a("see here.", href="http://rpubs.com/YJ_Choi/FPDynamicsData"), style = "color:gray")),
            h6(span("For questions, bugs, or typos, please", a("contact me.", href="https://www.isquared.global/YJ"), style = "color:gray")),
            h6(span("For code, please see", a("GitHub.", href="https://github.com/yoonjoung/FPDynamicsDiagram_Shiny"), style = "color:gray")),
            h6(em(span("Making Data Delicious, One Byte at a Time", style = "color:gray")))
                                
        )    
    )    
)

#******************************
# 2. SERVER
#******************************

server<-function(input, output) {

    ##### REACTIVE setting: dataselected() 
    dataselected <- reactive({
        filter(dta, country==input$country)
    })    
    
    ##### Output: latest survey in the selected country 
    output$Source <- renderText({
        source<-dataselected()[1,]  
        paste(source$country, source$type, source$year, sep = " ")
    })
    
    ##### REACTIVE setting: matrix()
    matrix <- reactive({    
        # Select only relevant variables/columns 
        dataselected() %>%
        select(grouplabel, 
                   MaleSterilization,FemaleSterilization,IUD,Implants,
                   Injectables,Pill,Condom,FemaleCondom,SDM,LAM,EC,OtherModern, 
                   Rhythm,Withdrawal,OtherTraditional,
                   DiscontinuationFailure,DiscontinuationInNeed,
                   DiscontinuationNotInNeed) 
    })

    ##### Output: Sankey Diagram
    output$Sankey <- renderSankeyNetwork({

        matrix<-matrix()
        matrix<-matrix[, colSums(matrix != 0) > 0]
    
            colnames<-names(matrix)
            colnames<-colnames[-1]
            n<-length(colnames)
            colnames<-colnames[-n]
            colnames<-colnames[-(n-1)]
            colnames<-colnames[-(n-2)]
        rownames(matrix) <-c(colnames)
        
        matrix<-select(matrix, -grouplabel) 

        # Recode first column (usually, but not necessarily female sterilization) 0=0.0001: 
        for (i in 1:nrow(matrix)) {
            if (matrix[i,1]==0) {
                matrix[i,1] <-0.0001
            }
        }
    
        # Again, label rows and check against the grouplabel
        rownames(matrix) <-c(colnames)       
        
        # Reshape data to long format 
        data_long <- matrix %>%
            rownames_to_column %>%
            gather(key = 'key', value = 'value', -rowname) %>%
            filter(value > 0)
        colnames(data_long) <- c("source", "target", "value")
        data_long$target <- paste(data_long$target, " ", sep="")
        
        # From these flows we need to create a node data frame: it lists every entities involved in the flow
        nodes <- data.frame(name=c(as.character(data_long$source),
                                   as.character(data_long$target)) %>%
                                unique()
                            )
        
        # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
        data_long$IDsource=match(data_long$source, nodes$name)-1 
        data_long$IDtarget=match(data_long$target, nodes$name)-1
        
        # Prepare colour scale 
        # Set number of methods by type for color later
        #ColourScal ='d3.scaleOrdinal().range(["#08306B","#08519C","#2171B5"])'
        nummodern<-sum(dataselected()$zzmodern)
        numtraditional<-sum(dataselected()$zztraditional)        
        ColourScal ='d3.scaleOrdinal().range(colorarray)'
        
            if (nummodern==2 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==2 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==2 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==2 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }
        
            if (nummodern==3 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#DEEBF7","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==3 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#DEEBF7","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==3 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#DEEBF7","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==3 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#3182BD","#9ECAE1","#DEEBF7","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }
        
            if (nummodern==4 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#2171B5","#6BAED6","#BDD7E7","#EFF3FF","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==4 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#2171B5","#6BAED6","#BDD7E7","#EFF3FF","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==4 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#2171B5","#6BAED6","#BDD7E7","#EFF3FF","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==4 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#2171B5","#6BAED6","#BDD7E7","#EFF3FF","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }
        
            if (nummodern==5 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#BDD7E7","#EFF3FF","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==5 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#BDD7E7","#EFF3FF","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==5 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#BDD7E7","#EFF3FF","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==5 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#BDD7E7","#EFF3FF","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }
            
            if (nummodern==6 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==6 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==6 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==6 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }    
        
            if (nummodern==7 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==7 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==7 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==7 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }
        
            if (nummodern==8 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==8 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==8 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==8 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#084594","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }
            
            if (nummodern==9 & numtraditional==0) {
            ColourScal ='d3.scaleOrdinal().range(["#08306B","#08519C","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==9 & numtraditional==1) {
            ColourScal ='d3.scaleOrdinal().range(["#08306B","#08519C","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#238b45","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==9 & numtraditional==2) {
            ColourScal ='d3.scaleOrdinal().range(["#08306B","#08519C","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#238b45","#41ab5d","#b10026","#fc4e2a","#ffffb2"])'
            }
            if (nummodern==9 & numtraditional==3) {
            ColourScal ='d3.scaleOrdinal().range(["#08306B","#08519C","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF","#238b45","#41ab5d","#74c476","#b10026","#fc4e2a","#ffffb2"])'
            }            
        
        # Finally, make the Network
        sankeyNetwork(Links = data_long, Nodes = nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=FALSE, colourScale=ColourScal, 
                    nodeWidth=40, fontSize=13, 
                    fontFamily = "sans-serif", nodePadding=20,
                    iterations=0)
    })

    ##### REACTIVE setting: matrix2() more tidy
    matrix2 <- reactive({    

        matrix<-matrix()
        matrix<-matrix[, colSums(matrix != 0) > 0]
    
            colnames<-names(matrix)
            colnames<-colnames[-1]
            n<-length(colnames)
            colnames<-colnames[-n]
            colnames<-colnames[-(n-1)]
            colnames<-colnames[-(n-2)]
        rownames(matrix) <-c(colnames)
        
        matrix<-select(matrix, -grouplabel) 

        # Recode first column (usually, but not necessarily female sterilization) 0=0.0001: 
        for (i in 1:nrow(matrix)) {
            if (matrix[i,1]==0) {
                matrix[i,1] <-0.0001
            }
        }
    
        # Again, label rows and check against the grouplabel
        rownames(matrix) <-c(colnames)       
        
        matrix2<-matrix
    })
    
    ##### Output: summary statistics
    output$sum <- renderText({
        sum<-format(round(sum(colSums(matrix2())),0), scientific = FALSE, big.mark=",")        
    })
    
    ##### Output: summary statistics
    output$dis <- renderText({
        total<-unname(colSums(matrix2()))
        n<-length(names(matrix2()))
        sumdisfailure<-total[(n-2)]
        sumdisinneed<-total[(n-1)]
        sumdisnotinneed<-total[n]
        sumdis<-sumdisfailure+sumdisinneed+sumdisnotinneed
        
        sum<-sum(colSums(matrix2()))

        dis<-round(100*sumdis/sum, 0)        
    })

    ##### Output: summary statistics
    output$disnotinneed <- renderText({
        total<-unname(colSums(matrix2()))
        n<-length(names(matrix2()))
        sumdisfailure<-total[(n-2)]
        sumdisinneed<-total[(n-1)]
        sumdisnotinneed<-total[n]
        sumdis<-sumdisfailure+sumdisinneed+sumdisnotinneed
        
        disnotinneed<-round(100*sumdisnotinneed/sumdis, 0) 
    })

    ##### Output: summary statistics
    output$disinneed <- renderText({
        total<-unname(colSums(matrix2()))
        n<-length(names(matrix2()))
        sumdisfailure<-total[(n-2)]
        sumdisinneed<-total[(n-1)]
        sumdisnotinneed<-total[n]
        sumdis<-sumdisfailure+sumdisinneed+sumdisnotinneed
        
        disinneed<-round(100*sumdisinneed/sumdis, 0)
    })

    ##### Output: summary statistics
    output$disfailure <- renderText({
        total<-unname(colSums(matrix2()))
        n<-length(names(matrix2()))
        sumdisfailure<-total[(n-2)]
        sumdisinneed<-total[(n-1)]
        sumdisnotinneed<-total[n]
        sumdis<-sumdisfailure+sumdisinneed+sumdisnotinneed
        
        disfailure<-round(100*sumdisfailure/sumdis, 0)
    })
}       

#******************************
# 3. CREATE APP 
#******************************

shinyApp(ui = ui, server = server)