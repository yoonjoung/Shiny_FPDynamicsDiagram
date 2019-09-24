library(shiny)
library(dplyr)
library(tidyverse)
library(networkD3)
library(RCurl)

# Monthly database update: 
# 1. Knit "FPDynamicsDiagram_MultipleCountries.rmd" in "C:\Users\YoonJoung Choi\Dropbox\0 Project\FPDynamicsDiagram"
# 2. upload "DHSAPI_discontinuation.csv" to gitup

date<-as.Date(Sys.time(	), format='%d%b%Y')

dta <- read.csv("https://raw.githubusercontent.com/yoonjoung/FPDynamicsDiagram_Shiny/master/DHSAPI_discontinuation.csv")
table(dta$country)  
dta<-arrange(dta, country, order)

countrylist<-unique(as.vector(dta$country))

#setwd("C:/Users/YoonJoung Choi/Dropbox/0 Project/FPDynamicsDiagram_Shiny")

#******************************
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    # Header panel 
    headerPanel("Interactive Visualization of Contraceptive Dynamics"),
    
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
            h4("Throughout women's life, their use of contraceptive methods keeps changing - i.e.,",strong("dynamic.")),
            
            h4("The interactive diagram shows",strong("contraceptive use continuation, switching, and discontinuation."), "It is based on 'twelve-month discontinuation rates' from Demographic and Health Surveys conducted in 60 countries."),

            hr(),
            h4("Data source (latest DHS survey in the selected country)"),
            verbatimTextOutput("Source"),# Output: latest survey in the selected country
            h5("The total number of contraceptive use episodes analyzed (among women age 15-49 who started an episode of contraceptive use within the 5 years before the survey):"),
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
            h5("For more information on the calendar data in DHS, discontinuation tabulation, and the diagram, please see", a("here.", href="http://rpubs.com/YJ_Choi/FPDynamicsData")),
            h5("For questions, bugs, or typos, please", a("contact me.", href="https://www.isquared.global/")),
            
            hr(),
            h6("Footnote on the figure:"),
            h6("1. Right side looks longer only because of more number of categories, and total height is same between source and target."),   
            h6("2. Caution: the flow/chord thickness is nearly impossible to differentiate the volume when it is low. For small volume flow, refer to the table,",strong("'Twelve-month contraceptive discontinuation rates',"),"in the survey's final report."),
            
            hr(),
            h6("Footnote on the data:"),        
            h6("1. Source: Estimated", strong("discontinuation rates"), "from",a("Demographic and Health Surveys API.", href="http://api.dhsprogram.com/#/index.html"),"API data may have more detailed categories of contraceptive methods than the table in the final report. Last updated on: September 21, 2019"),  
            h6("2. The data are at the episode level, and, thus, the distribution on the left side is NOT the 'method mix' distribution."),
            h6("3. In the figures, discontinuation refers to 'not using any methods' and thus excludes switching episodes."),   
            h6("4. Discontinuation is split into three groups: (1) discontinuation while 'not in need'; (2) discontinuation while 'in need'; and (3) discontinuation due to method failure. 'Not in need' includes two reasons: desire to become pregnant, and other fertility related reasons (e.g., infrequent sex/husband away, difficult to get pregnant/menopausal, and marital dissolution/separation). In addition, it is assumed, for this exercise, that all who discontinued because they wanted more effective methods indeed switched to another method."), 
            h6("5. Sum of percentage across three discontinuation types may not be 100, due to rounding"),
            h6("6. Switching to other methods was calculated, with a simple assumption that new switched method is selected randomly. Please see", a("here.", href="http://rpubs.com/YJ_Choi/FPDynamicsData"),"about the background and potential implications. For those interested, actual distribution of switching across methods can be calculated using", a("the women-level calendar data.",href="https://www.dhsprogram.com/data/calendar-tutorial/"))
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
                   Injectables,Pill,Condom,FemaleCondom,LAM,EC,OtherModern, 
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