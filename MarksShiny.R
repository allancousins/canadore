# import libraries
library(shiny)
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(wesanderson)
library(ggpubr)
library(urca)
library(tseries)
library(flextable)
library(jtools)
library(officer)
library(stringr)
library(readr)
library(tidyverse)
library(shinyWidgets)

scores <- c("F","D","D+","C","C+","B-","B","B+","A-","A","A+")
l_scores <- c(0,50,55,60,65,70,73,77,80,85,90)
u_scores <- c(49,54,59,64,69,72,76,79,84,89,100)

marks.df <- as.data.frame(cbind(scores,l_scores,u_scores))

marks.df[,2] <- as.numeric(marks.df[,2])
marks.df[,3] <- as.numeric(marks.df[,3])

cpm.df <- as.data.frame(matrix(rep(0,21600),nrow=4320))
colnames(cpm.df) <- c("Course","num","Grade","Semester","Instructor")

sem.vector <- c("Fall(2020)","Winter(2021)","Spring(2021)","Fall(2021)","Winter(2022)","Spring(2022)","Fall(2022)","Winter(2023)","Spring(2023)")
rep.sem.vector <- c(rep(sem.vector[1],40),
                    rep(sem.vector[2],40),
                    rep(sem.vector[3],40),
                    rep(sem.vector[4],40),
                    rep(sem.vector[5],40),
                    rep(sem.vector[6],40),
                    rep(sem.vector[7],40),
                    rep(sem.vector[8],40),
                    rep(sem.vector[9],40))


CPM100 <- rep(rnorm(360,mean=75,sd=10),1)
CPM105 <- rep(rnorm(360,mean=74,sd=10),1)
CPM110 <- rep(rnorm(360,mean=73,sd=9),1)
CPM115 <- rep(rnorm(360,mean=72,sd=8),1)
CPM118 <- rep(rnorm(360,mean=70,sd=11),1)
CPM125 <- rep(rnorm(360,mean=80,sd=7),1)

CMM140 <- rep(rnorm(360,mean=75,sd=7),1)
CPM200 <- rep(rnorm(360,mean=74,sd=11),1)
CPM205 <- rep(rnorm(360,mean=73,sd=8),1)
CPM215 <- rep(rnorm(360,mean=72,sd=9),1)
CPM220 <- rep(rnorm(360,mean=70,sd=10),1)
CPM225 <- rep(rnorm(360,mean=80,sd=10),1)


cpm.df$Course[1:360] <- rep("CPM100",360)
cpm.df$num[1:360] <- CPM100
cpm.df$Semester[1:360] <- rep.sem.vector
cpm.df$Instructor[1:360] <- rep("Rob",360)

cpm.df$Course[361:720] <- rep("CPM105",360)
cpm.df$num[361:720] <- CPM105
cpm.df$Semester[361:720] <- rep.sem.vector
cpm.df$Instructor[361:720] <- rep("Oday",360)

cpm.df$Course[721:1080] <- rep("CPM110",360)
cpm.df$num[721:1080] <- CPM110
cpm.df$Semester[721:1080] <- rep.sem.vector
cpm.df$Instructor[721:1080] <- rep("Rob",360)

cpm.df$Course[1081:1440] <- rep("CPM115",360)
cpm.df$num[1081:1440] <- CPM115
cpm.df$Semester[1081:1440] <- rep.sem.vector
cpm.df$Instructor[1081:1440] <- rep("Allan",360)

cpm.df$Course[1441:1800] <- rep("CPM118",360)
cpm.df$num[1441:1800] <- CPM118
cpm.df$Semester[1441:1800] <- rep.sem.vector
cpm.df$Instructor[1441:1800] <- rep("Guy",360)

cpm.df$Course[1801:2160] <- rep("CPM125",360)
cpm.df$num[1801:2160] <- CPM125
cpm.df$Semester[1801:2160] <- rep.sem.vector
cpm.df$Instructor[1801:2160] <- rep("Guy",360)


cpm.df$Course[2161:2520] <- rep("CMM140",360)
cpm.df$num[2161:2520] <- CMM140
cpm.df$Semester[2161:2520] <- rep.sem.vector
cpm.df$Instructor[2161:2520] <- rep("Jennifer",360)

cpm.df$Course[2521:2880] <- rep("CPM200",360)
cpm.df$num[2521:2880] <- CPM200
cpm.df$Semester[2521:2880] <- rep.sem.vector
cpm.df$Instructor[2521:2880] <- rep("Allan",360)

cpm.df$Course[2881:3240] <- rep("CPM205",360)
cpm.df$num[2881:3240] <- CPM205
cpm.df$Semester[2881:3240] <- rep.sem.vector
cpm.df$Instructor[2881:3240] <- rep("Guy",360)

cpm.df$Course[3241:3600] <- rep("CPM215",360)
cpm.df$num[3241:3600] <- CPM215
cpm.df$Semester[3241:3600] <- rep.sem.vector
cpm.df$Instructor[3241:3600] <- rep("Oday",360)

cpm.df$Course[3601:3960] <- rep("CPM220",360)
cpm.df$num[3601:3960] <- CPM220
cpm.df$Semester[3601:3960] <- rep.sem.vector
cpm.df$Instructor[3601:3960] <- rep("Rob",360)

cpm.df$Course[3961:4320] <- rep("CPM225",360)
cpm.df$num[3961:4320] <- CPM225
cpm.df$Semester[3961:4320] <- rep.sem.vector
cpm.df$Instructor[3961:4320] <- rep("Allan",360)

cpm.df$num[which(cpm.df$num>100)] <- 100



# Slider Input

current_terms <- c("Fall (2020)", "Winter (2021)", "Spring (2021)","Fall (2021)", "Winter (2022)", "Spring (2022)","Fall (2022)", "Winter (2023)", "Spring (2023)")
##########
# SERVER #
##########

#generic line initiating the SERVER 

server <- shinyServer(function(input, output) {

  #########################
  # Data load and cleanup #
  #########################

  #Import data
  
  #Clean data
  
  #############
  # Reactives #
  #############
  
  # define any reactive elements of the app
  
#Close the server definition
})

##################
# User Interface #
##################

#generic line initiating the UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel("Graphical Grade Distribution Tool"),
  
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(
      HTML("Please select which conditions you would like to see reflected in the graph and table to the right. Currently only the CPM program is displayed but 
           the tool can easily be ammended to display data by program (e.g. CPM, CET, Mechanical, etc). The data is simulated from normal distributions for proof of concept only.<br><br>"),
      
      sliderTextInput(
        inputId = "terms",
        label = "Academic Terms to Display",
        choices = current_terms,
        selected =c("Fall (2020)","Fall (2022)"),
        grid = TRUE,
        dragRange = TRUE),
      
      selectInput("display",
                  "Display By",
                  choices = c("Course" = 1,
                              "Instructor" = 2),
                  selected = 1)
    ),
    
    #beginning of main section
    mainPanel(plotOutput("ridge",height="80vh"),
              textOutput("desc"))
  )

#Close the UI definition
))

##SERVER-----
server <- function(input, output,session) {

  
  output$ridge <- renderPlot({
    
    year_start <- which(input$terms[1]==current_terms)
    year_end <- which(input$terms[2]==current_terms)
    
    cpm.df.slider <- cpm.df %>%
      filter(Semester==unique(cpm.df$Semester)[year_start:year_end])
    
    ggplot(data=cpm.df, aes(x = num, y = Course, fill = Course))+
      geom_density_ridges() +
      labs(x=" ",y=" ")+
      ggtitle("What is the distribution of numeric grades at Canadore?",
              subtitle = "Ridgline plot for current Construction Project Management courses")+
      theme(plot.title = element_text(face="bold",size=20))+
      theme(plot.subtitle = element_text(face="bold", color="grey",size=15)) +
      theme(legend.position = "none",axis.title.x = element_text(hjust=0.5),
            axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15))+
      scale_fill_hue(l=55, c=35)+
      scale_x_continuous(limits=c(40, 100),breaks=seq(40,100,5))
    
  })
  
  output$desc <- renderText({
    paste(current_terms)
    #paste(input$terms[2])
    
  })
}


##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)