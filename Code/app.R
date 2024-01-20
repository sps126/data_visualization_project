#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
df <- read.csv("NIJ_s_Recidivism_Challenge_Full_Dataset.csv")
new_df <- na.omit(df)
percent <- "%"

# Define UI 
ui <- fluidPage(theme = shinytheme("flatly"),
    # Application title
    titlePanel(
        h1("Recidivism in Georgia", align = "center")),
    
    p("Criminal justice reform is a serious issue in the United States. Two million people are currently 
      incarcerated with about 50 thousand serving life sentences. That means that the majority of people 
      incarceraeted will return to society. With that being said, the United States recidivism rate, 
      the rate of people who return to prision after release, is over 50%. For a system that claims 
      rehabilitation as one of it's goals, it is failing. We must examine factors that disuade people
      from returning to prison to understand where to allocate resources for reform. This data is from
      people released from prison in Georgia from 2013-2015. One important thing to note is that the data 
      only contains individuals that identify as black or white. On top of this, since the data is from Georgia
      the proportion of black individuals is skewed due to Atlanta being a majority black city."),
    tabsetPanel(type = "tabs",
    tabPanel("Descriptive Statistics", 
    fluidRow(column(
        radioButtons("explore", "Demographics",
                     c("Gender", "Race")
        ),
        p("Here you can explore the demographic breakdown of the dataset to get a better understanding of 
          incarceration in Georgia."),
        width = 4),
        column(plotOutput("Plot3"), width = 8))),
    tabPanel("Demographic Outcomes",
    fluidRow(
       column(radioButtons("demographics", "Demographic", 
                     c("Gender", "Race")), 
        p("Here you can explore different demographic characteristics and how they impact recidivism. Since
          race is about even, the variables that impact recidivism will focus on gender instead."), 
        width = 3),
      column(
        plotOutput("Plot1"), width = 9)
    )),
    
    tabPanel("Positive Variables",
    fluidRow(
      column(
        plotOutput("Plot2"), width = 9),
      column(selectInput('positiveCharacters', 'Characteristics', 
                         c("Jobs", 'Education Level')),
             radioButtons("posGender", "Gender:", c("Male", "Female", "Both"), 
                                selected = c("Both")),
             p("Jobs and education level are two variables that have been proven to disuade people from 
               returning to prison. You can explore this yourself."),
             width = 3)
      ))),
    p("After exploring the dashboard you can see that both education level and percent days employed are 
      integral in rehabilitation efforts for criminal justice reform. Recidivism is a pressing issue and if we 
      allocate resources to prioritize job training and education in prisons, we will see a decrease in 
      recidivism. For education, this could be in the form of high school and college classes while 
      incarcerated and continuing education after release. For percent days employed, we could focus on job 
      training while incarcerated and assisting with the job search after incarceration."),
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot1 <- renderPlot({
        # generate tables
      if (input$demographics == "Race") {
        x_1 = new_df$Race
        x_name = "Race"
        title_1 = "Race and the Effect on Recidivism in Georgia from 2013-2015"
        subtitle_1 = "About even in liklihood to return to prison by race"
        cat_1 <- c("Black", "White")} 
 
      else if (input$demographics == "Gender")
      {x_1 = new_df$Gender
      title_1 = "Gender and the Effect on Recidivism in Georgia from 2013-2015"
      subtitle_1 = "Men are more likely to return to prison" 
      x_name = "Gender"
      cat_1 <- c("Female", "Male")}
      
      ggplot(new_df, aes(x = x_1, fill = Recidivism_Within_3years)) +
        geom_bar(position = "fill") + 
        scale_fill_brewer(palette = "Set1", labels = c("False", "True")) +
        labs(fill = "Recidivism", x= x_name, y = 'Proportion', 
             title = title_1, 
             subtitle = subtitle_1,
             caption = "Data: National Institute of Justice") +
        geom_text(aes(label = paste(round(after_stat(count / tapply(count, x, sum)[as.character(x)]), 
                                          digits = 2) * 100, percent)),
                  stat = "count", position = position_fill(.5)) + scale_x_discrete(labels = cat_1) +
        theme_minimal() + theme(plot.title = element_text(face="bold"))
      
    })
    
    output$Plot2 <- renderPlot({
      #generate tables 
      if (input$positiveCharacters == "Jobs" & input$posGender == "Both") {
        pos_df = new_df %>% mutate(bin = cut(Percent_Days_Employed, breaks = 4))
        ggplot(pos_df, aes(x = bin, fill = Recidivism_Within_3years)) +
          geom_bar(position = "fill") + 
          scale_fill_brewer(palette = "Set1", labels = c("False", "True")) +
          labs(fill = "Recidivism", x= 'Percent Days Employed', y = 'Proportion', 
               title = "Percent Days of the Year Employed and the Effect on Recidivism in Georgia from 2013-2015", 
               subtitle = "Significantly less likely to return to prison when emplyed 50% of the year",
               caption = "Data: National Institute of Justice") +
          scale_x_discrete(labels= c("0-25%", "25-50%", "50-75%", "75-100%")) + 
          geom_text(aes(label = paste(round(after_stat(count / tapply(count, x, sum)[as.character(x)]), 
                                            digits = 2) * 100, percent)),
                    stat = "count", position = position_fill(.5)) + 
          theme_minimal() + theme(plot.title = element_text(face="bold"))} 
      else if (input$positiveCharacters == "Jobs" & input$posGender == "Male") {
        pos_df = new_df %>% filter(Gender == "M") %>% mutate(bin = cut(Percent_Days_Employed, breaks = 4))
        ggplot(pos_df, aes(x = bin, fill = Recidivism_Within_3years)) +
          geom_bar(position = "fill") + 
          scale_fill_brewer(palette = "Set1", labels = c("False", "True")) +
          labs(fill = "Recidivism", x= 'Percent Days Employed', y = 'Proportion', 
               title = "Percent Days of the Year Employed and the Effect on Recidivism in Georgia from 2013-2015", 
               subtitle = "Significantly less likely to return to prison when emplyed 50% of the year",
               caption = "Data: National Institute of Justice") +
          scale_x_discrete(labels= c("0-25%", "25-50%", "50-75%", "75-100%")) + 
          geom_text(aes(label = paste(round(after_stat(count / tapply(count, x, sum)[as.character(x)]), 
                                            digits = 2) * 100, percent)),
                    stat = "count", position = position_fill(.5)) + 
          theme_minimal() + theme(plot.title = element_text(face="bold"))}
      else if (input$positiveCharacters == "Jobs" & input$posGender == "Female") {
        pos_df = new_df %>% filter(Gender == "F") %>%mutate(bin = cut(Percent_Days_Employed, breaks = 4))
        ggplot(pos_df, aes(x = bin, fill = Recidivism_Within_3years)) +
          geom_bar(position = "fill") + 
          scale_fill_brewer(palette = "Set1", labels = c("False", "True")) +
          labs(fill = "Recidivism", x= 'Percent Days Employed', y = 'Proportion', 
               title = "Percent Days of the Year Employed and the Effect on Recidivism in Georgia from 2013-2015", 
               subtitle = "Significantly less likely to return to prison when emplyed 50% of the year",
               caption = "Data: National Institute of Justice") +
          scale_x_discrete(labels= c("0-25%", "25-50%", "50-75%", "75-100%")) + 
          geom_text(aes(label = paste(round(after_stat(count / tapply(count, x, sum)[as.character(x)]), 
                                            digits = 2) * 100, percent)),
                    stat = "count", position = position_fill(.5)) + 
          theme_minimal() + theme(plot.title = element_text(face="bold"))}
      
      else if(input$positiveCharacters == "Education Level" & input$posGender == "Both") {
        education <- c("Less than HS diploma", "High School Diploma", "At least some college")
        ggplot(new_df, aes(x = factor(Education_Level, education), fill = Recidivism_Within_3years)) +
          geom_bar(position = "fill") + 
          scale_fill_brewer(palette = "Set1", labels = c("False", "True")) +
          labs(fill = "Recidivism", x= 'Education level', y = 'Proportion', 
               title = "Education and the Effects on Recidivism in Georgia from 2013-2015", 
               subtitle = "Individuals with college degrees are less likely to go back to prison",
               caption = "Data: National Institue of Justice") + 
          geom_text(aes(label = paste(round(after_stat(count / tapply(count, x, sum)[as.character(x)]), 
                                            digits = 2) * 100, percent)),
                    stat = "count", position = position_fill(.5)) + 
          theme_minimal() + theme(plot.title = element_text(face="bold"))}
      else if (input$positiveCharacters == "Education Level" & input$posGender == "Male") {
        gender_filter <- new_df %>% filter(Gender == "M")
        education <- c("Less than HS diploma", "High School Diploma", "At least some college")
        ggplot(gender_filter, aes(x = factor(Education_Level, education), fill = Recidivism_Within_3years)) +
          geom_bar(position = "fill") + 
          scale_fill_brewer(palette = "Set1", labels = c("False", "True")) +
          labs(fill = "Recidivism", x= 'Education level', y = 'Proportion', 
               title = "Education and the Effects on Recidivism in Georgia from 2013-2015", 
               subtitle = "Individuals with college degrees are less likely to go back to prison",
               caption = "Dara: National Institue of Justice") + 
          geom_text(aes(label = paste(round(after_stat(count / tapply(count, x, sum)[as.character(x)]), 
                                            digits = 2) * 100, percent)),
                    stat = "count", position = position_fill(.5)) + 
          theme_minimal() + theme(plot.title = element_text(face="bold"))}
      else if (input$positiveCharacters == "Education Level" & input$posGender == "Female") {
        gender_filter <- new_df %>% filter(Gender == "F")
        education <- c("Less than HS diploma", "High School Diploma", "At least some college")
        ggplot(gender_filter, aes(x = factor(Education_Level, education), fill = Recidivism_Within_3years)) +
          geom_bar(position = "fill") + 
          scale_fill_brewer(palette = "Set1", labels = c("False", "True")) +
          labs(fill = "Recidivism", x= 'Education level', y = 'Proportion', 
               title = "Education and the Effects on Recidivism in Georgia from 2013-2015", 
               subtitle = "Individuals with college degrees are less likely to go back to prison",
               caption = "Data: National Institute of Justice") + 
          geom_text(aes(label = paste(round(after_stat(count / tapply(count, x, sum)[as.character(x)]), 
                                            digits = 2) * 100, percent)),
                    stat = "count", position = position_fill(.5)) + 
          theme_minimal() + theme(plot.title = element_text(face="bold"))}
      
    })
    
    output$Plot3 <- renderPlot({
      if (input$explore == "Gender") {
        values <- list(table(new_df$Gender))
        cat <- c("Female", "Male")
        df_3 <- data.frame(cat,values)
        title_3 = "Gender of Individuals Released from Prison in Georgia from 2013-2015"
        fill_3 = "Gender"}
      else {
        values <- list(table(new_df$Race))
        cat <- c("Black", "White")
        df_3 <- data.frame(cat,values)
        title_3 = "Race of Individuals Released from Prison in Georgia from 2013-2015"
        fill_3 = "Race"}
      
      ggplot(df_3, aes(x="", y=Freq, fill = cat)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() + 
        labs(title= title_3, fill = fill_3, caption = "Data: National Institute of Justice") +
        scale_fill_brewer(palette="Set1") +
        geom_text(aes(label = paste(round(Freq/ sum(Freq), digits = 2) * 100, percent)),
                  position = position_stack(vjust = 0.5)) + 
        theme(plot.title = element_text(face="bold"))
      
    })
    
    output$Plot4 <- renderPlot({
      if (input$demographics == "Race") {
        race = table(df$Race, df$Recidivism_Within_3years)
        demo_df = as.data.frame.matrix(race)
        demo_df$cat = c("Black", "White")
        demo_df$proportion = round(demo_df$true / sum(demo_df$true), digits = 2) * 100} 
      
      else {
        gender = table(df$Gender, df$Recidivism_Within_3years)
        demo_df = as.data.frame.matrix(gender)
        demo_df$proportion = round(demo_df$true / sum(demo_df$true), digits = 2) * 100
        demo_df$cat = c("Female", "Male")}
      
      ggplot(demo_df, aes(x="", y=proportion, fill=cat)) +
        geom_bar(stat="identity", width=1, color="white") + 
        labs(fill = 'Category') +
        coord_polar("y", start=0) +
        theme_minimal() + 
        theme(legend.position="bottom") + 
        labs(title= "Percentage that Reoffend in Georgia from 2013-2015", 
             caption = "Data: National Institute of Justice") +
        geom_text(aes(label = paste(proportion, percent)), 
                  position = position_stack(vjust = 0.5), color = "white", size=6) +
        scale_fill_brewer(palette="Set1") + 
        theme(plot.title = element_text(face="bold"))
      
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
