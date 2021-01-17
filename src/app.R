library(shiny)
library(shinydashboard)
library(formattable)
library(dplyr)
library(highcharter)


ui <- dashboardPage(
  dashboardHeader(title = "Student Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Grade-1", tabName = "grade1", icon = icon("university"))
    #menuItem("Grade-2", tabName = "grade2", icon = icon("university")),
    #menuItem("Grade-3", tabName = "grade3", icon = icon("university")),
    #menuItem("Grade-4", tabName = "grade4", icon = icon("university")),
    #menuItem("Grade-5", tabName = "grade5", icon = icon("university"))
  )),
  
  dashboardBody(
    tags$style(HTML(".sidebar-menu li a { font-size: 17px; }")),
    tags$head(tags$style(
      HTML(
        '.info-box {min-height: 137px;} .info-box-icon {height: 137px; line-height: 137px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;font-size: 100}'
      )
    )),
    
    # box(title = "Student Dashboard", status = "primary", solidHeader = T, width = 3,
    #     fluidPage(
    #       fluidRow(
    #         column(12, offset = 0, style = 'padding:1px;',
    #                selectInput(inputId = "selected_data",
    #                            label = "Assignment Number",
    #                            choices = c("1","2","3","4","5","6","7","8","9","10")))
    #       ),
    #
    #       fluidRow(
    #         column(12, offset = 0, style = 'padding:1px;',
    #                selectInput(inputId = "selected_data_1",
    #                            label = "Question Number",
    #                            choices = c("1","2","3","4","5","6","7","8","9","10")))
    #       ),
    #
    #       fluidRow(
    #         column(12, offset = 0, style = 'padding:1px;',
    #                selectInput(inputId = "selected_data_1",
    #                            label = "Student Name",
    #                            choices = c("1","2","3","4","5","6","7","8","9","10")))
    #       )
    #
    #     )
    # ),
    
    tabItems(
      # First tab content
      tabItem(
        tabName = "grade1",
        
        fluidRow(
          tabBox(
            width = 12,
            title = "Assignments",
            id = "tabset1",
            height = "350px",
            tabPanel("Assignment-1", value = 1, formattableOutput("g1_a")),
            tabPanel("Assignment-2", value = 1, formattableOutput("g1_a1")),
            tabPanel("Assignment-3", value = 1, formattableOutput("g1_a2")),
            tabPanel("Assignment-4", value = 1, formattableOutput("g1_a3")),
            tabPanel("Assignment-5", value = 1, formattableOutput("g1_a4")),
            tabPanel("Assignment-6", value = 1, formattableOutput("g1_a5")),
            tabPanel("Assignment-7", value = 1, formattableOutput("g1_a6")),
            tabPanel("Assignment-8", value = 1, formattableOutput("g1_a7")),
            tabPanel("Assignment-9", value = 1, formattableOutput("g1_a8")),
            tabPanel("Assignment-10", value = 1, formattableOutput("g1_a9"))
          )
          
        ),
        
        br(),
        br(),
        br(),
        br(),
        
        fluidRow(
          box(
            title = "Student Dashboard",
            status = "primary",
            solidHeader = T,
            width = 12,
            fluidPage(fluidRow(
              column(
                3,
                offset = 0,
                style = 'padding-left:5px;padding-right:40px;',
                selectInput(
                  "selected_data",
                  label = "Assignment Number",
                  choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
                )
              ),
              
              column(
                3,
                offset = 0,
                style = 'padding-right:40px;  ',
                selectInput(
                  inputId = "Select2",
                  label = "Question Number",
                  choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                  selected = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                  multiple = TRUE
                )
              ),
              column(
                3,
                offset = 0,
                style = 'padding-right:5px;',
                selectInput(
                  inputId = "Select3",
                  label = "Student Name",
                  choices = dataset$`Student Name`,
                  selected = dataset$`Student Name`,
                  multiple = TRUE
                )
              )
            ))
          )
          
        ),
        
        
        fluidRow(column(4, box(
          highchartOutput("plot1"), width = 12
        )),
        column(4, box(
          highchartOutput("plot2"), width = 12
        )),
        column(4, box(
          highchartOutput("plot3"), width = 12
        )))
        
      ),
      
      tabItem(tabName = "grade2")
      
    )
  )
)


server <- function(input, output) {
  #1
  output$g1_a <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset$res10 > 0, "green", "red")))
    
    formattable(
      dataset,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #2
  output$g1_a1 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset1$res10 > 0, "green", "red")))
    
    formattable(
      dataset1,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #3
  output$g1_a2 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset2$res10 > 0, "green", "red")))
    
    formattable(
      dataset2,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #4
  output$g1_a3 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset3$res10 > 0, "green", "red")))
    
    formattable(
      dataset3,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #5
  output$g1_a4 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset4$res10 > 0, "green", "red")))
    
    formattable(
      dataset4,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #6
  output$g1_a5 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset5$res10 > 0, "green", "red")))
    
    formattable(
      dataset5,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #7
  output$g1_a6 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset6$res10 > 0, "green", "red")))
    
    formattable(
      dataset6,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #8
  output$g1_a7 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset7$res10 > 0, "green", "red")))
    
    formattable(
      dataset7,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #9
  output$g1_a8 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset8$res10 > 0, "green", "red")))
    
    formattable(
      dataset8,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  
  #10
  output$g1_a9 <- renderFormattable({
    form1 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res1 > 0, "green", "red")))
    form2 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res2 > 0, "green", "red")))
    form3 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res3 > 0, "green", "red")))
    form4 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res4 > 0, "green", "red")))
    form5 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res5 > 0, "green", "red")))
    form6 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res6 > 0, "green", "red")))
    form7 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res7 > 0, "green", "red")))
    form8 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res8 > 0, "green", "red")))
    form9 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res9 > 0, "green", "red")))
    form10 <-
      formatter("span", style = ~ style(color = ifelse(dataset9$res10 > 0, "green", "red")))
    
    formattable(
      dataset9,
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      list(
        'Question-1' = form1,
        'Question-2' = form2,
        'Question-3' = form3,
        'Question-4' = form4,
        'Question-5' = form5,
        'Question-6' = form6,
        'Question-7' = form7,
        'Question-8' = form8,
        'Question-9' = form9,
        'Question-10' = form10
        
        
      )
    ) %>% select(
      `Student Name`,
      `Question-1`,
      `Question-2`,
      `Question-3`,
      `Question-4`,
      `Question-5`,
      `Question-6`,
      `Question-7`,
      `Question-8`,
      `Question-9`,
      `Question-10`
    )
  })
  
  # df<-reactive({
  # if(input$selected_data==1)
  # df<-dataset
  # else if (input$selected_data==2)
  # df<-dataset
  # else if (input$selected_data==3)
  #   df<-data_g1_a3
  # else if (input$selected_data==4)
  #   df<-data_g1_a4
  # else if (input$selected_data==5)
  #   df<-data_g1_a5
  # else if (input$selected_data==6)
  #   df<-data_g1_a6
  # else if (input$selected_data==7)
  #   df<-data_g1_a7
  # else if (input$selected_data==8)
  #   df<-data_g1_a8
  # else if (input$selected_data==9)
  #   df<-data_g1_a9
  # else
  #   df<-data_g1_a10
  # })
  
  output$plot1 <- renderHighchart({
    df <-
      subset(
        data_g1,
        Student %in% input$Select3 &
          Question %in% input$Select2 & asm %in% input$selected_data
      )
    req(df$Question)
    
    
    agg <-
      setNames(aggregate(df$Status ~ df$Question, df, sum),
               c("question", "values"))
    
    glimpse(agg)
    
    
    hchart(agg, "column", hcaes(x = question, y = values)) %>% hc_xAxis(title = list(text = "Question Number")) %>% hc_yAxis(title = list(text = "Fluency")) %>% hc_tooltip(
      formatter = JS(
        "function(){
        return '</b> Number of Fluent Answers: '+ this.y
  }"
),
useHTML = FALSE
      ) %>% hc_add_theme(hc_theme_gridlight())
    
  })
  
  
  output$plot2 <- renderHighchart({
    df <-
      subset(
        data_g1,
        Student %in% input$Select3 &
          Question %in% input$Select2 &
          asm %in% input$selected_data & Status %in% 1
      )
    
    # agg<-setNames(aggregate(df$Status ~ df$Student,df,sum), c("student","perform"))
    #
    # glimpse(agg)
    
    # glimpse(df)
    req(df$Student)
    
    hcpie(df$Student, y = df$Student) %>% hc_tooltip(
      formatter = JS(
        "function(){
        return '</b> Number of Fluent Answers: '+ this.y
  }"
),
useHTML = FALSE
      ) %>% hc_add_theme(hc_theme_flat())
    
  })
  
  
  output$plot3 <- renderHighchart({
    agg <-
      setNames(aggregate(data_g1$Status ~ data_g1$asm, data_g1, sum),
               c("asm", "values"))
    
    
    
    hchart(agg, "scatter", hcaes(x = asm, y = values)) %>% hc_xAxis(title = list(text = "Assignment Number")) %>% hc_yAxis(title = list(text = "Fluency")) %>% hc_tooltip(
      formatter = JS(
        "function(){
        return '</b> Number of Fluent Answers: '+ this.y
  }"
),
useHTML = FALSE
      ) %>% hc_add_theme(hc_theme_gridlight())
    
  })
  
  
}

shinyApp(ui, server)