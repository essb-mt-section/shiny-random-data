library(shiny)
library(shinyjs)

shinyUI(fluidPage(
    useShinyjs(),

    HTML('<table width=100%><tr><td valign="top">
           <H1>SPSS Datasets</H1>
           </td><td align="right">
           <img src="./Logo-EUR-black.png" width="200" alt="Erasmus University">
           </td></tr></table>'),


    verticalLayout(
        mainPanel(
            HTML("<p>To download your data file, choose the dataset and enter your student number.</p>"),

            selectInput("dataset", "Dataset:",
                        choices = c("-- select --",
                                    "Teaching Method",
                                    "Priming"
                                    )),
            textInput(inputId = "studentid",
                      label = "Student ID:",
                      value = NA),
            uiOutput("validStudentId"),
            disabled(downloadButton('downloadData', 'Download'))
        )
    )
))
