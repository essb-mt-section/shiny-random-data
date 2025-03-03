library(shiny)
library(shinyjs)
library(haven)
library(withr)
library(stringr)

source("random_data_functions.R")

get_data <- function(dataset_name, seed) {
    switch(dataset_name,
           "Teaching Method" = with_seed(seed, teaching_data()),
           "Priming" = with_seed(seed, simulate_priming_data()),
           "Aggression" = with_seed(seed, sample_from_sav(
                          "data/Aggression_population.sav", nsamples=125))
    )
}

eur_student_id <- function(student_id){
    # returns student number and student letters, if it is a valid EUR student ID.
    # Otherwise `list(number=NA, letters=NA)` is returned

    rtn <- list(number=NA, letters=NA)
    id_pattern <- regex("^[0-9]{6}[a-zA-Z]{2}$") # EUR student ID: six numbers followed by two letters
    student_id <- str_trim(student_id)
    if (str_detect(student_id, id_pattern)) {
        rtn$number=as.numeric(str_sub(student_id, end=6))
        rtn$letters=str_sub(student_id, start = 7)
    }
    return(rtn)
}

data_filename <- function(dataset_name, student_id ) {
    paste0(str_remove(dataset_name, " "), "_", student_id, ".sav")
}

function(input, output) {

    output$validStudentId  <- renderUI({
        id = eur_student_id(input$studentid)
        # activate download button
        goodId <- !is.na(id$number)
        goodData <- input$dataset != "-- select --"
        if (goodId & goodData) { shinyjs::enable("downloadData")
        } else { shinyjs::disable("downloadData") }
        # return subject ID text
        rtn = "&nbsp;"
        if ( (str_length(input$studentid) > 5) & is.na(id$number)) {
            rtn = paste0("<div style='color:red'>",
                         input$studentid, " is not a valid student ID.</div>")
        }
        return(HTML(paste0("<p>", rtn, "</p>")))
    })

    # downloadHandler() takes two arguments, both functions.
    output$downloadData <- downloadHandler(
        filename = function() {data_filename(input$dataset, input$studentid)},
        content = function(file) {
                    id = eur_student_id(input$studentid)
                    write_sav(get_data(input$dataset, seed=id$number), file )
        }
    )
}



