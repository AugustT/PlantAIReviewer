#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# # Add a random number column to sample images from
# ids <- read.csv('data/id_results_peaks_cleaned.csv', 
#                 stringsAsFactors = FALSE, 
#                 header = TRUE)
# ids$rand_id <- sample(1:nrow(ids),
#                       size = nrow(ids),
#                       replace = FALSE)
# write.csv(ids,
#           file = 'data/id_results_peaks_cleaned.csv',
#           row.names = FALSE)

ids <- read.csv('data/id_results_peaks_cleaned.csv', 
                stringsAsFactors = FALSE, 
                header = TRUE)
ids <- dplyr::distinct(ids)
results <- 'results/review_results.csv'

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Review of PlantNet classification of Flickr images"),

    sidebarLayout(
        sidebarPanel(
            textOutput('imgID'),
            checkboxInput(inputId = 'plantIDable',
                          label = 'Is a plant species identifiable?',
                          value = FALSE),
            checkboxGroupInput(inputId = 'plantIDcorrect',
                               label = 'Is the identification considered correct?',
                               choices = c('Family', 'Genus', 'Species')),
            selectInput(inputId = 'status_cat',
                        label = 'Choose a status category for the occurrence',
                        choices = c('Native', 'Native, origin unknown',
                                    'Introduced, origin unknown',
                                    'Intentionally introduced',
                                    'Accidentally introduced')),
            checkboxInput(inputId = 'plantHort',
                          label = 'Is the plant a horticultural species?',
                          value = FALSE),
            textInput(inputId = 'comment',
                      label = 'Comments'),
            actionButton(inputId = 'submit',
                         label = 'Submit'),
            actionButton(inputId = 'previous',
                         label = 'Previous')

        ),

        mainPanel(
           div(uiOutput("speciesImage"), style = 'float: left;'),
           leafletOutput('map', height = '200px', width = '400px'),
           textOutput('title'),
           textOutput('name'),
           htmlOutput('link')
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    imgID <- reactiveVal()
    if(file.exists(results)){
      so_far <- read.csv(results,
                         header = TRUE,
                         stringsAsFactors = FALSE)
      maxID <- min(ids$rand_id[ids$rand_id > max(as.numeric(so_far$rand_id),
                                                 na.rm = TRUE)]) 
      imgID(maxID)
    } else {
      imgID(min(ids$rand_id))
    }
    
    output$imgID <- renderText({
        paste('Current image:', imgID())
    })

    observeEvent(input$previous, {
      
      imgID(max(ids$rand_id[ids$rand_id < imgID()]))
      
    })
    
    # Output image title
    output$title <- renderText({
      
      paste('Title:', ids$title[ids$rand_id == imgID()])
      
    })
    
    # Output the displayed image name and score
    output$name <- renderText({
      
      x <- ids[ids$rand_id == imgID(),
              c('latin_name',
                'common_name_english',
                'classification_score')]
      paste('Classification:', x[1,1],
            x[1,2], '- score:',
            round(x[1,3],3))
    })
    
    # Output link to image
    output$link <- renderUI({
      a(href = as.character(ids$image_information_link[ids$rand_id == imgID()]),
        'Link to image on Flickr')
    })
    
    # Mini-map of location
    output$map <- renderLeaflet({
      
      x <- ids[ids$rand_id == imgID(), ]
      
      if(nrow(x) > 0){
      
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = x$longitude,
                     lat = x$latitude)
        
      }
    })
    
    observeEvent(input$submit, {
      
      done <- read.csv(results)

      if(!imgID() %in% done$rand_id){
        
        x <- ids[ids$rand_id == imgID(), ]
        x$plantIDable <- input$plantIDable
        x$plantIDcorrect <- paste(input$plantIDcorrect, collapse = ';')
        x$status_cat <- input$status_cat
        x$plantHort <- input$plantHort
        x$comment <- input$comment
        
        write.table(x, 
                    file = results, 
                    append = TRUE,
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ',')
        
        nv <- min(ids$rand_id[ids$rand_id > imgID()])
        imgID(nv) 
        
      } else {
        
        done[done$rand_id == imgID(),
             c('plantIDable', 'plantIDcorrect',
               'status_cat', 'plantHort',
               'comment')] <- c(input$plantIDable,
                                input$plantIDcorrect,
                                input$status_cat,
                                input$plantHort,
                                input$comment)
        
        write.table(done, 
                    file = results, 
                    append = FALSE,
                    col.names = TRUE,
                    row.names = FALSE,
                    sep = ',')
        
        nv <- min(ids$rand_id[ids$rand_id > imgID()])
        imgID(nv) 
      }
        
      updateCheckboxGroupInput(session = session, 
                               inputId = 'plantIDcorrect',
                               choices = c('Family', 'Genus', 'Species'))
      updateCheckboxInput(session = session,
                          inputId = 'plantIDable',
                          label = 'Is a plant species identifiable?',
                          value = FALSE)
      updateCheckboxInput(session = session,
                          inputId = 'plantHort',
                          label = 'Is the plant a horticultural species?',
                          value = FALSE)
      updateTextInput(session = session,
                      inputId = 'comment',
                      label = 'Comments',
                      value = '')
      updateSelectInput(session = session,
                        inputId = 'status_cat',
                        label = 'Choose a status category for the occurrence',
                        choices = c('Native', 'Native, origin unknown',
                                    'Introduced, origin unknown',
                                    'Intentionally introduced',
                                    'Accidentally introduced'),
                        selected = 'Native')
      
    })
    
    output$speciesImage <- renderUI({
        div(id = 'spImageDiv',
            img(src = as.character(ids$url_large_image[ids$rand_id == imgID()]),
                style = 'max-width: 600px; max-height: 600px')
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
