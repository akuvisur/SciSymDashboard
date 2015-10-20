library(RCurl)
library(httr)
library(jsonlite)
require(shinysky)
library(shinyjs)
library(shinydashboard)
library(DT)

schemas = jsonlite::fromJSON(getURL("http://localhost:5000/schema/all/"))

ui <- function() {

  dashboardPage(skin='black',
  
    dashboardHeader(title = "SciSym Dashboard"),
    
    dashboardSidebar(
      sidebarMenu(
        id="tabs",
        menuItem("Schemas", tabName="schemas", icon = icon("th")),
        menuItem("Schema Creator", tabName="creator", icon = icon("plus"))
      )
    ),
    
    body <- dashboardBody(
      # javascript filet www-kansioon
      #tags$head(tags$script(src="input.js")),
      useShinyjs(),

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),

      tabItems(
        schemaTab(),
        creatorTab()
      ),
      HTML('<hr style="color: #0A1F33;">'),
      fluidRow(shinyalert("general_alert", auto.close.after = 15))
    )
  )

}

server <- function(input, output, session) {
  
  output$factor_creator <- renderUI({
    if (is.null(input$factor_type))
      return()
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$factor_type,
      "tracked" = doTracked(),
      "multiple" = doMultiple()
    )
  })

  output$schematable = DT::renderDataTable( {
    schemas[c("title", "desc", "author")]
  }, options = list(pageLength = 5), colnames=c("Title", "Description", "Author"), server = FALSE
  )

  output$table_selected = renderUI({
    if (length(input$schematable_rows_selected)) {
      sr = as.numeric(input$schematable_rows_selected[1])
      s = POST("http://localhost:5000/symptom/keys/",
        body = toJSON(schemas$symptoms[sr])
      )
      f = POST("http://localhost:5000/factor/keys/",
        body = toJSON(schemas$factors[sr])
      )
      fluidPage(
        renderSchemaInformation(sr, jsonlite::fromJSON(content(s, "text")), jsonlite::fromJSON(content(f, "text")))
      )
    }
    else {
      fluidPage(renderPrint(cat("Select a schema to view more information")), hr())
    }
  })

  observeEvent(input$add_symptom, {
    #generate json
    newsymptom <- list(
      name=input$symptom_name,
      desc=input$symptom_desc,
      severity=input$symptom_severity,
      class=input$symptom_class,
      rep_window=input$symptom_window
    )
    resp <- POST("http://localhost:5000/symptom/add/", body = toJSON(newsymptom))
    res = content(resp, "text")
    js = fromJSON(res)
    
    if (js["info"] == "Added new symptom") {
      updateTextInput(session, "symptom_name", value = "")
      updateTextInput(session, "symptom_desc", value = "")
      showshinyalert(session, "general_alert", "New symptom added", "success")
    }
    else {
      showshinyalert(session, "general_alert", paste(js["info"]), "warning")
    }

  })

  observeEvent(input$add_factor, {
    #generate json
    newfactor <- list(
      name=input$factor_name,
      desc=input$factor_desc,      
      rep_window=input$factor_window,
      input=input$factor_type,
      values=input$factor_values,
      range_min=input$factor_min_range,
      range_max=input$factor_max_range
    )
    
    resp <- POST("http://localhost:5000/factor/add/", body = toJSON(newfactor))
    res = content(resp, "text")
    js = fromJSON(res)
    
    if (js["info"] == "Added new factor") {
      values = input$factor_selector
      values[[length(values)+1]] <- js["key"]

      updateTextInput(session, "factor_name", value = "")
      updateTextInput(session, "factor_desc", value = "")
      updateNumericInput(session, "factor_max_range", value = 10)
      updateNumericInput(session, "factor_min_range", value = 0)
      updateTextInput(session, "factor_values", value = "")
      showshinyalert(session, "general_alert", "New factor added", "success")

      updateSelectizeInput(session, "factor_selector", selected = list(values), label="this just changed")
      updateTabsetPanel(session, "factor_tabset", selected = "Select existing")
    }
    else {
      showshinyalert(session, "general_alert", paste(js["info"]), "warning")
    }  
  })
  
  # store schema
  observeEvent(input$save_schema, {
    newschema <- list(
      title=input$schema_title,
      desc=input$schema_desc,
      author=input$schema_author,
      schema_type=input$schema_type,
      symptoms=list(input$symptom_selector),
      factors=list(input$factor_selector)
    )

    resp <- POST("http://localhost:5000/schema/add/", body = toJSON(newschema))
    res = content(resp, "text")
    js = fromJSON(res)

    if (js["info"] == "Added new schema") {
      showshinyalert(session, "general_alert", paste(js["info"]), "success")
      updateSelectizeInput(session, "factor_selector", selected = '', choices = '')
      updateSelectizeInput(session, "symptom_selector", selected = '', choices = '')
      updateTextInput(session, "schema_author", value = "")
      updateTextInput(session, "schema_title", value = "")
      updateTextInput(session, "schema_desc", value = "")
      updateTabItems(session, "tabs", "schemas")
      renderSchemas()
    }
    else {
      showshinyalert(session, "general_alert", paste(js["info"]), "warning")
    }

  })

  observeEvent(input$switch, {
    updateTabItems(session, "tabs", "creator")
  })
  
  observeEvent(input$help_title, {
    showshinyalert(session, "general_alert", "This is the title", "info")
  })
  observeEvent(input$help_author, {
    showshinyalert(session, "general_alert", "This is the author", "info")
  })
  observeEvent(input$schema_help_desc, {
    showshinyalert(session, "general_alert", "This is the desc", "info")
  })
  
}

renderSchemas <- function() {
  schemas = jsonlite::fromJSON(getURL("http://localhost:5000/schema/all/"))
  output$schematable = DT::renderDataTable( {
    schemas[c("title", "desc", "author")]
  }, options = list(pageLength = 5), colnames=c("Title", "Description", "Author")
  )
}

renderSchemaInformation <- function(schemaRow, syms, facs) {
  tags$div(class ='schema_info',
    fluidPage(
      fluidRow(
        column(1, h5(tags$b("Title:"))), 
        column(7, h5(schemas$title[schemaRow])),
        column(1, h5(tags$b("DB:"))),
        column(3, h5(schemas$db_name[schemaRow]))
      ),
      fluidPage(
        fluidRow(column(8, h5(tags$b("Description:"))), column(1, h5(tags$b("Author:"))), column(3, h5(schemas$author[schemaRow]))),
        fluidRow(h5(schemas$desc[schemaRow]))
      ),
      fluidRow(
          #symptoms
          column(6, h5(tags$b("Symptoms:")), renderSymptoms(syms)
          ),
          # factors
          column(6, h5(tags$b("Factors:")), renderFactors(facs)
          )
        )
    )
  )
}

renderSymptoms <- function(syms) {
  lapply(1:nrow(syms), function(i) {
    div(class='schema_symptoms', 
      tags$b(syms$name[i]), 
      br(),
      syms$desc[i],
      tags$ul(
        tags$li(tags$b("Severity:"), syms$severity[i]),
        tags$li(tags$b("Reporting:"), syms$rep_window[i]),
        tags$li(tags$b("Class:"), syms$class[i])
      )
    )
  })
}

renderFactors <- function(facs) {
  lapply(1:nrow(facs), function(i) {
    div(class='schema_factors', 
      tags$b(facs$name[i]),
      br(),
      facs$desc[i],
      tags$b("Reporting:"), facs$rep_window[i],
      renderInput(facs[i,])
    )
  })
}

renderInput <- function(factor) {
  if (factor$input == 'multiple') {
    h5(tags$b("Input type:"), factor$input)
    tags$ul(tags$li(h5(tags$b("Values:"), factor$values)))
  }
  else if (factor$input == 'tracked') {
    h5(tags$b("Input type:"), factor$input)
    tags$ul(tags$li(h5(paste("Min: ", factor$range_min, " Max: ", factor$range_max))))
  
  }
}

schemaTab <- function() {
  tabItem(tabName = "schemas", 
    fluidPage(DT::dataTableOutput("schematable")),
    uiOutput("table_selected")
  )
}

creatorTab <- function() {
  tabItem(tabName = "creator",
    fluidPage(navbarPage("Schema creator", id ="creator_general_info",
      tabPanel("General info",
        useShinyjs(),
        creatorGeneralInfo(),
        fluidPage(shiny::actionButton("save_schema", "Save as new schema"))
      ),
      creatorSymptoms(),
      creatorFactors()
      )
    )
  )
}

creatorGeneralInfo <- function() {
  fluidPage(
    fluidPage(
     fluidRow(
       column(2, h5(tags$b("Title:"))), 
       column(4, textInput("schema_title", NULL)), 
       column(1, shiny::actionButton("help_title", NULL, icon("question")))
     ),
     
     fluidRow(
       column(2, h5(tags$b("Author:"))), 
       column(4, textInput("schema_author", NULL)), 
       column(1, shiny::actionButton("help_author", NULL, icon("question")))
     ),
     fluidRow(
       column(2, h5(tags$b("Description:"))),
       column(6, tags$textarea(id="schema_desc", rows=6, cols=50, NULL)), 
       column(1, shiny::actionButton("schema_help_desc", NULL, icon("question")))
     ),
     fluidRow(
       column(4, 
          radioButtons("schema_type", 
           tags$b("Schema type:"), 
           choices=c("Continuous", "Filled only once"))
       )
     )
   )
 )
}

creatorSymptoms <- function() {
  tabPanel("Symptoms",
    # add new existing / create new
    tabsetPanel(id = "symptom_tabset",
     tabPanel("Select existing", icon = icon("ellipsis-v"),
        symptomCreatorInput()
    ),
    #create new symptom
    tabPanel("Create new", icon = icon("plus"),
    fluidPage(
      fluidRow(textInput("symptom_name", "Name:")),
      fluidRow(
        h5(tags$b("Description:")),
        tags$textarea(id="symptom_desc", rows=3, cols=50, NULL)
      ),
      fluidRow(
        radioButtons("symptom_severity", "Severity:", 
                   c("Severe" = "severe", "Non-severe" = "none"),
                   inline = TRUE)
      ),
      fluidRow(radioButtons("symptom_window", "Reporting frequency:", 
        c(
          "Hourly"="hour",
          "Daily"="day",
          "Weekly"="week",
          "Monthly"="month"
        ),
        inline = TRUE)
      ),
      fluidRow(textInput("symptom_class", "Class:"))
      ),
      
      fluidRow(shiny::actionButton("add_symptom", "Save new symptom"))
      )
    )
  )
}

creatorFactors <- function() {
  tabPanel("Factors",
    tabsetPanel(id = "factor_tabset",
      tabPanel("Select existing", icon = icon("ellipsis-v"),
        factorCreatorInput()   
      ),
      tabPanel("Create new", icon = icon("plus"),
      fluidPage(
        fluidRow(textInput("factor_name", "Name:")),
        fluidRow(
          h5(tags$b("Description:")),
          tags$textarea(id="factor_desc", rows=3, cols=50, NULL)
        ),
        fluidRow(radioButtons("factor_window", "Reporting frequency:", 
          c(
            "Hourly"="hour",
            "Daily"="day",
            "Weekly"="week",
            "Monthly"="month"
          ),
          inline = TRUE)
        ),
        fluidRow(
          radioButtons("factor_type", "Input type:", 
            c("Tracked" = "tracked", "Multiple" = "multiple"),
            inline = TRUE)
        ),
        
        fluidRow(uiOutput("factor_creator")),
        
        fluidRow(shiny::actionButton("add_factor", "Save new factor"))
        )
      )
    )
  )
}

symptomCreatorInput <- function() {
  selectizeInput('symptom_selector', 'Select symptom(s)', multiple=TRUE, choices='',
    options = list(
      valueField = 'key',
      labelField = 'name',
      searchField = 'name',
      options = list(),
      create = FALSE,
      maxItems = 20,
      render = I("{
        option: function(item, escape) {
          return '<div>' +
          '<strong>'+escape(item.name)+'</strong>:<br>' +
          escape(item.desc) + '<br><ul>' +
          '<li>Severity: '+escape(item.severity) + '</li>' +
          '<li>Input type: '+escape(item.rep_window)+'</li>' +
          '<li>Class: '+escape(item.class)+'</li>' +
          '</ul></div>';
        }
      }"),
      score = I("function(search) {
        var score = this.getScoreFunction(search);
        return function(item) {
          return score(item) * (1);
        };                     
      }"),
      load = I("function(query, callback) {
        if(!query.length) return callback();
        $.ajax({
          url: 'http://localhost:5000/symptom/search/?search=' + encodeURIComponent(query),
          type: 'GET',
          error: function() {
            callback();
          },
          success: function(res) {
            r = JSON.parse(res);
            callback(r.results.slice(0,10));
          }
        });
      }
      "                        
      )
    )
  )
}

factorCreatorInput <- function() {
  selectizeInput('factor_selector', 'Select factor(s)', multiple=TRUE, choices='',
    options = list(
      valueField = 'key',
      labelField = 'name',
      searchField = 'name',
      options = list(),
      create = FALSE,
      maxItems = 20,
      render = I("{
        option: function(item, escape) {
          console.log(item);
          return '<div>' +
          '<strong>'+escape(item.name)+'</strong>:<br>' +
          escape(item.desc) + '<br><ul>' +
          '<li>Input type: '+escape(item.input) + '</li>' +
          '<li>Input frequency: '+escape(item.rep_window)+'</li>' +
          '</ul></div>';
        }
      }"),
      score = I("function(search) {
        var score = this.getScoreFunction(search);
        return function(item) {
          return score(item) * (1);
        };                     
      }"),
      load = I("function(query, callback) {
        if(!query.length) return callback();
        $.ajax({
          url: 'http://localhost:5000/factor/search/?search=' + encodeURIComponent(query),
          type: 'GET',
          error: function() {
           callback();
          },
          success: function(res) {
            r = JSON.parse(res);
            callback(r.results.slice(0,10));
          }
        });
      }
      "                        
      )
    )
  )
}

doMultiple <- function() {
  fluidPage(h5(tags$b("Possible values: (separate with ',')")),
  tags$textarea(rows=4, cols=60, id="factor_values"))
}

doTracked <- function() {
  fluidPage(numericInput("factor_min_range", "Minimum value", value = 0),
  numericInput("factor_max_range", "Maximum value", value = 10))
}

shinyApp(ui, server)