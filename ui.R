# token <- readRDS("droptoken.rds")
# rdrop2::drop_acc(dtoken = token)
require(shiny)

ui <- fluidPage(theme = "bootstrap.css",
  
  HTML('<head>
        <meta charset="utf-8" />
        <title>OptFilBovApp</title>
        </head>
      
        <div id="entete">
        <h1 id="main_title">Simulateur de performances bovines</h1>
        </div>'),
  
  
  #Side bar layout ----  
  sidebarLayout(fluid = TRUE,
                
                #choix des UI ----
                sidebarPanel(
                  radioButtons(inputId = "PI", 
                               label = "Choix parametre d'interet:",
                               choices = c("Efficience Individuelle" = "EI",
                                           "Qualite de Carcasses" = "QC",
                                           "Qualite Nutritionnelle" = "QN",
                                           "Qualite Sensorielle" = "QS"),
                               selected = "EI"),
                  
                  radioButtons(inputId = "scenario_comparatif", 
                               label = "Scenarios comparatifs:",
                               choices = c("Oui" = TRUE,
                                           "Non" = FALSE),
                               selected = FALSE),
                  
                  radioButtons(inputId = "graph_choice", 
                               label = "Choix graphique:",
                               choices = c("Barplot" = 'barplot',
                                           "Diagramme en radar" = 'radar_diag'),
                               selected = 'radar_diag'),
                  
                  
                  fluidRow(column(12, uiOutput('plot_tableau'))),
                  
                  width = 4
                ),
                
                
                #Apparition des sliders, plot et tableau dynamiquement ----
                mainPanel(
                  fluidRow(
                    column(6, uiOutput("dynamic_PI")),
                    column(6, uiOutput("dynamic_scenario")),
                    column(6, plotOutput('plot_1')),
                    column(6, uiOutput('plot_scenario2'))
                  )
                )
  ),
 
  HTML('<hr>
       <footer>
       <div id="logo_inra">
       <img src="inra_logo.png" alt="logo_company" id="logo_inra"/></div>
       <div id="social_network">
       <p>
       <a href="https://github.com/alex-conanec"><img src="github_logo.png" alt="logo_network" id="logo_github"/></a>
       <a href="https://www.linkedin.com/in/alexandre-conanec"><img src="in.png" alt="logo_network" id="logo_linkedin"/></a>
       </p>
       </div>
       <div id="logo_inria">
       <img src="inria_logo.png" alt="logo_company" id="logo_inria"/></div>')
  
)



