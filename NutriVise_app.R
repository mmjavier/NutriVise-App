# @author: Myko Jefferson Javier
# @date: December 12, 2024
# @course: CMSC 150 -B4L
# CMSC-150: Final Project

source("dietSolver.R")

if(interactive()){
  
  #Check if all packages are already installed in the system, if not installs it
  packages = c("shiny", "bslib", "bs4Dash", "systemfonts", "shinyjs", "DT", "shinyAce", "fresh")
  for(package in packages){
    if(system.file(package = package) == ""){
      install.packages(package)
    }
  }
  
  #Initialize Libraries
  library(shiny)
  library(bslib)
  library(bs4Dash)
  library(systemfonts)
  library(shinyjs)
  library(DT)
  library(shinyAce)
  library(fresh)
  
  #UI Section
  ui = fluidPage(
    
    useShinyjs(),
    
    #Set primary theme color to a custom hue
    use_theme(create_theme(
      bs4dash_status(
        primary = "#6dc6cc"
      )
    )),
    
    #Welcome Page
    title = "Nutrivise",
    conditionalPanel(
      condition = "output.showDashboard == false",
      fluidRow(
        div(
          class = "welcomePage",
          div(
            class = "text",
            h1("Welcome to", span("NutriVise!")),
            p("A Diet Solver app that will help you find the cheapest and most nutritious combination of foods 
          that will satisfy all off your daily nutritional requirements!"),
            actionButton("showDashboard", "Get Started", class = "button") #Button to go to the Diet Solver
          )
        )
      )
    ),
    
    #Diet Solver
    conditionalPanel(
      condition = "output.showDashboard == true",
      dashboardPage(
        help = NULL,
        dark = NULL,
        
        dashboardHeader(
          title = tagList(tags$img(src ="Nutrivise_logo.png", height = "50px", class = "logo"), span("NutriVise", class = "headerTitle"))),
        
        dashboardSidebar(
          sidebarMenu(
            menuItem("Diet Solver", tabName = "diet-solver", icon = icon("bowl-food")),
            menuItem("Nutrition Table", tabName = "nutritionTable", icon = icon("nutritionix")),
            menuItem("About", tabName = "about", icon = icon("circle-info"))
          ),
        ),
        
        dashboardBody(
          tabItems(
            tabItem(
              tabName = "diet-solver", #Diet Solver Tab
              conditionalPanel(
                condition = "output.optimize == false",
                fluidRow(
                  column(3, #Column for Food Selection Check Box Group
                         card(
                           card_header(span("Food Selections", class="subHeader"), class = "outputHeader"),
                           span(
                             h1("Select:", class = "smallHeader"),
                             actionButton("selectAll", "All", class = "select-button"), #Select All Button
                             actionButton("selectNone", "Reset", class = "select-button"), #Reset Selection Button
                             class = "buttonHolder"
                           ),
                           #Food Choice CheckBox
                           checkboxGroupInput(
                             "foods",
                             NULL,
                             choices = list("Frozen Broccoli" = 1,
                                            "Raw Carrots" = 2,
                                            "Raw Celery" = 3,
                                            "Frozen Corn" = 4,
                                            "Raw Iceberg Lettuce" = 5,
                                            "Raw Sweet Peppers" = 6,
                                            "Baked Potatoes" = 7,
                                            "Tofu" = 8,
                                            "Roasted Chicken" = 9,
                                            "Spaghetti W/ Sauce" = 10,
                                            "Ripe & Raw Red Tomato" = 11,
                                            "Raw Apple W/Skin" = 12,
                                            "Banana" = 13,
                                            "Grapes" = 14,
                                            "Fresh Raw Kiwifruit" = 15, 
                                            "Oranges" = 16,
                                            "Bagels" = 17,
                                            "Wheat Bread" = 18,
                                            "White Bread" = 19,
                                            "Oatmeal Cookies" = 20,
                                            "Apple Pie" = 21,
                                            "Chocolate Chip Cookies" = 22,
                                            "Regular Butter" = 23,
                                            "Cheddar Cheese" = 24,
                                            "3.3% Whole Fat Milk" = 25,
                                            "2% Lowfat Milk" = 26,
                                            "Skim Milk" = 27,
                                            "Poached Eggs" = 28,
                                            "Scrambled Eggs" = 29,
                                            "Bologna Turkey" = 30,
                                            "Frankfurter Beef" = 31,
                                            "Extralean Sliced Ham" = 32,
                                            "Pork Kielbasa" = 33,
                                            "Cap'N Crunch" = 34,
                                            "Cheerios" = 35,
                                            "Kellogg'S Corn Flakes" = 36,
                                            "Kellogg'S Raisin Bran" = 37,
                                            "Rice Krispies" = 38,
                                            "Special K" = 39,
                                            "Oatmeal" = 40,
                                            "Choc Malt-O-Meal" = 41,
                                            "Pizza W/Pepperoni" = 42,
                                            "Taco" = 43,
                                            "Hamburger W/Toppings" = 44,
                                            "Hotdog Plain" = 45,
                                            "Couscous" = 46,
                                            "White Rice" = 47,
                                            "Macaroni Ckd" = 48,
                                            "Peanut Butter" = 49,
                                            "Pork" = 50,
                                            "Sardines in Oil" = 51,
                                            "White Tuna in Water" = 52,
                                            "Air-Popped Popcorn" = 53,
                                            "Potato Chips Bbqflvr" = 54,
                                            "Pretzels" = 55,
                                            "Tortilla Chip" = 56,
                                            "Chicken noodle Soup" = 57,
                                            "Split Pea & Hamsoup" = 58,
                                            "Vegetbeef Soup"  = 59,
                                            "Neweng Clam Chowder" = 60,
                                            "Tomato Soup" = 61,
                                            "New E Clamchwd,W/Mlk" = 62,
                                            "Crm Mshrm Soup,W/Mlk" = 63,
                                            "Beanbacn Soup,W/Watr" = 64),
                             selected = c(1:20)
                           )
                         )),
                  column(9, # column where selected foods are displayed
                         card(
                           class = "accent",
                           width = 100,
                           h2("You've selected the following:  ", class = "subHeader"),
                           div(
                             class = "text-Output",
                             textOutput("selectedFoods")
                           ),
                         ),
                         actionButton("optimize", "Optimize", class= "opt-button")
                         
                  )
                )
              ),
              #Panel Showing the Result of the Simplex Computation
              conditionalPanel(
                condition = "output.optimize == true",
                card( #Displaying the optimized cost of the diet through text output, and the optimized menu through data table output
                  class = "accent",
                  h2("The Optimized Diet:  ", class = "headerTitle"),
                  div(
                    class = "result-Output",
                    textOutput("textResult"),
                    span("Optimal Menu", class="subHeader"),
                    DTOutput("menuMat")
                  )
                ),
                span(
                  actionButton("viewTableau", "View Results", class = "opt-button"), #button for showing the tableau and basic Sol per iteration
                  actionButton("returnHome", "Go Back", class = "opt-button") #button for going back to the food selection panel
                ),
                conditionalPanel(
                  class = "iterationDisplay",
                  condition = "output.showTableau == true",
                  column(12, #display initial tableau
                         h2("Initial Tableau"),
                         DTOutput("initialTableau")
                  ),
                  column(12, #display initial basic solution
                         h2("Initial Basic Solution"),
                         DTOutput("initialBasicSol")
                  ),
                  column(12, #display the tableau and basic sol from iteration 1 to nth iteration
                         uiOutput("displayIterations")
                  )
                ),
                
              )
              
            ),
            
            #Nutrition Table
            tabItem(
              tabName = "nutritionTable",
              card( #displays the nutrition table provided in the project specs
                card_header(span("Nutrition Table", class="headerTitle"), class = "cardHeader"),
                div(
                  DTOutput("nutritionTable")
                )
                
              )
            ),
            
            #About Tab
            tabItem(
              tabName = "about",
              div(
                class = "about",
                column(7, #Display code snippet
                       div(
                         class = "code-snippet",
                         aceEditor(
                           outputId = "codeSnippet",
                           mode = "r",        
                           theme = "dawn",      
                           readOnly = TRUE,                
                           height = "500px",               
                           fontSize = 13              
                         )
                       )
                ),
              )
            )
          )
        )
      ),
    ),
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
  )
  
  
  #Server Section
  server = function(input, output, session) {
    
    # Reactive value to store Simplex result
    simplexResults <- reactiveVal(NULL)
    
    #Vector containing the names of the Food Selections (used for displaying the names of the selected foods from the checkbox)
    Foods = c("Frozen Broccoli", "Raw Carrots", "Raw Celery", "Frozen Corn", "Raw Iceberg Lettuce", "Raw Sweet Peppers", "Baked Potatoes", "Tofu","Roasted Chicken","Spaghetti W/ Sauce","Ripe & Raw Red Tomato","Raw Apple W/Skin", "Banana","Grapes", "Fresh Raw Kiwifruit", "Oranges","Bagels","Wheat Bread","White Bread","Oatmeal Cookies","Apple Pie", "Chocolate Chip Cookies","Regular Butter", "Cheddar Cheese", "3.3% Whole Fat Milk", "2% Lowfat Milk","Skim Milk","Poached Eggs","Scrambled Eggs","Bologna Turkey", "Frankfurter Beef","Extralean Sliced Ham","Pork Kielbasa", 
              "Cap'N Crunch", "Cheerios","Kellogg'S Corn Flakes", "Kellogg'S Raisin Bran","Rice Krispies","Special K","Oatmeal", "Choc Malt-O-Meal","Pizza W/Pepperoni","Taco","Hamburger W/Toppings","Hotdog Plain", "Couscous","White Rice", "Macaroni Ckd","Peanut Butter","Pork","Sardines in Oil","White Tuna in Water", "Air-Popped Popcorn","Potato Chips Bbqflvr","Pretzels","Tortilla Chip", "Chicken noodle Soup", "Split Pea & Hamsoup","Vegetbeef Soup","Neweng Clam Chowder","Tomato Soup","New E Clamchwd,W/Mlk","Crm Mshrm Soup,W/Mlk", "Beanbacn Soup,W/Watr")
    
    # Initialize Reactive Values
    isVisible = reactiveVal(FALSE) # showDashboard
    showResult = reactiveVal(FALSE) # show optimization results
    showTableau = reactiveVal(FALSE) # show tableau & basicSol per iteration
    
    # Track button changes of showDashboard
    output$showDashboard = reactive({ isVisible() }) # creating showDashboard output
    outputOptions(output, "showDashboard", suspendWhenHidden = FALSE)
    observeEvent(input$showDashboard, { isVisible(TRUE) })
    
    # Reset Button Clicked
    observeEvent(input$selectNone, {
      updateCheckboxGroupInput(session = session, inputId = "foods", selected = character(0)) # Set the selected value to NULL
    })
    
    # Select All button clicked
    observeEvent(input$selectAll, {
      updateCheckboxGroupInput(session = session, inputId = "foods", selected = c(1:64)) # Sets the selected value to all the foods (1:64)
    })
    
    # Outputs the selected food choices to text
    output$selectedFoods = renderText({
      if (is.null(input$foods)) {
        "No food selected"
      } else {
        paste(Foods[as.numeric(input$foods)], collapse = ", ")
      }
    })
    
    # Sets the optimize button to be initially disabled
    shinyjs::disable("optimize")
    
    # Enables the button whenever the selected vector is not empty (i.e. User has some foods selected)
    observe({
      if (length(input$foods) > 0) {
        shinyjs::enable("optimize")
      } else {
        shinyjs::disable("optimize")
      }
    })
    
    # Creates the output optimize (used for the conditional panel), that updates off the reactive value showResult()
    output$optimize = reactive({ showResult() })
    outputOptions(output, "optimize", suspendWhenHidden = FALSE)
    
    # Optimize Button Clicked
    observeEvent(input$optimize, {
      selected_values = as.numeric(input$foods)
      
      # Store the result of Simplex to simplexResults (allows other outputs outside this observeEvent to access the results)
      results <- Simplex(selected_values)
      simplexResults(results)
      
      # Check if results is not NULL, and update the panels accordingly
      if (!is.null(simplexResults())) {
        showResult(TRUE) #Only show results panel if simplex returns a list
        # Print Results
        output$textResult = renderText({
          paste("The cost of the optimal diet with the selected food combinations is $", formatC(results$Z), ".", collapse = "\n")
        })
        
        output$menuMat = renderDT({
          datatable(results$menu, rownames = FALSE, options = list(
            dom = 't', scrollX = TRUE, pageLength = nrow(results$menu)
          ))
        })
      } else { #If Simplex returns NULL, the problem is infeasible (either Max Iteration was reached or pivot Element was 0)
        simplexResults(NULL) #sets the result to NULL
        showModal(modalDialog( #prints the prompt
          title = "PROBLEM INFEASIBLE",
          "It is not possible to meet the nutritional constraints with the foods that you have selected.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    # Creates the output showTableau (used for the conditional panel), that updates off the reactive value showTableau()
    output$showTableau = reactive({ showTableau() })
    outputOptions(output, "showTableau", suspendWhenHidden = FALSE)
    
    # View Results button clicked
    observeEvent(input$viewTableau, {
      showTableau(TRUE)
      
      # Outputs the Initial Tableau
      output$initialTableau = renderDT({
        results <- simplexResults()  # Access the Simplex results reactively
        datatable(results$tableauList[[1]], rownames = FALSE, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'rgb(109,198,204,0.3)', 'color': '#2a2a2a'});",
            "}"), dom = 't', scrollX = TRUE, pageLength = nrow(results$tableauList[[1]]), autoWidth = FALSE))
      })
      
      # Converts the initial basic solution vector to a data frame so DTOutput can display it
      results <- simplexResults()
      initialBasicSol = as.data.frame(t(results$basicSolList[[1]]))
      col_names = colnames(results$tableauList[[1]])
      col_names = col_names[-c(length(col_names))]
      colnames(initialBasicSol) = col_names
      
      output$initialBasicSol = renderDT({
        datatable(initialBasicSol, rownames = FALSE, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'rgb(109,198,204,0.3)', 'color': '#2a2a2a'});",
            "}"), dom = 't', scrollX = TRUE, autoWidth = FALSE, pageLength = nrow(initialBasicSol)))
      })
      
      # Outputs the tableau and basic solutions per iteration
      output$displayIterations = renderUI({
        results <- simplexResults()
        num_iterations = (results$numIteration - 1)  # subtracts 1 from the number of iteration as the initial values are stored at index 1
        
        # Works as a for-loop
        lapply(1:num_iterations, function(i) {
          # Gets the Basic Solution per Iteration
          basicSol = as.data.frame(t(results$basicSolList[[i + 1]]))
          colnames(basicSol) = col_names
          
          tagList(
            # Outputs the Tableau per Iteration
            div(
              style = "margin-top: 30px;",
              h3(paste("Tableau at Iteration", i)),
              renderDT({
                datatable(results$tableauList[[i + 1]], rownames = FALSE, options = list(
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(109,198,204,0.3)', 'color': '#2a2a2a'});",
                    "}"), dom = 't', scrollX = TRUE, pageLength = nrow(results$tableauList[[i + 1]]), autoWidth = FALSE))
              })
            ),
            # Outputs the Basic Solution per Iteration
            div(
              style = "margin-top: 30px;",
              h3(paste("Basic Solution at Iteration", i)),
              renderDT({
                datatable(basicSol, rownames = FALSE, options = list(
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rgb(109,198,204,0.3)', 'color': '#2a2a2a'});",
                    "}"), dom = 't', scrollX = TRUE, pageLength = nrow(basicSol), autoWidth = FALSE))
              })
            )
          )
        })
      })
    })
    
    # Go Back Button Clicked
    observeEvent(input$returnHome, {
      showResult(FALSE)
      showTableau(FALSE)
      simplexResults(NULL)  # Clear the Simplex results
    })
    
    #Displaying the Nutrition/InfoTable
    colnames(InfoTable) = c("Cost","Calories","Cholesterol (mg)","Total Fat (g)", "Sodium (mg)", "Carbs(g)", "Dietary Fiber (g)", "Protein (g)", "Vitamin A", "Vitamin C", "Calcium (mg)", "Iron (mg)")
    rownames(InfoTable) = Foods
    output$nutritionTable = ({renderDT({
      datatable(InfoTable, rownames = TRUE, options = list(dom = 't', scrollx = TRUE, pageLength = nrow(InfoTable)))
    })
    })
    
    #Display Code Snippet in About page
    code <- paste(readLines("dietSolver.R"), collapse = "\n")
    # Update the Ace editor with the code
    updateAceEditor(session, "codeSnippet", value = code)
  }
}

shinyApp(ui, server)
