ThreeDSDD <- function() {
    ### cheking for installed packages and if not yet installed they will be installed:
    package_list <-
        c(
            "shiny"
            , "shinyFiles"
            , "miniUI"
            , "ggplot2"
            , "jsonlite"
            , "rgl"
            , "tidyverse"
            , "plot3D"
            , "data.table"
            , "imager"
            , "scatterplot3d"
            , "Cairo"
            , "BBmisc"
            , "jsonlite"
            , "plotly"
            , "threejs"
            , "nlme"
        )

    if(
        sum(package_list %in% installed.packages()) < length(package_list)
    ){
        packages_to_install <-
            package_list[!{package_list %in% installed.packages()}]
        message(
            c("The following packages have not been installed yet: \n \t")
            , toString(packages_to_install)
        )
        install.packages(
            packages_to_install
            , dependencies = TRUE
        )
    }else{
        message("All dependencies fullfilled ...")
    }

    ### raise the limit for upload file size
    base::options(shiny.maxRequestSize=500*1024^2)

    polar2cart_m <- function(m){
        x <-
            m[,2]*cos(m[,1])
        y <-
            m[,2]*sin(m[,1])
        z <- base::tryCatch(
            m[,3]
            , error = function(x) return(rep(0, times = {m %>% nrow()})
                                         )
            )
        base::return(
            base::matrix(data = c(x,y,z), ncol = 3)
        )
    }

    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar(
            title = "NanoParticle Administration Sedimentation Simulator (NanoPASS)"
            , left = miniUI::miniTitleBarCancelButton()
            , right = miniUI::miniTitleBarButton(
                inputId = "quit"
                , label = "Quit Application"
                , primary = TRUE)
        ),
        shiny::withMathJax(),
        miniUI::miniTabstripPanel(

    # User Input Tab ----------------------------------------------------------

    miniUI::miniTabPanel("User Input", icon = shiny::icon("sliders"),
    miniUI::miniContentPanel(padding = 0,
        shiny::navlistPanel("User Input Panel",well = F,
            shiny::tabPanel("Calculation Method",
            shiny::h3(""),
            ### input flag for diffusion coefficient
            shiny::selectInput(
                inputId = "uip_flag_diffusion_coefficient"
                , label = "Set flag 'diffusion coefficient':"
                , choices = c(
                    "Simulation based on particle size or particle distribution" = FALSE
                    , "Simulation based on diffusion coefficient or diffusion coefficient distribution (default)" = TRUE
                )
                , selected = TRUE
                , multiple = FALSE
            )
            ### input flag for distribution
            , shiny::selectInput(
                inputId = "uip_flag_distribution"
                , label = "Set flag 'distribution':"
                , choices = c(
                    "Simulation based on average distribution value" = FALSE
                    , "Simulation based on diffusion coefficient distribution or particle size distribution from CSV file (default)" = TRUE
                    )
                , selected = TRUE
                , multiple = FALSE
            )
            ### input the distribution from CSV file
            , shiny::fileInput(
                inputId = "uip_example_distribution_path"
                , label = "Select CSV file with particle size distribution"
                , width = "300px"
                , accept = c(
                    "text/csv"
                    , "text/comma-separated-values,text/plain"
                    , ".csv"
                    )
                , multiple = FALSE
                , buttonLabel = "Select ..."
            )
            ### input average diffusion coefficient
            , shiny::numericInput(
                inputId = "uip_diffusion_coefficent"
                , label = shiny::HTML(
                    paste(
                        "average diffusion coefficient [nm"
                        , shiny::tags$sup(2)
                        , "s"
                        , shiny::tags$sup(-1)
                        , "]:"
                    )
                )
                , value = 489e+4
                )
            ),## End of TabPanel 'Calculation Method'
            shiny::tabPanel("Nanoparticle Characteristics", icon = shiny::icon(""),
            ### Input number of particles
            shiny::numericInput(
                inputId = "uip_particle_number"
                , label = "Number of Particles for Simulation (max: 100000):"
                , value = 10000
                , min = 100
                , max = 100000
                , step = 100
            )
            ### input simulation time
            , shiny::numericInput(
                inputId = "uip_simulation_time"
                , label = "Simulation time in hours (max: 72h):"
                , value = 24
                , min = 1
                , max = 72
                , step = 1
            )
            ### snapshot time
            , shiny::numericInput(
                inputId = "uip_snap_shot"
                , label = "Fraction of an hour a data snapshot is taken (1 = every hour, 0,5 = every 30min, 0,25 = every 15 min, ...)"
                , value = 1
                , min = 0.05
                , max = 1
                , step = 0.05
            )
            ### particles effective density
            , shiny::numericInput(
                inputId = "uip_effective_density"
                , label =
                shiny::HTML(
                    paste(
                        "effective density [g cm"
                        , shiny::tags$sup(-3)
                        ,"]:"
                    )
                )
                , value = 1.552
            )
            ### particles hydrodynamic diameter
            , shiny::numericInput(
                inputId = "uip_hydr_diam"
                , label = "hydrodynamic diameter [nm]:"
                , value = 150
                , min = 1
                , max = 500
                , step = 1
            )
        ),## End of tabPanel 'Nanoparticle Characteristics'
            shiny::tabPanel("Cell Culture Dish", icon = shiny::icon(""),
            shiny::numericInput(
                inputId = "uip_bottom_area"
                , label = shiny::HTML(
                paste(
                    "dish bottom area [cm"
                    , shiny::tags$sup(2)
                    , "]:"
                )
                )
                , value = 0.34
                , min = 0.01
                , max = 5
                , step = 0.01
            ),
            shiny::numericInput(
                inputId = "uip_filling_level"
                , label = "medium filling level [cm]:"
                , value = 0.88
                , min = 0.01
                , max = 2
                , step = 0.01
            )
        ),## End of tabPanel 'Cell Culture Dish'
            shiny::tabPanel(title = "Cells",
            shiny::numericInput(
                inputId = "uip_cell_growth_height"
                , label = "height of cells growth at the walls [cm]:"
                , value = 0.54
                , min = 0.01
                , max = 2
                , step = 0.01
            )
        ),## End of tabPanel 'Cells'
            shiny::tabPanel(title = "Cell Culture Medium",
            shiny::numericInput(
                inputId = "uip_medium_density"
                , label = shiny::HTML(
                    paste(
                        "density [g cm",
                        shiny::tags$sup(-3)
                        ,"]:"
                    )
                )
            , value = 1.0037
            )
            , shiny::numericInput(
                inputId = "uip_medium_viscosity"
                , label = "medium viscosity [mPa s]:"
                , value = 0.725
            )
            , shiny::numericInput(
                inputId = "uip_temperature"
                , label = "temperature during incubation [\u00B0C]:"
                , value = 37
                , min = 1
                , max = 50
                , step = 1
            )
            , shiny::numericInput(
                inputId = "uip_temperature_NTA"
                , label = "temperature during particle characterisation (NTA) [\u00B0C]:"
                , value = 24.8
                , min = 0
                , max = 50
                , step = 0.1
            )
            , shiny::numericInput(
                inputId = "uip_medium_viscosity_NTA"
                , label = "medium viscosity during particle characterisation (NTA) [mPa s]:"
                , value = 0.893
                , max = 1
                , min = 0
                , step = 0.001
            )
    )## End of tabPanel 'Cell Culture Medium'
)
)## End of miniContentPanel

),## End of MiniTabstripPanel 'User Input'

    # Simulation Tab ----------------------------------------------------------

    miniUI::miniTabPanel("Simulation", icon = shiny::icon("cogs"),
        ### prior the simulation the data provided by the user will be promtet for verification
        miniUI::miniContentPanel(padding = 0,
            shiny::navlistPanel("Simulation Panel", id = "user_input_summary_tabs",well = F
                , shiny::tabPanel("Start new simulation", padding = 0,
                    shiny::h3("User Input summary - Check input"),
                    shiny::helpText("Simulation time depends on the number of particles that are used for the
                                    calculations and the incubation time."),
                    shiny::uiOutput("subset_table_1")
                )
                , shiny::tabPanel("Define Save File",
                    shiny::h3("Define the file where the simulation data will be saved")
                    , shinyFiles::shinySaveButton(
                        id = 'simulationSaveFile'
                        , 'Save file'
                        , 'Save file as...'
                        , filetype = base::list(RData='RData')
                        )
                    , shiny::verbatimTextOutput("save_path")
                )
            )
        )
        ####conditional panel only shows up when the 'Start new simulatiton' panel is selected
        ####Start Button Conditional Panel####
        , shiny::conditionalPanel(
            condition = "input.user_input_summary_tabs == 'Define Save File'",
            miniUI::miniButtonBlock(
                shiny::actionButton("startSimulation", "Start simulation ...")
            )

        )
    ),

    # Visualisation Tab  ------------------------------------------------------

miniUI::miniTabPanel("Visualisation"
    , icon = shiny::icon("area-chart"),
    miniUI::miniContentPanel(padding = 0
        , shiny::navlistPanel("Visualisation Panel", id = "user_input_summary_tabs",well = F
            , shiny::tabPanel("Load previous simulation",
                shiny::helpText("Using this section it is possible to load a previously created simulation and recreate the results and plots."),
    ### load the previously saved simulation
                shiny::fileInput(
                    inputId = "uip_simulation_data"
                    , label = "Select RData file of previous simulations"
                    , width = "300px"
                    , accept = c(
                        "*.RData"
                        )
                    , multiple = FALSE
                    , buttonLabel = "Select RData ..."
                )
                , shiny::HTML("<font color='red'><b>Do not proceed</b></font> unless the status bar prompts <b>'Upload complete'</b>!")
                , shiny::br()
                , shiny::helpText("(Because otherwise, no visuals will be created!)")
            )
        , shiny::tabPanel("Simulation Plots"
            , shiny::uiOutput("RData_check")
        )
    # shinySaveButton('save', 'Save', 'Save as...')
        )
    )
)

    # Interactive VisTab ------------------------------------------------------

, miniUI::miniTabPanel(
    title = "Simulation Data Visualisation",
    icon = shiny::icon("chart-bar"),
    ## setting the layout
    miniUI::miniContentPanel(
        padding = 0
        , shiny::navlistPanel(
            "Interactive Visualisation Panel"
            , id = "uip_interactive_visualisation_panel"
            , well = FALSE
            # defining the panel header
            , shiny::tabPanel(
                title = "Loading Simulation Data for Visualisation"
                ## help text for explanation
                , shiny::helpText("Use this section to load archived simulation data (stored in .RData files) of previous particle simulation in order to visualise interactively certain timepoints.")
                , shiny::fileInput(
                    inputId = "SimulationData"
                    , label = ""
                    , accept = "RData"
                )
                ## this panel only shows up when there has been an .RData file loaded
                , shiny::conditionalPanel(
                    condition = "output.SimulationData == true"
                    , shiny::actionButton(
                        inputId="simulationsettings"
                        , label = "Load settings"
                        )
                    , shiny::tableOutput("SimulationSettings")
                )
            )## end of 'tabPanel'
            ## here comes the next 'tabPanel' for visualisation settings
            , shiny::tabPanel(
                title = "Visualisation Tab"
                , shiny::uiOutput("visSlider_time_point")
                , shiny::uiOutput("")
            )
        )## end of 'navlistPanel'
    )## end of 'miniContentPanel'
)

    # Credit Tab --------------------------------------------------------------


, miniUI::miniTabPanel("Credits"
, icon = shiny::icon("rebel"),
    miniUI::miniContentPanel(
        shiny::HTML("This Shiny-App was written by Dr. Falko Frenzel and is based on the Master Thesis of Laura K\u00f6nig.")
        , shiny::HTML("Special Thanks to beta-Testers: Dr. Linda B\u00f6hmert, Linn Voss & Valerie Stock")
        , shiny::tags$hr()
        , shiny::h3("Package References")
        , shiny::tableOutput("citations")
         # shiny::uiOutput("markdownReferences")
    )
)
)
)

#fileinfo <- ""
simulation_data <- base::list()


server <- function(input, output, session) {
    library(tidyverse)
    library(data.table)

    ### getting the citations
    output$markdownReferences <- shiny::renderUI({
        shiny::HTML(markdown::markdownToHTML(knitr::knit("rmd/references.Rmd", quiet = TRUE)))
    })
    ###
    ###
    output$citations <-
        shiny::renderTable(
            hover = TRUE
            , {
            c(
                {citation("shiny") %>% unlist %>% paste0(collapse = " ")},
                {citation("shinyFiles") %>% unlist %>% paste0(collapse = " ")},
                {citation("miniUI") %>% unlist %>% paste0(collapse = " ")},
                # {citation("leaflet") %>% unlist %>% paste0(collapse = " ")},
                {citation("ggplot2") %>% unlist %>% paste0(collapse = " ")},
                {citation("rgl") %>% unlist %>% paste0(collapse = " ")},
                {citation("tidyverse") %>% unlist %>% paste0(collapse = " ")},
                {citation("plot3D") %>% unlist %>% paste0(collapse = " ")},
                {citation("data.table") %>% unlist %>% paste0(collapse = " ")},
                # {citation("imager") %>% unlist %>% paste0(collapse = " ")},
                {citation("scatterplot3d") %>% unlist %>% paste0(collapse = " ")},
                {citation("Cairo") %>% unlist %>% paste0(collapse = " ")},
                {citation("BBmisc") %>% unlist %>% paste0(collapse = " ")},
                {citation("jsonlite") %>% unlist %>% paste0(collapse = " ")},
                {citation("plotly") %>% unlist %>% paste0(collapse = " ")},
                {citation("threejs") %>% unlist %>% paste0(collapse = " ")}
            ) %>%
                    data.table("Packages used within this Shiny Application" = .)
    })

    ### sum up of the user input provided on the first tabset
    ### prompt the provided flags
    output$uip_flag_settings <-
        shiny::renderText({
            stringr::str_c(
                "Simulation based on diffusion coefficient or diffusion coefficient distribution? <b>"
                , input$uip_flag_diffusion_coefficient
                ,"</b><br/>Simulation based on coefficient distribution or particle size distribution? <b>"
                , input$uip_flag_distribution
                , "</b></br>"
                , "Defined diffusion coefficient <b>"
                , input$uip_diffusion_coefficent
                , " nm<sup>2</sup> s<sup>-1</sup></b>"
            )
        })

    # Calculations that are necessary to operate the interactive visTab --------

    ## hide the tab that is useless unless an .RData file has been loaded
    shiny::hideTab(
        inputId = "uip_interactive_visualisation_panel"
        , target = "Visualisation Tab"
        )

    ## stop app if button 'Done' has been clicked
    shiny::observeEvent(
        input$quit
        ,{
        shiny::showNotification(
            paste("Application has been stopped ... ")
            , type = "error"
            , duration = 10
            )
        shiny::stopApp(TRUE)
        }
    )

    ## Check if there has been a file uploaded or not
    output$SimulationData <-
        shiny::reactive({
            base::return(!base::is.null(input$SimulationData))
        })

    ## define the output setting for the 'SimulationData'
    shiny::outputOptions(
        output
        , "SimulationData"
        , suspendWhenHidden = FALSE
        )

    ## of the button 'Show setting' has been clicked; table will be generated here
    shiny::observeEvent(input$simulationsettings,{
        if ( base::is.null(input$SimulationData)) base::return(NULL)
        inFile <- input$SimulationData
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = base::new.env()
        name <- base::load(file, envir = e)
        data <- e[[name]]

        delta_snapshot <-
            {data$simulation_settings$`data snapshot time [delta h]`}
        SimulationSettings <-
            data$simulation_settings %>%
            base::unlist() %>%
            base::t() %>%
            data.table::data.table() %>%
            base::t() %>%
            base::data.frame(
            "settings" = base::row.names(.)
            , "values" = .[,1]
            ) %>%
            dplyr::select_(
            .dots = c("settings", "values")
            ) %>%
            data.table::data.table()
            simulation_period <- {
                data$simulation_settings$`simulation time`
            }

    # Plot the data
    output$SimulationSettings <-
        shiny::renderTable(
            hover = TRUE
            ,{
                SimulationSettings
            }
            )

    ## show the new tabPanel for further settings
    shiny::showTab(
        inputId = "uip_interactive_visualisation_panel"
        , target = "Visualisation Tab"
        )

    ## create the slider ui for the VisTab
    output$visSlider_time_point <-
        shiny::renderUI({
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::sliderInput(
                        inputId = "simulation_time_point"
                        , label = "Set Simulation Timepoint [h]"
                        , min = delta_snapshot
                        , max = {data$Output %>% length()} * {delta_snapshot}
                        , value = 12
                        , step = 0.05
                    )
                    , shiny::tags$hr()
                    , shiny::p("Use the mouse zoom to rotate, zoom, and pan.")
                    , shiny::p("(ATTENTION: due to massive number of particles the simulation might be slow and not that responsive as you might wish. Please be patient.)"
                               )
                    , shiny::checkboxInput(
                        inputId = "particles_switch"
                        , label = "Show floating particles?"
                        , value = TRUE
                    )
                    , shiny::checkboxInput(
                        inputId = "bottom_hit_switch"
                        , label = "Show particles that hit the ground?"
                        , value = TRUE
                    )
                    , shiny::checkboxInput(
                        inputId = "wall_hit_switch"
                        , label = "Show particles that hit the wall?"
                        , value = TRUE
                    )
                    , shiny::sliderInput(
                        inputId = "surface_density"
                        , label = "Select the smoothnes of the surface plots"
                        , min = 5
                        , max = 50
                        , value = 25
                    )
                    )
                , shiny::mainPanel(
                shiny::h3(
                    shiny::textOutput("current_simulation_time_point")
                    )
                , threejs::scatterplotThreeOutput(outputId = "scatterplot", height = "400px")
                , shiny::tags$hr()
                , shiny::fluidRow(
                shiny::splitLayout(
                    cellWidths = c("50%", "50%")
                    , plotly::plotlyOutput(outputId = "bottom_hits", height = "300px")
                    , plotly::plotlyOutput(outputId = "wall_hits", height = "300px")
                    )
                )
                )
                )
            }
            )

    ## calculate the scatter plot with 'threejs'
    output$scatterplot <-
        threejs::renderScatterplotThree({
            data.table::data.table() %>%
            base::rbind(
                if(input$particles_switch){
                    data$Output[[input$simulation_time_point/delta_snapshot]]$Vcart %>%
                    data.table::data.table() %>%
                    .[z != -10000000000]
                    }
                ) %>%
            base::rbind(
                if(input$bottom_hit_switch){
                    data$Output[[input$simulation_time_point/delta_snapshot]]$Bottom_count %>%
                    polar2cart_m() %>%
                    data.table::data.table() %>%
                    data.table::setnames(
                    old = c("V1","V2","V3")
                    , new = c("x","y","z")
                    ) %>%
                    .[!is.na(x)]
                    }
                ) %>%
            base::rbind(
                if(input$wall_hit_switch){
                    data$Output[[input$simulation_time_point/delta_snapshot]]$Wall_count %>%
                    polar2cart_m() %>%
                    data.table::data.table() %>%
                    data.table::setnames(
                    old = c("V1","V2","V3")
                    , new = c("x","y","z")
                    ) %>%
                    .[!is.na(x)]
                    }
                ) %>%
            BBmisc::normalize(
                method = "range"
                , range = c(0,1)
                , margin = 2L
                ) %>%
            base::as.matrix() %>%
            threejs::scatterplot3js(
                size = 0.1
                , bg = "white"
                , color = base::rep("red", length.out = nrow(.))
                , grid = FALSE
                , pch = "."
                , lcol = "#FF3200"
                , xlim = c(0,1)
                , ylim = c(0,1)
                , zlim = c(0,1)
                , num.ticks = NULL
                )
            })

    ## render text to display the current simulation time point
    output$current_simulation_time_point <-
        shiny::renderText({
            base::paste("Current time point:\n"
            , input$simulation_time_point
            , " h in a "
            , simulation_period
            , " h simulation period.")
            })

    ## calculation of the surface plot depicting the bottom hits
    output$bottom_hits <-
        plotly::renderPlotly({
            x_surface_bottom <-
                data$Output[[input$simulation_time_point/delta_snapshot]]$Bottom_count %>%
                polar2cart_m() %>%
                .[,1]

            y_surface_bottom <-
                data$Output[[input$simulation_time_point/delta_snapshot]]$Bottom_count %>%
                polar2cart_m() %>%
                .[,2]

            x_c_surface_bottom <- base::cut(x_surface_bottom, input$surface_density)
            y_c_surface_bottom <- base::cut(y_surface_bottom, input$surface_density)

            z_surface_bottom <- base::table(x_c_surface_bottom, y_c_surface_bottom)

            plotly::plot_ly(
                z = z_surface_bottom
                , color = I("red")
                ) %>%
                plotly::add_surface() %>%
                plotly::layout(
                    base::list(
                        title = "Bottom Hits"
                        )
                    )
            })

    ## calculation of the surface plot depicting the wall hits
    output$wall_hits <-
        plotly::renderPlotly({
            x_surface_wall <-
                data$Output[[input$simulation_time_point/delta_snapshot]]$Wall_count %>%
                polar2cart_m() %>%
                .[,1]
            y_surface_wall <-
                data$Output[[input$simulation_time_point/delta_snapshot]]$Wall_count %>%
                polar2cart_m() %>%
                .[,3]

            x_c_surface_wall <- base::cut(x_surface_wall, input$surface_density)
            y_c_surface_wall <- base::cut(y_surface_wall, input$surface_density)

            z_surface_wall <- base::table(x_c_surface_wall, y_c_surface_wall)
            plotly::plot_ly(
            z = z_surface_wall
            , color = I("blue")
            ) %>%
            plotly::add_surface() %>%
            plotly::layout(
            base::list(
                title = "Wall Hits"
                )
            )}
            )
    })

    ### give an overview of the uploaded CSV file that represents the distribution
    output$distribution_table <-
        shiny::renderDataTable(
            options = base::list(
                dom = "Blfrtip"
                , buttons = base::list(
                    "copy"
                    , base::list(
                        extend = "collection"
                        , buttons = base::c("csv", "excel", "pdf")
                        , text = "Download"
                        )
                    ) # end of buttons customization
                # customize the length menu
                , lengthMenu = base::list( c(10, 20, -1) # declare values
                , c(10, 20, "All") # declare titles
                ) # end of lengthMenu customization
                , pageLength = 10
            ),{
            inFile <- input$uip_example_distribution_path

            # distribution_table <-
            utils::read.delim(
                file = inFile$datapath
                , sep = ";"
                , stringsAsFactors = F
                )
            })

    #### summary for the particle characteristics ####
    output$uip_particle_characteristics <-
        shiny::renderText({
            stringr::str_c(
            "Particle Number for Simulation set to: <b>"
            , input$uip_particle_number
            , "</b></br>"
            , "Simulation time set to: <b>"
            , input$uip_simulation_time
            , " h</b></br>"
            , "Defined Time for taking a Snapshot during the Simulation: <b>"
            , input$uip_snap_shot
            , "</b></br>"
            , "Particles effective Density: <b>"
            , input$uip_effective_density
            , " g cm"
            , "<sup>-3</sup>"
            , "</b></br>"
            , "Particles hydrodynamic Diameter: <b>"
            , input$uip_hydr_diam
            , " nm</b></br>"
            )
            })

    #### summary for the cell culture settings ####
    output$uip_culture_dish <-
        shiny::renderText({
            stringr::str_c(
                "Dish Area: <b>"
                , input$uip_bottom_area
                , " cm<sup>2</sup> </b></br>"
                , "Medium Filling Level: <b>"
                , input$uip_filling_level
                , " cm</b></br>"
                )
            })

    #### summary for the cell settings ####
    output$uip_cells <-
        shiny::renderText({
            stringr::str_c(
                "Height the cells grow at the wells walls: <b>"
                , input$uip_cell_growth_height
                , " cm</b></br>"
                )
            })

    #### summary for the cell culture medium settings ####
    output$uip_cell_culture_medium <-
        shiny::renderText({
            stringr::str_c(
                "Cell culture medium density: <b>"
                , input$uip_medium_density
                , " g cm<sup>-3</sup></b></br>"
                , "Cell culture medium viscosity: <b>"
                , input$uip_medium_viscosity
                , " mPa s</b></br>"
                , "Medium temperature during incubation: <b>"
                , input$uip_temperature
                , " \u00B0C</b></br>"
                , "Temperature during the particle characterisation using the NTA method: <b>"
                , input$uip_temperature_NTA
                , " \u00B0C</b></br>"
                , "Medium viscosity during the particle characterisation using the NTA method: <b>"
                , input$uip_medium_viscosity_NTA
                , " mPa s</b></br>"
                )
            })

    ### combine the user input in order to let it be controlled by the user and
    ### in also to have it within a nice layout ###
    output$subset_table_1 <-
        shiny::renderUI({
            shiny::fluidRow(
                shiny::navlistPanel(well = F,
                    shiny::tabPanel(
                        title = "Calculation Method",
                        shiny::htmlOutput("uip_flag_settings"),
                        shiny::h4("Content of selected CSV file:"),
                        shiny::column(
                            width = 6
                            ,shiny::dataTableOutput("distribution_table")
                            )
                        ),
                    shiny::tabPanel(
                        title = "Nanoparticle Characterisics",
                        shiny::htmlOutput("uip_particle_characteristics")
                        ),
                    shiny::tabPanel(
                        title = "Cell Culture Dish",
                        shiny::htmlOutput("uip_culture_dish")
                        ),
                    shiny::tabPanel(
                        title = "Cells",
                        shiny::htmlOutput("uip_cells")
                        ),
                    shiny::tabPanel(
                        title = "Cell Culture Medium",
                        shiny::htmlOutput("uip_cell_culture_medium")
                        )
                    )
                )
            })

    #### check if there was an RData file provided ####
    ### else: give warning and guide the user back to the previous panel
    output$RData_check <-
        shiny::renderUI({
            if(is.null(input$uip_simulation_data)){
                shiny::modalDialog(title = "No RData file with a previous simulation has been provided to the app. Please provide an RData file or consider rerunning the simulation in the previous tab.")
                # input$uip_simulation_data <- NULL
            }else{
                RData_file <- input$uip_simulation_data

                load(
                    file = RData_file$datapath
                )

        ### create the dosage-over-time plot
        ranges <- shiny::reactiveValues(x = NULL, y = NULL)

        output$dose_over_time_plot_original <-
            shiny::renderPlot({

                ### ggplot code for the dosage over time plot
                simulation_data$sedimentation %>%
                data.table::data.table() %>%
                .[,"total" := .$`dose wall` + .$`dose bottom`] %>%
                data.table::melt.data.table(id.vars = "hour") %>%
                data.table::setnames("variable" , "dose") %>%
                {ggplot2::ggplot(mapping = ggplot2::aes_(
                    x = .$hour
                    , y = .$value
                    , colour = .$dose
                    )
                    ) +
                        ggplot2::geom_line() +
                        ggplot2::geom_point() +
                        ggplot2::theme_minimal() +
                        ggplot2::xlab("incubation time/simulation time [h]") +
                        ggplot2::ylab("dose [%]") +
                        ggplot2::theme(
                            legend.text = ggplot2::element_text(size = 18)
                            , legend.title = ggplot2::element_text(size = 20)
                            , axis.title = ggplot2::element_text(size = 18)
                            , axis.text = ggplot2::element_text(size = 16)
                            )}
                })

    output$dose_over_time_plot_zoom <-
        shiny::renderPlot({

            ### ggplot code for the dosage over time plot for zoom purpose
            simulation_data$sedimentation %>%
            data.table::data.table() %>%
            .[,"total" := .$`dose wall` + .$`dose bottom`] %>%
            data.table::melt.data.table(id.vars = "hour") %>%
            data.table::setnames("variable" , "dose") %>%
            {ggplot2::ggplot(mapping = ggplot2::aes_(
                x = .$hour
                , y = .$value
                , colour = .$dose
                )
                ) +
                    ggplot2::geom_line() +
                    ggplot2::geom_point() +
                    ggplot2::theme_minimal() +
                    ggplot2::xlab("incubation time/simulation time [h]") +
                    ggplot2::ylab("dose [%]") +
                    ggplot2::theme(
                        legend.text = ggplot2::element_text(size = 18)
                        , legend.title = ggplot2::element_text(size = 20)
                        , axis.title = ggplot2::element_text(size = 18)
                        , axis.text = ggplot2::element_text(size = 16)
                        ) +
                    ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                }
            })

    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    shiny::observe({
        brush <- input$plot2_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            } else {
                ranges$x <- NULL
                ranges$y <- NULL
                }
        })

    ### create the 3D histogram for the bottom of the well
    output$bottom_hist_plot <-
        shiny::renderPlot({
            index_bottom <- {
                simulation_data$Output %>% length()}
            x <- simulation_data$Output[[index_bottom]]$Bottom_count %>% polar2cart_m() %>% .[,1]
            y <- simulation_data$Output[[index_bottom]]$Bottom_count %>% polar2cart_m() %>% .[,2]

            x_c <- cut(x, 20)
            y_c <- cut(y, 20)

            z <- table(x_c, y_c)

            plot3D::hist3D(
                z = z
                , border = "black"
                , phi = 45
                , theta = 45
                , main = "Bottom hits @ End of Simulation"
                )
            })

    ### create the 3D histogram for the wall hits of the well
    output$wall_hist_plot <-
        shiny::renderPlot({
            index_wall <- {simulation_data$Output %>% length()}
            x <- simulation_data$Output[[index_wall]]$Wall_count %>% .[,1]
            y <- simulation_data$Output[[index_wall]]$Wall_count %>% .[,3]

            x_c <- cut(x, 20)
            y_c <- cut(y, 20)

            z <- table(x_c, y_c)

            plot3D::hist3D(
                z = z
                , border = "black"
                , phi = 45
                , theta = 45
                , main = "Wall hits @ End of Simulation"
                )
        })
    }# end of the 'else' statement at the beginning of the 'RData_check' output

    shiny::fillRow(flex = c(3,2),
        shiny::fluidRow(
            shiny::column(width = 11, class = "well",
                shiny::h4("Dose over Time Plot"),
                shiny::plotOutput("dose_over_time_plot_original", height = 350,
                    brush = shiny::brushOpts(
                        id = "plot2_brush",
                        resetOnNew = TRUE
                        )
                    ),
                shiny::plotOutput(
                    "dose_over_time_plot_zoom"
                    , height = 350
                    , hover = shiny::hoverOpts(id="plot_hover")
                    )
                , shiny::uiOutput("my_tooltip")
                )
            ),
        shiny::fluidRow(
            shiny::column(width = 8, class = "well",
                shiny::h4("3D Histogram for Wall/Bottom Hits"),
                shiny::plotOutput("wall_hist_plot", height = 350),
                shiny::plotOutput("bottom_hist_plot", height = 350)
                )
            )
        ) # end of the 1st 'fillRow' statement
    }) # end of the render UI for the 'RData_check' section

    shiny::observe(
        x = {
            shinyFiles::shinyFileSave(
                input
                , 'simulationSaveFile'
                , roots=c(wd = stringr::str_c(getwd(), "/.."))
                , session=session
                )
            }
        , label = "storage_file_creator"
    )

    ####Simulation Start button action definition####
    shiny::observeEvent(
        input$startSimulation
        ,{
            shiny::showModal(shiny::modalDialog(
                title = "Simulation started"
                , "This might take a while ..."
                , easyClose = TRUE
                , size = "m"
                )
            )

    ####definitions for the simulation####
    ### This script defines the function for 'distribution_processing'.
    ### The input for this function is defined and given by the main script
    ### called 'Three_DSDD.R'
        distribution_processing <-
            function(
                uip_excel_table_path = uip_excel_table_path
                , uip_N = ~p_N
            ){
                ## read in the sample distribution
                Sample_Distribution <-
                    utils::read.csv2(
                        file = uip_excel_table_path
                        , header = TRUE
                        , stringsAsFactors = FALSE
                        ) %>%
                    sapply(as.numeric) %>%
                    data.table::data.table()

                ## Clean the input data, because there are only two columns
                ## allowed: "Diffusion.Coefficient" "example.distribution"
                ## The first one needs to be the "Diffusion Coefficient",
                ## the second one has to be the "Distribution".
                ## The names of the columns are not strict but the order _is_!!!

                if(ncol(Sample_Distribution) > 2){
                  crayon::bgRed("Check the input style of your distribution CSV file!\nThere are more than two columns ...") %>%
                    crayon::white() %>%
                    cat("\n")
                  stop("
There is an issue with the provided distribution data csv ...\n
Maybe you exported row names while writing the csv file.\n
Those might be misinterpreted as a third column ... ?!")
                }


                ## load diffusion coefficient/particles size
                D_value_from_table <-
                    Sample_Distribution %>%
                    .[,1]

                ## read in the values for the cumulative distribution
                ## load the values for cummulative distribution
                D_cumsum_from_table <-
                    Sample_Distribution %>%
                    .[,2]

                ## find the values that are larger than 0 in the cumulative
                ## distribution getting the non-zero elements from the
                ## distribution
                D_cumsum <-
                    Sample_Distribution[
                        Sample_Distribution[[2]] > 0
                        ][,2]

                ## getting the non-zero elements diffusion coefficents from
                ## the list
                D_value <-
                    Sample_Distribution[
                        Sample_Distribution[[2]] > 0
                        ][,1]

                ## multiply the cumulative coefficient with the number of
                ## given particles
                D_cumsum_N <-
                    round(
                        D_cumsum * uip_N
                        )

                ## loop over the elements
                Distribution <- c()
                for(ELEMENT in seq_along(D_cumsum_N[[1]])){
                    if(ELEMENT == 1){
                        Distribution[D_cumsum_N[[1]][ELEMENT]] <-
                            D_value[[1]][ELEMENT]
                    }
                    if(ELEMENT == length(D_cumsum_N[[1]])){
                        Distribution[{D_cumsum_N[[1]][ELEMENT-1]+1}:uip_N] <-
                            D_value[[1]][D_cumsum_N == uip_N][1]
                        if(D_cumsum_N[[1]][ELEMENT-1] == uip_N){
                            break
                        }
                        }
                    if(ELEMENT>1 && ELEMENT < length(D_cumsum_N[[1]])){
                        Distribution[{D_cumsum_N[[1]][ELEMENT-1]}:D_cumsum_N[[1]][ELEMENT]+1] <-
                            D_value[[1]][ELEMENT]
                    }
                    }

                    ## return the generated distribution based on the given
                    ## input
                    return(Distribution[1:uip_N])
                }

    ####helper function for converting cartesian coordinates in a cylindrical
    #### polar coordinatesystem
    cart2polar <- function(x,y,z){
        rho <- sqrt(x**2+y**2)

        if(x == 0 && y == 0){
            phi <- 0
            }
        if(x >= 0){
            phi <- asin(y/rho)
            }
        if(x > 0){
            phi <- atan(y/x)
            }
        if(x < 0){
            phi <- -(asin(y/rho)+pi)
            }
        z <- z
        return(matrix(c(phi, rho, z), ncol=3, byrow = FALSE))
        }

    ####helper function for converting the cartesian coordinates to cylindrical
    #### polar coordinates - this function can be applied to be applied to
    #### matrices
    cart2polar_m <- function(x){
        rho <- sqrt(x[1]**2+x[2]**2)

        ## calculation of the angle phi
        if(x[1] == 0 && x[2] == 0){
            phi <- 0
            }
        if(x[1] >= 0){
            phi <- asin(x[2]/{sqrt(x[1]**2+x[2]**2)})
            }
        if(x[1] > 0){
            phi <- atan(x[2]/x[1])
            }
        if(x[1] < 0){
            phi <- -(asin(x[2]/{sqrt(x[1]**2+x[2]**2)})+pi)
            }
        z <- x[3]
        return(c(phi, rho, z))
        }

    ####revert function: cylindircal polar to cartesian coordinates####
    polar2cart <- function(phi, rho, z){
        x <- rho*cos(phi)
        y <- rho*sin(phi)
        z <- z
        return(
            matrix(data = c(x,y,z), ncol = 3)
            )
        }

    polar2cart_m <- function(m){
        x <- m[,2]*cos(m[,1])
        y <- m[,2]*sin(m[,1])
        z <-
            tryCatch(
                m[,3]
                , error = function(x) return(rep(0, times = {m %>% nrow()}))
                )
        return(matrix(data = c(x,y,z), ncol = 3))
        }


    add_step <- function(
        ## this function calculates the particles moving from one position to
        ## the next one - for every single particle individually
        ## function variables
        N                 ## particle number
        , V               ## matrix with coordinates of the particle
        , Wall_count      ## matrix with coordinates of the hits on the wall
        , Bottom_count    ## matrix with coordinates of the hits on the bottom
        , tau             ## time step in seconds [s]
        , count_bottom    ## counter for bottom hits
        , count_wall      ## counter for wall hits
        , p_Distribution  ## scalar or matrix containing the diffusion coefficient(s)
        , p_radius        ## radius of the well
        , p_cell_growth_heigth ## height the cells grow at the well walls
        , p_filling_level ## hight of the medium filling level
        , v_s             ## particles sedimentation velocity
        ){
            ####setting the column names####
            pol_names <- c("phi", "rho", "z")
            cart_names <- c("x", "y", "z")

            #### function content ####

            ## choosing a place long outside the cylinder
            cart_a <- -1e+10
            cart_b <- -1e+10
            cart_c <- -1e+10

            ## convert to cylinder coordinates
            polar <-
                cart2polar(
                    x = cart_a
                    , y = cart_b
                    , z = cart_c
                )
            pol_a <- polar[1,1]
            pol_b <- polar[1,2]
            pol_c <- polar[1,3]

            ## calculation of a random distribution
            Vcart <-
                polar2cart(V[,1], V[,2], V[,3])

            dimnames(Vcart) <- list(1:nrow(V), cart_names)

            ## creation of the random motion of the particles in the cartesian coordinate
            ## system
            RandMotionInCart <-
                matrix(
                    rnorm(3*N)
                    , nrow = 3
                    , byrow = T
                    )

            ## calculation of the traveled distance of the given particles
            x <- sqrt(2*p_Distribution*tau)*RandMotionInCart[1,]
            y <- sqrt(2*p_Distribution*tau)*RandMotionInCart[2,]
            z <- sqrt(2*p_Distribution*tau)*RandMotionInCart[3,] - v_s*tau

            XYZ <-
                matrix(
                    data = c(x,y,z)
                    , ncol = 3
                    , byrow = FALSE
                    , dimnames = list(1:length(x), cart_names)
                    )

            ## particles shoud not moven when cart_a, cart_b, cart_c (outside)
            ## if the particles are inside -> TRUE -> 1
            ## if the particles are outside -> FALSE -> 0
            InsideBox <-
                !(Vcart[,1] == cart_a | Vcart[,2] == cart_b | Vcart[,3] == cart_c)

            InsideBox <-
                matrix(
                    data = c(InsideBox,InsideBox,InsideBox)
                    , ncol = 3
                    , byrow = FALSE
                    )

            XYZ = XYZ * InsideBox

            Vcart = Vcart + XYZ

            ####convert the new cartesian coordinates to polar coordinates to check for wall and bottom hits####
            V <-
                t(
                    apply(Vcart, 1, cart2polar_m)
                )

            dimnames(V) <- list(1:nrow(V), pol_names)

            ####checking for particles that are located outside the zylinder (those particles will be replaced back in the cylinder; particles at cart_a, cart_b and cart_c shall not be replaced! )####
            #### find particles outside the cylinder wall ...
            wall_indexes <-
                {
                    V[,2] > p_radius & V[,2] < pol_b
                }
            #### ... and place them back inside
            V[wall_indexes, 2] <- p_radius

            ### find particles exceeding the top and bottom area of the cylinder ...
            top_indexes <-
                {V[,3] > p_filling_level}
                ### ... and place them back inside
            V[top_indexes,3] <- p_filling_level - z[top_indexes]

            ### find particles that do exceed the bottom area ...
            bottom_index <-
                {V[,3] <= 0 & V[,3] > pol_c}
            ### ... and raplace them
            V[bottom_index,3] <-  0

            ### now count those particles that have hit the bottom
            bottom_hit <-V[,3] == 0
            bottom_Start <- count_bottom + 1
            count_bottom <- count_bottom + sum(bottom_hit)
            bottom_End <- count_bottom

            Bottom_count_i <-
                matrix(
                    data = c(
                        V[bottom_hit,1]
                        , V[bottom_hit,2]
                        )
                    , ncol = 2
                    , byrow = FALSE
                    )

            if(nrow(Bottom_count_i)>0){
                dimnames(Bottom_count_i) <-
                    list(1:nrow(Bottom_count_i), pol_names[1:2])
            }

            Bottom_count <-
                rbind(
                    Bottom_count
                    , Bottom_count_i
                    )
    ### place those particles outside the controled volume
    V[bottom_hit,1] <- pol_a
    V[bottom_hit,2] <- pol_b
    V[bottom_hit,3] <- pol_c

    ## counting the wall hits
    wall_hit <-
    {
            V[,2] == p_radius & V[,3] <= p_cell_growth_heigth & V[,3] >= 0
        }
    wall_start <- count_wall + 1
    count_wall <- count_wall + sum(wall_hit)
    wall_end <- count_wall

    Wall_count_i <-
        matrix(
            data = c(
                V[wall_hit, 1]
                , V[wall_hit, 2]
                , V[wall_hit, 3]
                )
            , ncol = 3
            , byrow = FALSE
            )

    if(nrow(Wall_count_i) > 0){
        dimnames(Wall_count_i) <-
            list(
                1:nrow(Wall_count_i)
                , pol_names
            )
        }

    Wall_count <-
        rbind(
            Wall_count
            , Wall_count_i
            )

    V[wall_hit, 1] <- pol_a
    V[wall_hit, 2] <- pol_b
    V[wall_hit, 3] <- pol_c

    ####setting the dimension names for the matrices####
    dimnames(V) <- list(c(1:nrow(V)), pol_names)
    dimnames(Vcart) <- list(c(1:nrow(Vcart)), cart_names)
    if(nrow(Bottom_count) > 0){
        dimnames(Bottom_count) <-
            list(
                c(
                    1:nrow(Bottom_count)
                    )
                , pol_names[1:2]
                )
        }
    if(nrow(Wall_count) > 0){
        dimnames(Wall_count) <-
            list(
                c(
                    1:nrow(Wall_count)
                    )
                , pol_names
                )
        }

    ####return the generated values####
    return(
            list(
                "V" = V %>% as.data.frame()
                , "Vcart" = Vcart %>% as.data.frame()
                , "Wall_count" = Wall_count %>% as.data.frame()
                , "Bottom_count" = Bottom_count %>% as.data.frame()
                , "count_bottom" = count_bottom
                , "count_wall" = count_wall
                )
            )
    }

    # Main Function Script ----------------------------------------------------
    ## Within this script the main function is defined and can be seen as a head of
    ## the programm.
    ## In later stages of the software development - the Shiny Web Applett will be
    ## kept in here as well.

    ## A lot of user input is requested in order to run the simulation.
    ## All the requested user input does have a default value.

    Three_DSDD <-
        function( ## User Input section
            uip_N =  input$uip_particle_number                         ## particle number
            , uip_bottom_area = input$uip_bottom_area                  ## dish bottom area [cm2]
            , uip_filling_level = input$uip_filling_level              ## fill level of medium [cm]
            , uip_cell_growth_height = input$uip_cell_growth_height    ## height the cells grow [cm]
            , uip_temperature = input$uip_temperature                  ## temperature during incubation [\uc2b0C]
            , uip_effective_density = input$uip_effective_density      ## effective density [g cm-3]
            , uip_medium_density = input$uip_medium_density            ## medium density [g cm-3]
            , uip_medium_viscosity = input$uip_medium_viscosity        ## viscosity [mPa s]
            , uip_simulation_time = input$uip_simulation_time          ## simulation time [h]
            , uip_temperature_NTA = input$uip_temperature_NTA          ## temperature during NTA measurement [\uc2b0C]
            , uip_medium_viscosity_NTA = input$uip_medium_viscosity_NTA
            ## [mPa s]
            , uip_diffusion_coefficient = input$uip_diffusion_coefficent ## diffusion coefficient [nm2 s-1]
            , uip_hydro_diameter = input$uip_hydr_diam                                  ## hydrodynamic diameter of the nano
            ## particle [nm]
            , uip_flag_diffusion_coefficient = input$uip_flag_diffusion_coefficient
            , uip_flag_distribution = input$uip_flag_distribution
            , uip_accelleration = 9.80665

            ## Reading in the data from a given example distribution is available too.
            ## Therefore the data can be given in a CSV file or EXCEL spread sheet.
            # , uip_example_distribution_path = input$uip_example_distribution_path$datapath  ## defines the path to the spread sheet
            , uip_excel_table_path = as.character(input$uip_example_distribution_path$datapath)  ## defines the path to the spread sheet
            ## like table with two columns:
            ##    1. Diffusion coefficent [nm2 s-1]
            ##    2. percentage of distribution (?)
            , uip_snap_shot = input$uip_snap_shot                   ##  defines the portion of an hour
            ##  at which a data snapshot shall be
            ##  taken (value between 0 and 1)
            , uip_simulation_data_file = "./simulation_data.RData"
            ){
                ## Next section focuses on processing the user input to a nanometer based scale
                p_N <- uip_N
                p_alpha <- 2*pi                     ##
                p_area <- uip_bottom_area*1e+14     ## bottom area [nm2]
                p_radius <- sqrt(p_area/pi)         ## radius of the bottom [nm]
                p_filling_level <- uip_filling_level * 1e+7
                ## height of the medium level [nm]
                p_cell_growth_heigth <- uip_cell_growth_height * 1e+7
                ## height of the cell growth [nm]
                p_cell_size <- 18.1e+3              ## cell size [nm] (used for time step adjustment)
                p_density <- uip_effective_density * 1e-24
                ## effective density [kg nm-3]
                p_temperature <- 273.15 + uip_temperature
                ## incubation temperature [K]
                p_k_Boltzmann <- 1.380568e-5        ## Boltzmann constant [kg nm2 s-2 K]
                p_medium_density <- uip_medium_density * 1e-24
                ## media density [kg nm-3]
                p_accelleration <- uip_accelleration * 1e+9       ## acceleration of gravity [nm s-2]
                p_medium_viscosity <- uip_medium_viscosity * 1e-12
                ## media viscosity [kg nm-1 s-1]
                ## (default: water)

                ## With the use of flags the user can choose which parameter to input.
                ## Because there might be some available and others don't.
                p_flag_diffusion_coefficient <- as.logical(uip_flag_diffusion_coefficient)
                ## options are:
                ## 0 -  input particle size or particle size
                ##      distribution
                ## 1 -  input diffusion coefficient or
                ##      diffusion coefficient distribution
                p_flag_distribution <- as.logical(uip_flag_distribution)
                ## options are:
                ## 0 -  use average value
                ## 1 -  use diffusion coefficient distribution
                ##      distribution or particle size
                ##      distribution from given spread sheet

                #####calculating the parameters based on given measurements####
                ## Evaluate the flags and calculate the parameters.
                if(p_flag_diffusion_coefficient == 1 && p_flag_distribution == 1){
                    print(
                        "OPTION I: Using diffusion coefficient distribution and particle size distribution provided by spread sheet ..."
                        )

                p_temperature_NTA <- 273.15 + uip_temperature_NTA
                ## temperature during the NTA measurement [K]
                p_medium_viscosity_NTA <- uip_medium_viscosity_NTA * 1e-12
                ## medium viscosity during NTA measurement
                ## [kg nm-1 s-1]

                ## loading the script with the function definition for the next step
                # source("scripts/001_distribution_processing.R")
                # uip_excel_table_path <- uip_example_distribution_path
                p_Distribution <-           ## generation of the diffusion coefficient matrix
                    distribution_processing(
                        uip_excel_table_path = uip_excel_table_path
                        , uip_N = p_N
                        )
                ## calculating the radius of the hydration hull
                hydr_radius <-
                    {p_k_Boltzmann*p_temperature_NTA}/{6*pi*p_medium_viscosity_NTA*p_Distribution} ##
                ## calculating the effective mass of the particles [kg]
                ## an effective mass larger than 0 leads to particle segmentation
                p_effective_mass <-
                    {4*pi*hydr_radius^3}*(p_density - p_medium_density)/{3}
                ## calculating the sedimentation velocity [nm/s]
                v_s <-
                    {p_effective_mass*p_accelleration*p_Distribution}/{p_k_Boltzmann*p_temperature}
                ## calculation of the particle diameter
                particle_diameter <-
                    mean(hydr_radius*2)
                } ## end of 'if' statment

                ####calculation based on average diffusion coefficient####
                if(p_flag_diffusion_coefficient == 1 && p_flag_distribution == 0){
                    print(
                        "OPTION II: Using diffusion coefficent distribution with an average value for particle size distribution ..."
                        )
                    p_Distribution <- uip_diffusion_coefficient
                    hydr_radius <-
                        {p_k_Boltzmann*p_temperature}/{6*pi*p_medium_viscosity*p_Distribution}
                    p_effective_mass <-
                        {4*pi*hydr_radius^3}*(p_density - p_medium_density)/{3}
                    v_s <-
                        {p_effective_mass*p_accelleration*p_Distribution}/{p_k_Boltzmann*p_temperature}
                    particle_diameter <-
                        mean(hydr_radius*2)
                    } ## end of the second 'if'-statement

                ####calculation of the particle size distribution####
                if(p_flag_diffusion_coefficient == 0 && p_flag_distribution == 1){
                    print(
                        "OPTION III: Using input for particle size and particle size distribution provided via spread sheet ..."
                        )
                    hydr_diam <- distribution_processing(
                        uip_excel_table_path = uip_excel_table_path
                    )
                    hydr_radius <-  hydr_diam/2
                    p_Distribution <-
                        {p_k_Boltzmann*p_temperature}/{6*pi*p_medium_viscosity*hydr_radius}
                    p_effective_mass <-
                        {4*pi*hydr_radius^3}*(p_density - p_medium_density)/{3}
                    v_s <-
                        {p_effective_mass*p_accelleration*p_Distribution}/{p_k_Boltzmann*p_temperature}
                    }## end of 'if'-statement

                ####calculation using the average particle size####
                if(p_flag_diffusion_coefficient == 0 && p_flag_distribution == 0){
                    print(
                        "OPTION IV: Using input values for particle size and average value for diffusion coefficient ..."
                        )
                    hydr_radius <- uip_hydro_diameter/2
                    p_Distribution <-
                        {p_k_Boltzmann*p_temperature}/{6*pi*p_medium_viscosity*hydr_radius}
                    p_effective_mass <-
                        {4*pi*hydr_radius^3}*(p_density - p_medium_density)/{3}
                    v_s <-
                        {p_effective_mass*p_accelleration*p_Distribution}/{p_k_Boltzmann*p_temperature}
                    }## end of 'if'-statement

                # Simulation Starts Here --------------------------------------------------

                #####initialising the simulation####
                simulation_time <- uip_simulation_time*3600 ## in seconds [s]
                tau <- 1  ## time step in [s]

                ####create a random distribution for the particles at time point tau == 0####
                V <- matrix(
                    data = c(
                        p_alpha * runif(p_N) ## angle rho
                        , sqrt(runif(p_N)) * p_radius ## euclidian distance phi
                        , p_filling_level * runif(p_N)) ## z
                        , ncol = 3 ## matrix with 3 columns (rho, phi, z)
                        , byrow = F ## filled columnwise
                        )

                ####initial counter for wall and bottom hits####
                count_wall <- 0
                count_bottom <- 0

                ####setting the column names####
                pol_names <- c("phi", "rho", "z")
                cart_names <- c("x", "y", "z")

                ####initial matrix for polar coordinates of wall hits####
                Wall_count <-
                    matrix(
                        ncol = 3
                        , dimnames = list(NA,pol_names)
                        )[-1,]

                ####initial matrix for polar coordinates of bottom hits####
                Bottom_count <-
                    matrix(
                        ncol = 2
                        , dimnames = list(NA,pol_names[1:2])
                        )[-1,]

                Input <-
                    list(
                        "V" = V
                        , "Wall_count" = Wall_count
                        , "Bottom_count" = Bottom_count
                        , "count_bottom" = count_bottom
                        , "count_wall" = count_wall
                        )

                times <-
                    seq(
                        from = {3600*uip_snap_shot}
                        , to = (uip_simulation_time*3600)
                        , by = {3600*uip_snap_shot}
                        )
                df <- data.frame()
                window <- 100

                Output <- list()

                # the actual simulation over time starts here -----------------------------

                init <- 1
                for(i in seq(simulation_time)){
                    message(stringr::str_c("Calculating simulation step ", i, " of a total simulation time of ", simulation_time, " seconds."))
                    Input <-
                        add_step(
                            N = p_N
                            , V = Input$V
                            , Wall_count = Input$Wall_count
                            , Bottom_count = Input$Bottom_count
                            , tau = tau
                            , count_bottom = Input$count_bottom
                            , count_wall = Input$count_wall
                            , p_Distribution = p_Distribution
                            , p_radius = p_radius
                            , p_cell_growth_heigth = p_cell_growth_heigth
                            , p_filling_level = p_filling_level
                            , v_s = v_s
                            )

                    if(i %in% times ){
                        message(
                          stringr::str_c(
                            "simulation running.\n",
                            round((i/simulation_time)*100)
                            ,"% completed."
                          )
                        )
                        shiny::showNotification(
                            ui = stringr::str_c(
                                "simulation running.\n",
                                round((i/simulation_time)*100)
                                ,"% completed."
                                )
                            , type = "message"
                            , duration = 60
                            )

                    df <-
                        base::rbind(
                            df,
                            c(
                                (i/3600)
                                , {Input$count_bottom * 100 / (p_N)}
                                , {Input$count_wall * 100 / (p_N)}
                                )
                            )

                    Output[[{which(times == i)}]] <- Input
                    Output[[{which(times == i)}]]$timepoint <- i*(uip_simulation_time)
                    }## end of the 'if'-statement to check
                    }## end of the sequential 'for'-loop

                names(df) <-
                    c("hour", "dose bottom", "dose wall")

                simulation_data <-
                    list(
                        "simulation_settings" = list(
                        "particle number" = uip_N
                        , "dish bottom area [cm\uc2b2]" = uip_bottom_area
                        , "filling level [cm]" = uip_filling_level
                        , "cell growth height [cm]" = uip_cell_growth_height
                        , "incubation temperature [\uc2b0C]" = uip_temperature
                        , "effective particel density [g cm\ue281bb\uc2b3]" = uip_effective_density
                        , "medium density [g cm\ue281bb\uc2b3]" = uip_medium_density
                        , "medium viscosity [mPa s]" = uip_medium_viscosity
                        , "simulation time [h]" = uip_simulation_time
                        , "data snapshot time [delta h]" = uip_snap_shot
                        , "NTA temperature [\uc2b0C]" = uip_temperature_NTA
                        , "NTA medium viscosity [mPa s]" = uip_medium_viscosity_NTA
                        , "diffusion coefficient [nm\uc2b2 s\ue281bb\uc2b9]" = uip_diffusion_coefficient
                        , "particles hydrodynamic diameter [nm]" = uip_hydro_diameter
                        , "diffusion coefficient used for simulation [nm\uc2b2 s\ue281bb\uc2b9]" = uip_flag_diffusion_coefficient
                        , "particle distribution file used" = uip_flag_distribution
                        , "particle distribution file" = input$uip_example_distribution_path$name
                        )
                        , "sedimentation" = df
                        , "Output" = Output
                        )

                ####save the simulation data to the given file####
                save(simulation_data, file = uip_simulation_data_file)

                return(simulation_data)
                }

                if(is.na(input$simulationSaveFile$name)){
                    shiny::showNotification(
                        ui = stringr::str_c(
                            "WARNING: No file set to save simulation data ..."
                            , input$simulationSaveFile$name
                            )
                        , type = "error"
                        )
                }else{##FALSE
                    shiny::showNotification(
                        ui = stringr::str_c(
                            "Saving simulation data to file "
                            , input$simulationSaveFile$name
                            , " Starting simulation ... "
                            )
                        , type = "message"
                        )

                SaveFileInfo <-
                    as.character(
                        shinyFiles::parseSavePath(
                            roots = c(wd = stringr::str_c(getwd(), "/.."))
                            , input$simulationSaveFile
                            )$datapath
                        )

                simulation_data <-
                    Three_DSDD(
                        uip_N =  input$uip_particle_number
                        , uip_bottom_area = input$uip_bottom_area
                        , uip_filling_level = input$uip_filling_level
                        , uip_cell_growth_height = input$uip_cell_growth_height
                        , uip_temperature = input$uip_temperature
                        , uip_effective_density = input$uip_effective_density
                        , uip_medium_density = input$uip_medium_density
                        , uip_medium_viscosity = input$uip_medium_viscosity
                        , uip_simulation_time = input$uip_simulation_time
                        , uip_temperature_NTA = input$uip_temperature_NTA
                        , uip_medium_viscosity_NTA = input$uip_medium_viscosity_NTA
                        , uip_diffusion_coefficient = input$uip_diffusion_coefficent
                        , uip_hydro_diameter = input$uip_hydr_diam
                        , uip_flag_diffusion_coefficient = input$uip_flag_diffusion_coefficient
                        , uip_flag_distribution = input$uip_flag_distribution
                        , uip_simulation_data_file = SaveFileInfo
                        , uip_accelleration = 9.80665
                        , uip_excel_table_path = as.character(input$uip_example_distribution_path$datapath)
                        , uip_snap_shot = input$uip_snap_shot
                        )

                shiny::showNotification(
                        ui = stringr::str_c(
                        "Simulation done and results saved to file: "
                        , SaveFileInfo
                        )
                    , closeButton = TRUE
                    , type = "message"
                )
                }
    }) ####End of the observer for the simulation start button####

    output$save_path <-
        shiny::renderPrint({
            volumes <- c(wd='.')
            shinyFiles::parseSavePath(volumes, input$simulationSaveFile)
            })

    shiny::observeEvent(
        input$done
        , {
            shiny::stopApp(TRUE)
            })

    # Section for visualisation -----------------------------------------------

    output$simulation_length <-
        shiny::reactive({
            simulation_data$Output %>%
            length()
            })

    output$scatterplot <-
        threejs::renderScatterplotThree({
            data.table::data.table() %>%
            rbind(
                if(input$particles_switch){
                    simulation_data$Output[[input$simulation_time_point]]$Vcart %>%
                    data.table::data.table() %>%
                    .[~x != -10000000000]
                    }) %>%
            rbind(
                if(input$bottom_hit_switch){
                    simulation_data$Output[[input$simulation_time_point]]$Bottom_count %>%
                    polar2cart_m() %>%
                    data.table::data.table() %>%
                    data.table::setnames(
                        old = c("V1", "V2", "V3")
                        , new = c("x", "y", "z")
                    ) %>%
                    .[!is.na(~x)]
                    }) %>%
            rbind(
                if(input$wall_hit_switch){
                    simulation_data$Output[[input$simulation_time_point]]$Wall_count %>%
                    polar2cart_m() %>%
                    data.table::data.table() %>%
                    data.table::setnames(
                        old = c("V1", "V2", "V3")
                        , new = c("x", "y", "z")
                        ) %>%
                    .[!is.na(~x)]
                    }) %>%
            BBmisc::normalize() %>%
            as.matrix() %>%
            threejs::scatterplot3js(
                size = 0.1
                , bg = "black"
                , color = rep("red", length.out = nrow(.))
                , grid = FALSE
                , pch = "@"
                # , width = 100
                )
            })

    output$current_simulation_time_point <-
        shiny::renderText({
            paste("Current time point:\n"
                , input$simulation_time_point * 0.25#time_snap_value
                , " h in a "
                , simulation_period
                , " h simulation period."
                )
            })

    output$bottom_hits <-
        plotly::renderPlotly({
            x_surface_bottom <-
                simulation_data$Output[[input$simulation_time_point]]$Bottom_count %>%
                polar2cart_m() %>%
                .[,1]
            y_surface_bottom <-
                simulation_data$Output[[input$simulation_time_point]]$Bottom_count %>%
                polar2cart_m() %>%
                .[,2]

            x_c_surface_bottom <- cut(x_surface_bottom, input$surface_density)
            y_c_surface_bottom <- cut(y_surface_bottom, input$surface_density)

            z_surface_bottom <- table(x_c_surface_bottom, y_c_surface_bottom)

            plotly::plot_ly(
                z = z_surface_bottom
                            , color = I("red")
                            ) %>%
                plotly::add_surface() %>%
                plotly::layout(
                    list(
                        title = "Bottom Hits"
                        )
                    )
            })

    output$wall_hits <-
        plotly::renderPlotly({
            x_surface_wall <-
                simulation_data$Output[[input$simulation_time_point]]$Wall_count %>%
                polar2cart_m() %>%
                .[,1]
            y_surface_wall <-
                simulation_data$Output[[input$simulation_time_point]]$Wall_count %>%
                polar2cart_m() %>%
                .[,3]

            x_c_surface_wall <- cut(x_surface_wall, input$surface_density)
            y_c_surface_wall <- cut(y_surface_wall, input$surface_density)

            z_surface_wall <- table(x_c_surface_wall, y_c_surface_wall)

            plotly::plot_ly(
                z = z_surface_wall
                , color = I("blue")
                ) %>%
                plotly::add_surface() %>%
                plotly::layout(
                    list(
                        title = "Wall Hits"
                         )
                    )
            })
}

#### initial snippet to start the application
shiny::runGadget(
    app =  shiny::shinyApp(
        ui = ui
        , server = server
        )
    , viewer = shiny::browserViewer()
    )
}
