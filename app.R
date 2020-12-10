#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!(require(shiny))){
    install.packages("shiny", quiet = T)
    require(shiny)
}
if (!(require(shiny.i18n))){
    install.packages("shiny.i18n", quiet = T)
    require(shiny.i18n)
}
if (!(require(fmsb))){
    install.packages("fmsb", quiet = T)
    require(fmsb)
}
if (!(require(colormap))){
    install.packages("colormap", quiet = T)
    require(colormap)
}
if (!(require(dplyr))){
    install.packages("dplyr", quiet = T)
    require(dplyr)
}
if (!(require(hrbrthemes))){
    install.packages("hrbrthemes", quiet = T)
    require(hrbrthemes)
}    
if (!(require(ggplot2))){
    install.packages("ggplot2", quiet = T)
    require(ggplot2)
} 
if (!(require(shinyWidgets))){
    install.packages("shinyWidgets", quiet = T)
    require(shinyWidgets)
}

# file with translations
i18n <- Translator$new(translation_csvs_path = "data")
# change this to en
i18n$set_translation_language("en")


# Define UI for the fitness tracker
ui <- fluidPage(
    
    shiny.i18n::usei18n(i18n),
    div(style = "float: right;",
        selectInput('selected_language',
                    i18n$t("Change language"),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation())
    ),
    
    # Application title
    title = i18n$t("InnerSource Project Fitness Assessment"),
    useSweetAlert(),
    
    h1(i18n$t("InnerSource Project Fitness Assessment Questionnaire")),
    
    # All Questions are listed below
    
    h3(i18n$t("Technology compatibility")),
    br(),
    
    fluidRow(
        column(3, sliderTextInput("v2.collab",
                                  label = i18n$t("The project have functionality that is likely to be interesting to developers outside the original development team"),
                                  grid = T, force_edges = TRUE,
                                  choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, radioGroupButtons("v1.mvp", 
                               "Is it a Minimum Viable Product that works and can be experimented on?",
                               choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                               checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
        
        column(3, sliderTextInput("v1.value", 
                                  label = "The project is valuable to the company",
                                  grid = T, force_edges = TRUE,
                                  choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v2.use",label = "The project (or some modules) could be widely used by other teams in the company who might depend on it",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
    ),
    br(),
    fluidRow(
        
        column(12, checkboxGroupButtons("v1.prob",label = "Is any of the following true for the project? (choose all applicable options)",
                    choices = list("It has to satisfy very strict requirements" = 1, 
                                "It has confidential IP that should not be exposed" = 2, 
                                "It is in the critical path of important deliverables" = 1),           justified = TRUE,
                              checkIcon = list(
                                  yes = tags$i(class = "fa fa-check-square", 
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-square-o", 
                                              style = "color: steelblue")))),
        column(12, checkboxGroupButtons("v2.plus",label = NULL,
                    choices = list('It has useful features/modules that might prevent effort wasted on "Reinventing the Wheel"' = 1, 
                                   "Maintainers are receptive to external contributions" = 1
                                   ),   justified = TRUE,
                                        checkIcon = list(
                                            yes = tags$i(class = "fa fa-check-square", 
                                                         style = "color: steelblue"),
                                            no = tags$i(class = "fa fa-square-o", 
                                                        style = "color: steelblue")))),
    ),
    br(),
    
    h3("Process compatibility"),
    br(),
    
    fluidRow(
        column(3, sliderTextInput("v3.mod",label =  "The project is modular enough to make changes easy and safe to make",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v3.doc",label = "All ancillary resources (Adequate Code Documentation, Discussion forum, Bug Tracker, Wiki etc.) are set up for the project",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v3.con",label = "The project has clear and easy-to-find Contribution guidelines, documentation on development environment setup, running tests, etc. making it easy to contribute to",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, radioGroupButtons("v3.vcs", 
                               "Is all of the code stored in a version control repository that makes branches, pull requests, and integration easy?",
                               choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                               checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
    ),
    br(),
    fluidRow(
        column(3, radioGroupButtons("v4.release", 
                                    "Can releases be made frequently and/ or on a time-based schedule?",
                                choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                                checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
        
        
        column(3, sliderTextInput("v4.common",label = "The project uses specialized tools/ framework that outsiders need to learn before they can contribute",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, radioGroupButtons("v4.announce", 
                               "Is there a mechanism for making announcements that anyone in the organization can follow and search? (Examples: Slack, email)",
                               choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                               checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
        
        column(3, radioGroupButtons("v4.search", 
                                    "Is there a recorded mechanism for discussion so that all Guest Contributor questions and internal team decisions are searchable by incoming Guest Contributors? (Examples: Slack, online forum)",
                            choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
    ),
    
    br(),
    h3("People compatibility"),
    br(),
    fluidRow(
        column(3, sliderTextInput("v5.accept",label = "Team Members are ok with accepting code and changes to their code from outsiders",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v5.see",label = "Team Members are ok with having outsiders see their less-than-perfect code",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v5.conv",label = "Team Members are ok with having to conduct conversations (that are sometimes difficult) with outsiders about accepting and rejecting their contributions",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v5.mentor",label = "Team Members are ready to mentor and/or learning to mentor contributors from other teams",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")))
    ),
    
    br(),
    fluidRow(
        column(3, sliderTextInput("v5.work",label = "Team Members are ready to work with outside contributors for fixing any defects on the contributed code",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v5.doc",label = "Team Members are willing to create and maintain documentation for outside contributors",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v5.forum",label = "Team Members are willing to participate in forums and answer questions patiently (instead of offline conversations)",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v5.review",label = "Team Members are willing to do code reviews for outside submissions",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
    ),
    
    br(),
    fluidRow(
        column(3, sliderTextInput("v6.flex",label = "Management is willing to support flexible work requirements and value the time spent on cross-departmental contributions",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v6.merit",label = "Management supports a meritocratic philosophy that appreciates good contributions from all corners",
                        grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v6.fail",label = "Management understands the difficulties and can accept experimentation, failure, and repositioning to a reasonable extent",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
        
        column(3, sliderTextInput("v6.reward",label = "Management is willing to recognize and incentivize the efforts for making InnerSource Successful",
                                  grid = T, force_edges = TRUE, choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))),
    ),
    
    # Action Buttons are listed below
    
    br(),
    fluidRow(
        column(2, switchInput(
            inputId = "alert", label = "Disable Alerts"
        )),
        column(2, offset = 1, actionBttn("go",label = "Check Results", 
                                 style = "pill", color = "danger",icon = icon("eye"))),
        column(2, offset = 1, downloadBttn("saveData",label = "Download Scores", 
                                         style = "jelly", color = "success")),
        column(2, offset = 1, downloadBttn("savePlot", label = "Download Plots", style = "jelly")),
    ),    

    # Show Project Score
    br(),
    fluidRow(
        fluidRow(column(7, offset = 2, h3(textOutput("score"))))

    ),
    # For testing: comment if not used
    # fluidRow(column(12, verbatimTextOutput("test"))),
    
    # Show Plots
    
    br(),
    fluidRow(
        column(6,plotOutput("radarplot") ),
        column(6, plotOutput("lolipop"))
    )
           


)

# Define server logic 
server <- function(input, output, session) {
    
    observeEvent(input$selected_language, {
        # This print is just for demonstration
        print(paste("Language change!", input$selected_language))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
    })
    
    # Mapping Likert Scale responses to numbers
    maplist = list("Strongly disagree" = 1, "Disagree" = 2, "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5)
    
    # Create Data Frame from responses
    getData <- eventReactive(input$go, {
        v1.1 = 5 - sum(as.numeric(input$v1.prob))
        v2.2 = 1 + sum(as.numeric(input$v2.plus))*2
        v1 = (v1.1 + 1 +4*as.numeric(input$v1.mvp) + 
                  as.numeric(maplist[input$v1.value]))/3
        v2 = (as.numeric(maplist[input$v2.collab]) + as.numeric(maplist[input$v2.use]) + 
                 v2.2 )/3
        v3 = (as.numeric(maplist[input$v3.con]) + as.numeric(maplist[input$v3.doc]) +
                  as.numeric(maplist[input$v3.mod]) + 1 + 4*as.numeric(input$v3.vcs))/4
        v4 = (3 + (6 - as.numeric(maplist[input$v4.common])) + 4*as.numeric(input$v4.announce) +
                  4*as.numeric(input$v4.search) + 4*as.numeric(input$v4.release))/4
        v5 = (as.numeric(maplist[input$v5.accept]) + as.numeric(maplist[input$v5.see]) + 
                  as.numeric(maplist[input$v5.conv])+ as.numeric(maplist[input$v5.mentor]) + 
                  as.numeric(maplist[input$v5.work]) + as.numeric(maplist[input$v5.doc]) + 
                  as.numeric(maplist[input$v5.forum])+ as.numeric(maplist[input$v5.review]))/8
        v6 = (as.numeric(maplist[input$v6.flex]) + as.numeric(maplist[input$v6.merit]) + 
                  as.numeric(maplist[input$v6.reward])+ as.numeric(maplist[input$v6.fail]))/4
        
        data <- as.data.frame(matrix(c(v1, v2, v3, v4, v5, v6), ncol=6))
        colnames(data) <- c("Seed Product", "Multiple potential stakeholders", 
                            "Ease of Contribution", "Appropriate tools & practices", 
                            "Team Readiness for InnerSource", 
                            "Management Support" )
        
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
        data <- rbind(rep(5,6) , rep(0,6) , data)
        return(data)
    })
    
    # For Testing - when things break: comment if not used
    # output$test = renderPrint({ print(input)})

    # Calculate Project Score - weighted by responses from the Survey
    getScore <- eventReactive(input$go, { v1.1 = 5 - sum(as.numeric(input$v1.prob))
     v1 = v2 = v3 = v4 = v5 = v6 = v7 =0
     v1.1 = 5 - sum(as.numeric(input$v1.prob))
     v2.2 = 1 + sum(as.numeric(input$v2.plus))*2
    v1 = (v1.1*0.6 + (1 +4*as.numeric(input$v1.mvp))*0.74 +
              as.numeric(maplist[input$v1.value])*0.70)
    v2 = (as.numeric(maplist[input$v2.collab])*0.89 + v2.2*0.70 +
              as.numeric(maplist[input$v2.use])*0.85)
    v3 = (as.numeric(maplist[input$v3.con])*0.80 + as.numeric(maplist[input$v3.doc])*0.75 +
              as.numeric(maplist[input$v3.mod])*0.75 + 1 + 4*as.numeric(input$v3.vcs)*0.6)
    v4 = (3 + (6 - as.numeric(maplist[input$v4.common])) + 4*as.numeric(input$v4.announce) +
              4*as.numeric(input$v4.search) + 4*as.numeric(input$v4.release))*0.60
    v5 = (as.numeric(maplist[input$v5.accept]) + as.numeric(maplist[input$v5.see]) +
              as.numeric(maplist[input$v5.conv])+ as.numeric(maplist[input$v5.mentor]))*0.80
    v6 = (as.numeric(maplist[input$v5.work]) + as.numeric(maplist[input$v5.doc]) +
              as.numeric(maplist[input$v5.forum])+ as.numeric(maplist[input$v5.review]))*0.80
    v7 = (as.numeric(maplist[input$v6.flex]) + as.numeric(maplist[input$v6.merit]) +
              as.numeric(maplist[input$v6.reward])+ as.numeric(maplist[input$v6.fail]))*0.85
    score = round((v1 + v2 + v3 + v4 + v5 + v6 + v7)/7.7 - 2.5, 3)
    return(score)

    })
    
    # Alert
    observeEvent(input$go, {
        if (!input$alert){
        sendSweetAlert(
            session = session,
            title = "Thank you!",
            text = "See the Result Below",
            type = "success"
        )}
    })

    # generate plots
    
    gen_radarplot <- reactive({
        par(mar=c(0,0,4,0))
        radarchart( df=getData(), axistype=1, seg = 5,
                    #custom polygon
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                    #custom the grid
                    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5,1), cglwd=0.8,
                    #custom labels
                    vlcex=1.2, title = "Visual Breakdown of Fitness Score"
        )
    })
    
    gen_lollipopplot <- reactive({
        getData() %>% slice(3) %>% t() %>% as.data.frame() %>% add_rownames() %>% mutate(Category=factor(c(rep("Technology compatibility",2), rep("Process compatibility",2), rep("People compatibility",2)))) %>% arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
            ggplot( aes(x=rowname, y=V1, color=Category)) +
            geom_segment( aes(x=rowname ,xend=rowname, y=0, yend=V1), color="grey") +
            geom_point(size=5) +
            coord_flip() +
            theme_ipsum(base_family="sans", base_size = 14)  +
            ylim(0,5) + ylab("score") + xlab("")+ ggtitle("Visual Breakdown of Fitness Score") +
            theme( text = element_text(size=14),
                   legend.position="bottom",
                   legend.title = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_blank()
            ) + guides(fill=guide_legend(nrow=3, byrow=TRUE))
    })
    
    # render output 
    
    output$score <- renderText({ 
        paste("The Estimated Fitness Score of this Project is:", getScore(), "; (Range: 0 to 10)")
    })
    
    output$radarplot <- renderPlot({
        gen_radarplot()
    })
    
    output$lolipop <- renderPlot({
        gen_lollipopplot()
    })
    
    # saving data
    output$saveData = downloadHandler(
        filename = function() {
            paste("data", Sys.Date(), as.numeric(input$go) ,"0.csv", sep="_")
        },
        content = function(file) {
            dataout = getData()
            dataout = dataout %>% slice(3)
            dataout$Final.Score = getScore()
            write.csv(dataout, file, row.names = F)
        }
    )
    
    # saving plots
    output$savePlot = downloadHandler(
        filename = function() {
            "Rplots.pdf"
        },
        content = function(file) {
            pdf(file, width = 12, height = 10)
            par(mar=c(0,0,4,0))
            radarchart( df=getData(), axistype=1, seg = 5,
                        #custom polygon
                        pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                        #custom the grid
                        cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5,1), cglwd=0.8,
                        #custom labels
                        vlcex=1.2, title = "Visual Breakdown of Fitness Score"
            )
            print( gen_lollipopplot() )
            dev.off()
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
