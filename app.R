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
if (!(require(showtext))){
    install.packages("showtext", quiet = T)
    require(showtext)
}
showtext_auto()


# file with translations
translator <- Translator$new(translation_csvs_path = "data")

# Define UI for the fitness tracker
ui <- uiOutput('page_content')


# Define server logic 
server <- function(input, output, session) {
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  likert.choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
  
  YN.choices = list("Yes" = 1, "No" = 0)
  v1problist = list("It has to satisfy very strict requirements" = 1, 
                    "It has confidential IP that should not be exposed" = 2, 
                    "It is in the critical path of important deliverables" = 1)
  v2pluslist = list("It has useful features/modules that might prevent effort wasted on 'Reinventing the Wheel" = 1, 
                    "Maintainers are receptive to external contributions" = 1)
  # Update Radio Buttons & Check options
  observeEvent(i18n(), { 
    names(YN.choices) = i18n()$t(names(YN.choices)) 
    names(v1problist) = i18n()$t(names(v1problist)) 
    names(v2pluslist) = i18n()$t(names(v2pluslist)) 
    
    updateCheckboxGroupButtons(session, "v1.prob",label = i18n()$t("Is any of the following true for the project? (choose all applicable options)"),
                         choices = v1problist,
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-check-square", 
                                        style = "color: steelblue"),
                           no = tags$i(class = "fa fa-square-o", 
                                       style = "color: steelblue")))
    
    updateCheckboxGroupButtons(session, "v2.plus",label = NULL,
                               choices = v2pluslist,
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square", 
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-square-o", 
                                             style = "color: steelblue")))
    
    updateRadioGroupButtons(session, "v1.mvp", 
                            i18n()$t("Is it a Minimum Viable Product that works and can be experimented on?"),
                            choices = YN.choices,
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                            selected = req(input$v1.mvp))
    
    updateRadioGroupButtons(session, "v3.vcs", 
                            i18n()$t("Is all of the code stored in a version control repository that makes branches, pull requests, and integration easy?"),
                            choices = YN.choices,
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                            selected = req(input$v3.vcs))
    
    updateRadioGroupButtons(session, "v4.release", 
                            i18n()$t("Can releases be made frequently and/ or on a time-based schedule?"),
                            choices = YN.choices,
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                            selected = req(input$v4.release))
    
    updateRadioGroupButtons(session, "v4.announce", 
                            i18n()$t("Is there a mechanism for making announcements that anyone in the organization can follow and search? (Examples: Slack/ email)"),
                            choices = YN.choices,
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                            selected = req(input$v4.announce))
    
    updateRadioGroupButtons(session, "v4.search", 
                            i18n()$t("Is there a mechanism to record discussions so that new guest contributors can search for all previous Q-A and internal team decisions? (Examples: Slack/ online forum)"),
                            choices = YN.choices,
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                            selected = req(input$v4.search))
    
    })
  
  
    # observeEvent(input$selected_language, {
    #     # This print is just for demonstration
    #     print(paste("Language change!", input$selected_language))
    #     # Here is where we update language in session
    #     shiny.i18n()::update_lang(session, input$selected_language)
    # })
    
    # Mapping Likert Scale responses to numbers
    maplist = list("Strongly disagree" = 1, "Disagree" = 2, "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5, ' 非常不同意' = 1, ' 不同意' = 2, ' 不同意也不反对' = 3, ' 同意' = 4, ' 非常同意' = 5 )
    
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
        # Setup the text settings
        seedProduct = i18n()$t("Seed Product")
        stakeholders = i18n()$t("Multiple potential stakeholders")
        contribution = i18n()$t("Ease of Contribution")
        tools = i18n()$t("Appropriate tools & practices")
        readiness  = i18n()$t("Team Readiness for InnerSource")
        management = i18n()$t("Management Support")
        colnames = c(seedProduct, stakeholders, contribution, tools, readiness, management)
        
        colnames(data) <- colnames
        
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
                title = i18n()$t("Thank you!"),
                text = i18n()$t("See the Result Below"),
                type = "success"
            )}
    })
    
    observeEvent(input$info, {
        sendSweetAlert(
            session = session,
            title = "What it all means",
            text = 'Any project can be developed in InnerSource style, but some projects are more suitable candidates than others. The "fitness" of a project can be measured by assessing whether the project has the necessary qualities to attract enough developers to build a community around it, that the tools and processes allow for collaborative development, and that the project maintainers are ready to face the challenges of InnerSource style development. We label this as the the Project-Process-People model. We suggest two sub-factors within each which overall provides six dimensions to measure the fitness of candidate projects.\n Further Reading: https://ieeexplore.ieee.org/document/6809709, https://innersourcecommons.org/checklist/',
            type = "info"
        )
    })
    
    # generate plots
    
    gen_radarplot <- reactive({
        showtext_auto()
        par(mar=c(0,0,4,0))
        radarchart( df=getData(), axistype=1, seg = 5,
                    #custom polygon
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                    #custom the grid
                    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5,1), cglwd=0.8,
                    #custom labels
                    vlcex=1.2, 
                    title = paste0(i18n()$t("Visual Breakdown of Fitness Score"),
                                   '---',input$name)
        )
    })
    
    # If-else for language translation
    gen_lollipopplot <- reactive({
        showtext_auto()
        fontFamily = "sans"
        if (input$selected_language == '中文'){
            fontFamily =  "wqy-microhei"
        }
        #setup the display text
        technology = i18n()$t("Project compatibility")
        process = i18n()$t("Process compatibility")
        people = i18n()$t("People compatibility")
        
        getData() %>% slice(3) %>% t() %>% as.data.frame() %>% add_rownames() %>% mutate(Category=factor(c(rep(technology,2), rep(process,2), rep(people,2)))) %>% arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
            ggplot( aes(x=rowname, y=V1, color=Category)) +
            geom_segment( aes(x=rowname ,xend=rowname, y=0, yend=V1), color="grey") +
            geom_point(size=5) +
            coord_flip() +
            theme_ipsum(base_family=fontFamily, base_size = 14)  + 
            ylim(0,5) + ylab(i18n()$t("score")) + xlab("")+ 
            ggtitle(paste0(i18n()$t("Visual Breakdown of Fitness Score"), '---', 
                           input$name)) +
            theme( text = element_text(size=14),
                   legend.position="bottom",
                   legend.title = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_blank()
            ) 
        
    })
    
    # render output 
    
    output$score <- renderText({
        titleLeftText = i18n()$t("The Estimated Fitness Score of this Project is:")
        titleRightText = i18n()$t("; (Range: 0 to 10)")
        paste(titleLeftText, getScore(), titleRightText)
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
            projectname = i18n()$t("Project Name")
            score = i18n()$t("Final Score")
            dataout = getData()
            dataout = dataout %>% slice(3)
            # dataout$Final.Score = getScore()
            dataout = cbind(projectname=input$name, score= getScore(),dataout)
            colnames(dataout)[1:2] = c(projectname, score)
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
                        vlcex=1.2, 
                        title = paste0(i18n()$t("Visual Breakdown of Fitness Score"),
                                                  '---',input$name)
            )
            print( gen_lollipopplot() )
            dev.off()
            
        }
    )
    
    # saving to local storage
    observeEvent(input$local, {
      tryCatch({
      main.dir = getwd()
      dir.create(file.path(main.dir, input$name ))
      setwd(file.path(main.dir, input$name))
      tym = as.integer(Sys.time())
      #data
      filename = sprintf('Data_%s_%s.csv',input$name,tym)
      projectname = i18n()$t("Project Name")
      score = i18n()$t("Final Score")
      dataout = getData()
      dataout = dataout %>% slice(3)
      dataout = cbind(projectname=input$name, score= getScore(),dataout)
      colnames(dataout)[1:2] = c(projectname, score)
      write.csv(dataout, filename, row.names = F)
      
      #plots
      pfname = sprintf('Plot_%s_%s.pdf',input$name,tym)
      pdf(pfname, width = 12, height = 10)
      par(mar=c(0,0,4,0))
      radarchart( df=getData(), axistype=1, seg = 5,
                  #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5,1), cglwd=0.8,
                  #custom labels
                  vlcex=1.2, 
                  title = paste0(i18n()$t("Visual Breakdown of Fitness Score"),
                                 '---',input$name)
      )
      print( gen_lollipopplot() )
      dev.off()
      setwd(main.dir)
      
      #confirmation
      sendSweetAlert(
        session = session,
        title = i18n()$t("Thank you!"),
        text = i18n()$t("The File has been saved"),
        type = "success"
      )},
      error=function(cond) {
        sendSweetAlert(
          session = session,
          title = i18n()$t("ERROR!"),
          text = paste(i18n()$t("Something went wrong"), cond),
          type = "error"
        )
      })
      
    })
    
    # UI 
    output$page_content <- renderUI({
      fluidPage(
        div(style = "float: right;",
            selectInput('selected_language',
                        i18n()$t("Change language"),
                        choices = translator$get_languages(),
                        selected = input$selected_language),
        ),
        
        # Application title
        title = i18n()$t("InnerSource Project Fitness Assessment"),
        useSweetAlert(),
        
        h1(i18n()$t("InnerSource Project Fitness Assessment Questionnaire")),
        
        textInput("name", label = h3(i18n()$t("Enter Project Name")), value = ""),
        
        
        # All Questions are listed below
        
        h3(i18n()$t("Project compatibility")),
        br(),
        
        fluidRow(
          column(3, sliderTextInput("v2.collab",
                                    label = i18n()$t("The project has functionality that is likely to be interesting to developers outside the original development team"),
                                    grid = T, force_edges = TRUE,
                                    choices = i18n()$t(likert.choices))),
          
          column(3, radioGroupButtons("v1.mvp", 
                                      i18n()$t("Is it a Minimum Viable Product that works and can be experimented on?"),
                                      choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                                      checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
          
          column(3, sliderTextInput("v1.value", 
                                    label = i18n()$t("The project is valuable to the company"),
                                    grid = T, force_edges = TRUE,
                                    choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v2.use",label = i18n()$t("The project (or some modules) could be widely used by other teams in the company who might depend on it"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
        ),
        br(),
        fluidRow(
          
          column(12, checkboxGroupButtons("v1.prob",label = i18n()$t("Is any of the following true for the project? (choose all applicable options)"),
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
        
        h3(i18n()$t("Process compatibility")),
        br(),
        
        fluidRow(
          column(3, sliderTextInput("v3.mod",label =   i18n()$t("The project is modular enough to make changes easy and safe to make"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v3.doc",label = i18n()$t("All ancillary resources (Adequate Code Documentation, Discussion forum, Bug Tracker, Wiki etc.) are set up for the project"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v3.con",label = i18n()$t("The project has clear and easy-to-find Contribution guidelines, documentation on development environment setup, running tests, etc. making it easy to contribute to"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, radioGroupButtons("v3.vcs", 
                                      i18n()$t("Is all of the code stored in a version control repository that makes branches, pull requests, and integration easy?"),
                                      choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                                      checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
          
        ),
        br(),
        fluidRow(
          column(3, radioGroupButtons("v4.release", 
                                      i18n()$t("Can releases be made frequently and/ or on a time-based schedule?"),
                                      choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                                      checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
          
          
          column(3, sliderTextInput("v4.common",label = i18n()$t("The project uses specialized tools/ framework that outsiders need to learn before they can contribute"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, radioGroupButtons("v4.announce", 
                                      i18n()$t("Is there a mechanism for making announcements that anyone in the organization can follow and search? (Examples: Slack/ email)"),
                                      choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                                      checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
          
          column(3, radioGroupButtons("v4.search", 
                                      i18n()$t("Is there a mechanism to record discussions so that new guest contributors can search for all previous Q-A and internal team decisions? (Examples: Slack/ online forum)"),
                                      choices = list("Yes" = 1, "No" = 0), justified = TRUE, selected = 0,
                                      checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
        ),
        
        br(),
        h3(i18n()$t("People compatibility")),
        br(),
        fluidRow(
          column(3, sliderTextInput("v5.accept",label = i18n()$t("Team Members are  willing to accept code from outsiders and make code changes based on their feedback"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v5.see",label = i18n()$t("Team Members are ok with having outsiders see their less-than-perfect code"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v5.conv",label = i18n()$t("Team Members are ok with having to conduct conversations (that are sometimes difficult) with outsiders about accepting and rejecting their contributions"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v5.mentor",label = i18n()$t("Team Members are ready to mentor and/or learning to mentor contributors from other teams"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices)))
        ),
        
        br(),
        fluidRow(
          column(3, sliderTextInput("v5.work",label = i18n()$t("Team Members are ready to work with external contributors for fixing any defects on the contributed code"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v5.doc",label = i18n()$t("Team Members are willing to create and maintain documentation for external contributors"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v5.forum",label = i18n()$t("Team Members are willing to participate in online forums and answer questions patiently (instead of having offline conversations)"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v5.review",label = i18n()$t("Team Members are willing to do code reviews for external contributors' submissions"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
        ),
        
        br(),
        fluidRow(
          column(3, sliderTextInput("v6.flex",label = i18n()$t("Management is willing to support flexible work requirements and value the time spent on cross-departmental contributions"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v6.merit",label = i18n()$t("Management supports a meritocratic philosophy that appreciates good contributions from all quarters"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v6.fail",label = i18n()$t("Management understands the difficulties of a cultural change and can accept experimentation, failure, and repositioning to a reasonable extent"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
          
          column(3, sliderTextInput("v6.reward",label = i18n()$t("Management is willing to recognize and incentivize the efforts for making InnerSource Successful"),
                                    grid = T, force_edges = TRUE, choices = i18n()$t(likert.choices))),
        ),
        
        # Action Buttons are listed below
        
        br(),
        fluidRow(
          column(1, switchInput(
            inputId = "alert", label = i18n()$t("Disable Alerts")
          )),
          column(2, offset = 1, actionBttn("go",label = i18n()$t("Check Results"), 
                                           style = "pill", color = "danger",icon = icon("eye"))),
          column(2, offset = 0, actionBttn("info",label = i18n()$t("Explanation"), 
                                           style = "pill", color = "primary",icon = icon("info-circle"))),
          column(2, offset = 0, actionBttn("local",
                                           label = i18n()$t("Save results"), 
                                           style = "pill", color = "warning",
                                           icon = icon("save"))),
          column(2, offset = 0, downloadBttn("saveData",label = i18n()$t("Download Scores"), 
                                             style = "jelly", color = "success")),
          
          column(2, offset = 0, downloadBttn("savePlot", label = i18n()$t("Download Plots"), style = "jelly", color = 'royal')),
          
          
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
        
        
        
      )})
}

# Run the application 
shinyApp(ui = ui, server = server)