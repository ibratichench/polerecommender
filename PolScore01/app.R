# POLE Score
# Ivana Bratic Hench, IfP Basel

library(shiny)

#define the treatment decision
poletreatfunctionhtml <- function(ca,tg,cg,indel,tmb,polerec,exo,msi,insilicoYesNo){
    if(insilicoYesNo=="no"){
        poleimg<-"POLEScore_absent_recommendation.png"
        poletreattext<-sprintf("Follow the guidelines in the table below:<br><img src='%s' width='100%%'>",poleimg)
    }else if (insilicoYesNo=="yes"){
        polescore <- sum(as.integer(c(ca,tg,cg,indel,tmb,polerec)))
        if(polescore>=4){
            poletext<-"POLE mutation is pathogenic."
        }else if (polescore==3){
            poletext<-"POLE mutation is a VUS."
        }else{
            poletext<-"POLE mutation is non-pathogenic."
        }
        polescoretext<-sprintf("The POLE score is %i.<br>%s",polescore,poletext)
        if(exo==TRUE && polescore>=4){
            poletreat<-"Treat as POLEmut EC."
        }else if (exo==TRUE && polescore<4 && msi==TRUE){
            poletreat<-"Treat as MMRd EC."
        }else if (exo==TRUE && polescore<4 && msi==FALSE){
            poletreat<-"Treat as POLEwt EC."
        }else if (exo==FALSE && msi==FALSE){
            poletreat<-"Treat as NSMP/p53abn EC."
        }else if (exo==FALSE && msi==TRUE){
            poletreat<-"Treat as MMRd EC."
        }else{
            poletreat<-"Not defined."
        }
        poletreattext<-sprintf("MSI is %s<br>%s<br>Recommendation: %s",msi,polescoretext,poletreat)
    }
    return(poletreattext)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}")) # thicker hr bar, see https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar/43593325
    ),

    # Application title
    titlePanel("POLE Recommender"),
    sidebarLayout(
        sidebarPanel(
            h6("I. Bratic Hench, IfP Basel, 2020"),
            selectInput("insilicoYesNo", "Can you answer the questions below with the existing data?", choices=c("yes","no")),
            checkboxInput("ca","C>A is over 20%.",FALSE),
            checkboxInput("tg","T>G is over 4%.",FALSE),
            checkboxInput("cg","C>G is below 0.6%.",FALSE),
            checkboxInput("indel","Indel number is below 5%.",FALSE),
            checkboxInput("tmb","TMB is over 100mut/MB.",FALSE),
            checkboxInput("polerec","Detected POLE mutation is a recurrent variant in endom. carcinoma.",FALSE),
            hr(),
            h5("Is the POLE mut in exonuclease domain:"),
            checkboxInput("exo","POLE mut is at AA position 268-471.",FALSE),
            h5("Microsatelite instability:"),
            checkboxInput("msi","Sample is MSI.",FALSE),
            hr(),
            h6("NSMP - no specific molecular profile"),
            h6("p53abn EC - p53 IHC should be performed to exclude a p53abn EC"),
            hr(),
            h6("León‐Castillo et al. Interpretation of somatic POLE mutations in endometrial carcinoma. J. Pathol. 2020, 250, 323–335, doi:10.1002/path.5372.")
        ),
        # show the results
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Treatment Choice", htmlOutput("poleresult")),
                        tabPanel("POLE Mutations", htmlOutput("polemutations"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$poleresult <- renderUI(HTML(poletreatfunctionhtml(input$ca,input$tg,input$cg,input$indel,input$tmb,input$polerec,input$exo,input$msi,input$insilicoYesNo)))
    output$polemutations <- renderUI(HTML("<b>Pathogenic Pole Mutations</b><br><img src='POLEmut_Path.png' width='48%'><br><br><b>Other Pole Mutations</b><br><img src='POLE_Mut_list.png' width='100%'>"))    
}

# Run the application 
shinyApp(ui = ui, server = server)
