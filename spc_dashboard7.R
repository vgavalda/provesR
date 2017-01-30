# PASSAT 
# Identificacio de punts i llistat amb datatable 

setwd("~/R/spc_shinydashboard")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
source("~/R/funcions/funcio_spc.R")


ui <- dashboardPage(skin = "yellow",
    dashboardHeader(titleWidth = 150, title = "Grafics SPC"),
    dashboardSidebar(
        width = 150,
        textInput("proj", label = "Projecte:", value = "626800D"),
        
        textInput("fase", label = "Fase:", value = "SINT"),
        
        actionButton("okllista", label = "Llista caract.", icon=icon("list")),
        br(),
        br(),
        numericInput("codcar", label = "Codi caract.:", value = 2),
        
        numericInput("llan1", label = "Llancament inicial:", value = 0),
        
        numericInput("llan2", label = "Llancament final:", value = 999999),
        
        dateInput("data1", label = "Data inicial", value = "2016-01-01"),
        
        dateInput("data2", label = "Data final", value = Sys.Date() ),
        
        actionButton("somhi", label = "Actualitza", icon=icon("refresh"), col="green"),
        br(),
        br(),
        p("Si apareix l'error: object 'ordre' not found, vol dir que hi ha diverses ER incompatibles. Cal reduir les possibilitats")
    ),
    dashboardBody(
        fluidRow(
            column(width = 4, ############################
                   
                   box(title = "Fases amb control",
                       collapsible = TRUE,
                       width = NULL,
                       tableOutput("llistafases")
                   ),
                   box(
                       title = "Llista de codis", 
                       collapsible = TRUE,
                       width = NULL,
                       tableOutput("llistacodis")
                   )
            ),
            column(width = 1, ####################################
                   radioButtons("tipusdades", label ="valors", choices = c("mig"=1, "individuals"=2), selected = 1),
                   actionButton("somhi2", label = "Actualitza", icon=icon("refresh")),
                   br(),
                   br(),
                   br(),
                   selectInput("tipusgraf", label = "tipus", choices = c("histograma" = 0, "punts" = 1, "linies" = 2, "boxplot" = 3), selected = 1),
                   conditionalPanel(condition = "input.tipusgraf==0", numericInput("ample", label = "# de barres?", value = 12)),
                   conditionalPanel(condition = "input.tipusgraf==1", checkboxInput("linia", label = "dibuixo linia?", value = FALSE)),
                   br(),
                   br(),
                   selectInput("subgrups", label = "subgrups", choices = c("cap" = 0, "llancament" = 1, "maquina" = 2, "operari" = 3), selected = 0),
                   selectInput("color", label = "color", choices = c("cap" = 0, "llancament" = 1, "maquina" = 2, "operari" = 3), selected = 0),
                   br(),
                   br(),
                   downloadButton("gravagrafic", label= "Desa grafic"),
                   br(),
                   br(),
                   textInput("nomcsv", label = "Nom fitxer", value = "dades"),
                   downloadButton("gravadades", label= "Desa dades")
            ),
            column(width = 7,
                   fluidRow(
                       box(
                           title = "Grafics",
                           width = NULL,
                           height = 600,
                           plotOutput("graficggplot", brush = "selecc")
                       ),
                       box(
                           title = "Punts seleccionats",
                           collapsible = TRUE,
                           width = NULL,
                           DT::dataTableOutput("seleccionats")
                       ),
                       box(
                           title = "Resum informacio dades",
                           collapsible = TRUE,
                           width = NULL,
                           tableOutput("resumdades")
                       )
                   )
            )
        )
    )
)



server <- function(input, output) {
    
    #recuperem dades de l'spc
    spc <- reactive({
        # en funcio de quin tipus de dades volem, cridarem a una funcio o una altra.
        if(input$tipusdades==2)
            test <- dadesspcvalorsindiv(datainicial=input$data1, datafinal=input$data2, peca=input$proj, fase=input$fase, codcar=input$codcar, input$llan1, input$llan2)
        else 
            test <- dadesspc(datainicial=input$data1, datafinal=input$data2, peca=input$proj, fase=input$fase, codcar=input$codcar, input$llan1, input$llan2)
        return(test)
    })
    
    #recuperacio nom caracteristica
    nomcar <- reactive({
        nomcaracteristica(peca=spc()[[1]][1], codifase=input$fase, codicaracteristica=spc()[[9]][1])
    })
    
    #recuperacio llista codis caracteristiques
    llistacodcar <- reactive({
        quinscodiscar(peca=input$proj, codifase=input$fase)
    })
    
    #recuperacio llistat fases
    llistadefases <- reactive({
        fasesprojecte(input$proj)
    })
    
    #Resum de les dades
    resum <- reactive ({
        resumdades(spc()[[6]], input$proj, input$fase, input$codcar) 
    })
    
    
    #--------------------------
    #GENERACIO CODIS CARACTERISTIQUES
    #--------------------------    
    
    output$llistafases <- renderTable(include.rownames=FALSE, {
        
        input$okllista # L'actualitzacio de la llista, esta supeditada al boto 'okllista'
        
        isolate({print(llistadefases())}) # La impresio de la llista no es reactiva, esta 'isolada'...
    })
    
    output$llistacodis <- renderTable(include.rownames=FALSE, {
        
        input$okllista # L'actualitzacio de la llista, esta supeditada al boto 'okllista'
        
        isolate({
            dfllista <- as.data.frame(llistacodcar())
            print(dfllista, row.names=FALSE)
            #print(as.data.frame(llistacodcar()))
            
        }) # La impresio de la llista no es reactiva, esta 'isolada'...
    })
    
    #--------------------------
    #GENERACIO GRAFICS, RESUM DADES I INFO PUNT SELECCIONAT
    #--------------------------

    # Grafic ggplot
    output$graficggplot <- renderPlot({
        
        # noms dels camps spc(): 
        #"codpec" "llan" "maquina" "data" "operari" "valor" "limitsup" "limitinf" "caract" "ordfas" "infgrafic" "supgrafic"
        
        input$somhi # L'actualitzacio del grafic esta supeditat a premer el boto 'somhi'
        input$somhi2 # L'actualitzacio del grafic tambe ho esta al boto 'somhi2'
        
        
        if(input$tipusgraf==0){  # opcions en el cas d'histograma. Cas especial per les linies verticals dels limits
            
            if(input$linia){
                # si volem dibuixada la linia, afegim els geom_histogram i geom_density
                isolate(p <- ggplot(data=spc(), aes(x=valor))) #aillem nomes la part que recupera les dades d'spc()
                # la resta, no esta isolat, canvia tan bon punt modifiquem l'ample
                p <- p + geom_histogram(aes(y=..density..), colour="black", fill="lightyellow", bins=input$ample) + geom_density(alpha=.2, fill="#FF6666") 
            } 
            else{ 
                # si no volem linia, nomes afegim geom_histo
                isolate( p <- ggplot(data=spc(), aes(x=valor))) #aillem nomes la part que recupera les dades d'spc()
                # la resta, no esta isolat, canvia tan bon punt modifiquem l'ample
                p <- p + geom_histogram(color="black",fill="lightyellow", bins=input$ample) 
            }  
            
            #nomes podem dibuixar els limits si no hi ha subgrups
            if(input$subgrups==0)   p <- p + geom_vline(aes(xintercept=limitinf),color="grey", linetype="dashed", size=2) + geom_vline(aes(xintercept=limitsup),color="black", linetype="dashed", size=2)

            #eleccio del facet, en funcio dels subgrups escollits
            if(input$subgrups==1) p <- p + facet_grid(llan~.)
            if(input$subgrups==2) p <- p + facet_grid(maquina~.)
            if(input$subgrups==3) p <- p + facet_grid(operari~.)
            
           
            #eleccio del color
            if(input$color==1) p <- p + aes(color=llan) 
            if(input$color==2) p <- p + aes(color=maquina)
            if(input$color==3) p <- p + aes(color=operari)
            
            #afegim els limits dels grafics
            p <- isolate (p + xlim(median(spc()$infgrafic),median(spc()$supgrafic)))
            
        } 
        else {   #opcions en el cas de NO ser histograma
            
            isolate (p <- ggplot(data=spc(), aes(x=data,y=valor)))
            
            #afegim el geom en funcio del tipus de grafic
            if(input$tipusgraf==1)  p <- p + geom_jitter(width = 0.2, height = 0, alpha=0.4, size = 4) 
            if(input$tipusgraf==2)  p <- p + geom_line() 
            if(input$tipusgraf==3)  p <- p + geom_boxplot(notch = TRUE, notchwidth = 0.9, outlier.colour = "red", outlier.shape = 1) 
            
            #nomes podem dibuixar els limits si no hi ha subgrups
            if(input$subgrups==0) p <- p + geom_hline(aes(yintercept=limitinf),color="grey", linetype="dashed", size=2) + geom_hline(aes(yintercept=limitsup),color="black", linetype="dashed", size=2)


            #eleccio del facet, en funcio dels subgrups escollits
            if(input$subgrups==1)  p <- p + facet_grid(~llan)
            if(input$subgrups==2)  p <- p + facet_grid(~maquina)
            if(input$subgrups==3)  p <- p + facet_grid(~operari) 
            
            
            #assignacio del color com a subgrup
            if(input$color==1) p <- p + aes(color=llan)
            if(input$color==2) p <- p + aes(color=maquina)
            if(input$color==3) p <- p + aes(color=operari)

            # si es demana afegim linia de tendencia
            if(input$linia) p <- p + geom_smooth()
            
            # definim els limits del grafic (eix vertical)
            isolate( p <- p + ylim(median(spc()$infgrafic),median(spc()$supgrafic)))
        } 
   
        # afegim titol i canvi de tema
        isolate (p <- p + ggtitle(paste(input$proj, "-",input$fase,". ",nomcar()[[1]], sep="")) + theme_bw())
        
        # dibuixa grafic
        p
        
        
    })#, height = 550)
    
    # Resum de les dades
    output$resumdades <- renderTable({
        
        input$somhi # L'actualitzacio del resum tambe esta supeditat a premer el boto 'somhi'
        input$somhi2 # L'actualitzacio del grafic tambe ho esta al boto 'somhi2'
        
        isolate(resum()) # L'actualitzacio del resum esta tambe 'isolat'...
    },
    include.rownames=FALSE
    )
      
    output$seleccionats <- DT::renderDataTable({
        brushedPoints(spc()[,c(2:6)], input$selecc)
    }) 
    
    #------------------    
    # DESAR GRAFICS i DADES
    #------------------
    
    # Desa grafic temporal al disc
    output$gravagrafic <- downloadHandler(
        filename = function() { paste(input$proj, input$fase, "pdf", sep = ".") },
        content = function(file) {
            ggsave(file, device = "pdf")
        }
        
    )
    output$gravadades <- downloadHandler(
        filename = function() {
            paste(input$nomcsv, "csv", sep = ".") },
        content = function(file) {
            write.csv(spc(), file)
        }
        
    )

}

shinyApp(ui, server)