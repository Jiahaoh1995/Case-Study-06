#### Case Study 06 ####
#######################


### 1. Datenimport ### 

# Laden der notwendigen Pakete
if(!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(stringr)) {
  install.packages("stringr")
  require(stringr)
}

library(dplyr)
library(magrittr)


## Ordner "Einzelteil" ##

# Auflisten, welche Dateien in dem Ordner gespeichert sind.
Einzelteil_ordner <- list.files(path = "Data/Einzelteil")
Einzelteil_ordner

# Pfadangabe:
Einzelteil_pfad <- file.path("Data/Einzelteil",Einzelteil_ordner)

# Benötigt wird nur das Einzelteil T14.
Einzelteil_T14 <- read_csv2(Einzelteil_pfad[14], col_names = TRUE)

# Inhalt/Aufbau anschauen.
str(Einzelteil_T14)
summary(Einzelteil_T14)


## Ordner "Fahrzeug" ##

# Auflisten, welche Dateien in dem Ordner gespeichert sind.
Fahrzeug_ordner <- list.files(path = "Data/Fahrzeug")
Fahrzeug_ordner

# Pfadangabe
Fahrzeug_pfad <- file.path("Data/Fahrzeug",Fahrzeug_ordner)

# Fahrzeugbestandteile einlesen und abspeichern in Variablen.
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv2(Fahrzeug_pfad[1], col_names = TRUE)
Bestandteile_Fahrzeuge_OEM1_Typ12 <- read_csv2(Fahrzeug_pfad[2], col_names = TRUE)
Bestandteile_Fahrzeuge_OEM2_Typ21 <- read_csv2(Fahrzeug_pfad[3], col_names = TRUE)
Bestandteile_Fahrzeuge_OEM2_Typ22 <- read_csv2(Fahrzeug_pfad[4], col_names = TRUE)

# Inhalt/Aufbau anschauen.
summary(Bestandteile_Fahrzeuge_OEM1_Typ11)
summary(Bestandteile_Fahrzeuge_OEM1_Typ12)
summary(Bestandteile_Fahrzeuge_OEM2_Typ21)
summary(Bestandteile_Fahrzeuge_OEM2_Typ22)

# Fahrzeuginformationen einlesen und abspeichern in Variablen.
# read_csv2 führt zum inkorrekten Laden des Inhaltes, wegen anderem Trennzeichen.
Fahrzeuge_OEM1_Typ11 <- read_csv(Fahrzeug_pfad[5], col_names = TRUE)
Fahrzeuge_OEM1_Typ12 <- read_csv2(Fahrzeug_pfad[6], col_names = TRUE)
Fahrzeuge_OEM2_Typ21 <- read_csv(Fahrzeug_pfad[7], col_names = TRUE)
Fahrzeuge_OEM2_Typ22 <- read_csv2(Fahrzeug_pfad[8], col_names = TRUE)

# Inhalt/Aufbau anschauen.
summary(Fahrzeuge_OEM1_Typ11)
summary(Fahrzeuge_OEM1_Typ12)
summary(Fahrzeuge_OEM2_Typ21)
summary(Fahrzeuge_OEM2_Typ22)


## Ordner "Geodaten" ##

# Auflisten, welche Dateien in dem Ordner gespeichert sind.
Geodaten_ordner <- list.files(path = "Data/Geodaten")
Geodaten_ordner

# Pfadangabe:
Geodaten_pfad <- file.path("Data/Geodaten",Geodaten_ordner)

# Benötigt werden Daten zu den Gemeinden aus Datei 1.
Geodaten_Gemeinden <- read_csv2(Geodaten_pfad[1], col_names = TRUE)

# Inhalt/Aufbau anschauen.
str(Geodaten_Gemeinden)
summary(Geodaten_Gemeinden)


## Ordner "Komponenten" ##

# Auflisten, welche Dateien in dem Ordner gespeichert sind.
Komponente_ordner <- list.files(path = "Data/Komponente")
Komponente_ordner

# Pfadangabe
Komponente_pfad <- file.path("Data/Komponente",Komponente_ordner)

# Nur K2LE1 enthält T14, deshalb bnötigen wir nur K2LE1.
Bestandteile_Komponente_K2LE1 <- read_csv2(Komponente_pfad[5], col_names = TRUE)

# Inhalt/Aufbau anschauen.
summary(Bestandteile_Komponente_K2LE1)



## Ordner "Zulassungen" ##

# Auflisten, welche Dateien in dem Ordner gespeichert sind.
Zulassungen_ordner <- list.files(path = "Data/Zulassungen")
Zulassungen_ordner

# Pfadangabe
Zulassungen_pfad <- file.path("Data/Zulassungen",Zulassungen_ordner)

# Eine Datei, die die Zulassungsdaten aller Fahrzeuge enthält.
Zulassungen_alle_Fahrzeug <- read_csv2(Zulassungen_pfad, col_names = TRUE)

# Inhalt/Aufbau anschauen.
str(Zulassungen_alle_Fahrzeug)
summary(Zulassungen_alle_Fahrzeug)



### 2. Bearbeiten der Daten ###

## Einzelteil T14 ##
 T14_info <- Einzelteil_T14 %>%
  select(ID_T14, 
         Herstellernummer_T14 = Herstellernummer, 
         Werksnummer_T14 = Werksnummer, 
         Fehlerhaft_T14 = Fehlerhaft
  ) %>%
  filter(
    Herstellernummer_T14 == 213
  )

# Gibt es NA-Einträge?
any(is.na(T14_info)) # nein

# Inhalt/Aufbau anschauen.
summary(T14_info)

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(Einzelteil_T14)

# Datensatz "T14_info" enthält Daten zu allen Sitzbezugen T14, die vom
# Werk/Unternehmen "213" produziert wurden.
# Über die Komponentendaten können die Einzelteile den Sitzbezugen zugeordnet
# werden. Über die Sitzes-ID's diese wiederum zu den Fahrzeugen und damit
# den Zulassungen und Geodaten.
# In welchen Komponenten sind die Einzelteile verbaut? 

## Komponenten ##

# Gibt es NA-Einträge?
any(is.na(Bestandteile_Komponente_K2LE1)) # nein

# Zusammenfassung des Datensatzes:
summary(Bestandteile_Komponente_K2LE1)

# Wir benötigen nur ID_T14 und ID_K2LE1.
K2LE1_info <- Bestandteile_Komponente_K2LE1 %>%
  select(
    ID_T14,
    ID_Sitze = ID_K2LE1
  ) 

# Gibt es NA-Einträge?
any(is.na(K2LE1_info)) # nein

# Inhalt/Aufbau anschauen.
summary(K2LE1_info)

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(Bestandteile_Komponente_K2LE1)


# In welchen Fahrzeugen sind diese Sitzbezugen verbaut?

## Fahrzeug ##
# Zusammenfassung des Datensatzes von Typ 11 12 21 22 durch ID_Fahrzeug
# Typ11
OEM1_Typ11 <- Fahrzeuge_OEM1_Typ11 %>% 
  select(
    ID_Fahrzeug,
    Herstellernummer_Fahrzeug = Herstellernummer,
    Werksnummer_Fahrzeug = Werksnummer,
    Fehlerhaft_Fahrzeug = Fehlerhaft
  ) %>% 
  left_join(
    select(Bestandteile_Fahrzeuge_OEM1_Typ11,
           ID_Sitze,
           ID_Fahrzeug
    ),
    by = "ID_Fahrzeug"
  )
any(is.na(OEM1_Typ11)) # nein
summary(OEM1_Typ11)

# Typ12
OEM1_Typ12 <- Fahrzeuge_OEM1_Typ12 %>% 
  select(
    ID_Fahrzeug,
    Herstellernummer_Fahrzeug = Herstellernummer,
    Werksnummer_Fahrzeug = Werksnummer,
    Fehlerhaft_Fahrzeug = Fehlerhaft
  ) %>% 
  left_join(
    select(Bestandteile_Fahrzeuge_OEM1_Typ12,
           ID_Sitze,
           ID_Fahrzeug
    ),
    by = "ID_Fahrzeug"
  )
any(is.na(OEM1_Typ12)) # nein
summary(OEM1_Typ12)

# Typ21
OEM2_Typ21 <- Fahrzeuge_OEM2_Typ21 %>% 
  select(
    ID_Fahrzeug,
    Herstellernummer_Fahrzeug = Herstellernummer,
    Werksnummer_Fahrzeug = Werksnummer,
    Fehlerhaft_Fahrzeug = Fehlerhaft
  ) %>% 
  left_join(
    select(Bestandteile_Fahrzeuge_OEM2_Typ21,
           ID_Sitze,
           ID_Fahrzeug
    ),
    by = "ID_Fahrzeug"
  )
any(is.na(OEM2_Typ21)) # nein
summary(OEM2_Typ21)

# Typ22
OEM2_Typ22 <- Fahrzeuge_OEM2_Typ22 %>% 
  select(
    ID_Fahrzeug,
    Herstellernummer_Fahrzeug = Herstellernummer,
    Werksnummer_Fahrzeug = Werksnummer,
    Fehlerhaft_Fahrzeug = Fehlerhaft
  ) %>% 
  left_join(
    select(Bestandteile_Fahrzeuge_OEM2_Typ22,
           ID_Sitze,
           ID_Fahrzeug
    ),
    by = "ID_Fahrzeug"
  )
any(is.na(OEM2_Typ22)) # nein
summary(OEM2_Typ22)

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(Bestandteile_Fahrzeuge_OEM1_Typ12)
rm(Fahrzeuge_OEM1_Typ12)
rm(Bestandteile_Fahrzeuge_OEM2_Typ22)
rm(Fahrzeuge_OEM2_Typ22)
rm(Bestandteile_Fahrzeuge_OEM1_Typ11)
rm(Fahrzeuge_OEM1_Typ11)
rm(Bestandteile_Fahrzeuge_OEM2_Typ21)
rm(Fahrzeuge_OEM2_Typ21)

# Zusammenfügen in einen Datensatz.
OEM_Alle <- bind_rows(OEM1_Typ11, OEM1_Typ12, OEM2_Typ21, OEM2_Typ22)

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(OEM1_Typ11)
rm(OEM1_Typ12)
rm(OEM2_Typ21)
rm(OEM2_Typ22)


## Zulassung ##


## Informationen zu bisherigen Daten hinzufügen ##

# Hinzufügen der Informationen von Komponenten an den Einzelteil-Datensatz.
T14_plus_K2LE1 <- T14_info %>%
  left_join(
    K2LE1_info,
    by = "ID_T14"
  )
# Gibt es NA-Einträge?
any(is.na(T14_plus_K2LE1)) # nein
# Inhalt/Aufbau anschauen.
summary(T14_plus_K2LE1)
# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(T14_info)
rm(K2LE1_info)


# Welche Fahrzeuge hat Sitzbezug T14 gebaut?
T14_K2LE1_Fahrzeug_Alle <- OEM_Alle %>%
  left_join(
    T14_plus_K2LE1,
    by = "ID_Sitze"
  )

# Gibt es NA-Einträge?
any(is.na(T14_K2LE1_Fahrzeug_Alle)) #ja

# Inhalt/Aufbau anschauen.
summary(T14_K2LE1_Fahrzeug_Alle)

# NA's entfernen
T14_K2LE1_Fahrzeug <- na.omit(T14_K2LE1_Fahrzeug_Alle)

# Gibt es NA-Einträge?
any(is.na(T14_K2LE1_Fahrzeug)) #nein

# Inhalt/Aufbau anschauen.
summary(T14_K2LE1_Fahrzeug)

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(OEM_Alle)
rm(T14_plus_K2LE1)
rm(T14_K2LE1_Fahrzeug_Alle)


# Wo wurden die Fahrzeuge zugelassen?
Datensatz_Zulassungen <- T14_K2LE1_Fahrzeug %>% 
  left_join(
    select(Zulassungen_alle_Fahrzeug,
           ID_Fahrzeug = IDNummer,
           Zulassung_Datum = Zulassung,
           Zulassung_Gemeinde = Gemeinden
    ),
    by = "ID_Fahrzeug"
  )

# Gibt es NA-Einträge?
any(is.na(Datensatz_Zulassungen)) # nein

# Inhalt/Aufbau anschauen.
summary(Datensatz_Zulassungen)

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(T14_K2LE1_Fahrzeug)
rm(Zulassungen_alle_Fahrzeug)


# Geodaten für die Zulassungs-Gemeinden hinzufügen.
Datensatz_Ausfuehrlich <- Datensatz_Zulassungen %>% 
  left_join(
    select(Geodaten_Gemeinden,
           Zulassung_Gemeinde = Gemeinde,
           Postleitzahl,
           Laengengrad,
           Breitengrad),
    by = "Zulassung_Gemeinde"
  ) 
 
# Gibt es NA-Einträge? 
any(is.na(Datensatz_Ausfuehrlich)) # ja

# Zusammenfassung des Datensatzes:
summary(Datensatz_Ausfuehrlich)

# NA's entfernen
Datensatz_Ausfuehrlich_sauber <- na.omit(Datensatz_Ausfuehrlich)

# Gibt es NA-Einträge? 
any(is.na(Datensatz_Ausfuehrlich_sauber)) # nein

# Zusammenfassung des Datensatzes:
summary(Datensatz_Ausfuehrlich_sauber)

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(Geodaten_Gemeinden)
rm(Datensatz_Zulassungen)
rm(Datensatz_Ausfuehrlich)


## Datensatz erstellen
Datensatz_Analyse <- Datensatz_Ausfuehrlich_sauber %>% 
  select(
    ID_T14,
    ID_Sitze,
    ID_Fahrzeug,
    Werksnummer_Fahrzeugtyp,
    Zulassung_Datum,
    Zulassung_Gemeinde,
    Postleitzahl,
    Laengengrad,
    Breitengrad
  ) 

# Zusammenfassung des Datensatzes:
summary(Datensatz_Analyse)

# Datensatz speichern im RData-Format.
save(Datensatz_Analyse, file = "Finaler_Datensatz_06.RData")

# Löschen der nicht mehr genutzten Variablen/Datensätze, um Speicherkapazität hoch zu halten.
rm(Datensatz_Ausfuehrlich_sauber)






### 3. App ###

# Notwendigen Pakete laden.
if(!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}

if(!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if(!require(shinyjs)) {
  install.packages("shinyjs")
  require(shinyjs)
}

if(!require(plotly)) {
  install.packages("plotly")
  require(plotly)
}

if(!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if(!require(leaflet.extras)) {
  install.packages("leaflet.extras")
  require(leaflet.extras)
}

if(!require(ggsci)) {
  install.packages("ggsci")
  require(ggsci)
}

#Einlesen des zurvor erstellten Datensatzes aus dem Analyseprozess.
Datensatz <- get(load("Finaler_Datensatz_06.RData"))
#Begrenzung für Slider
erstes <- min(Datensatz$Zulassung_Datum)
letztes <- max(Datensatz$Zulassung_Datum)

# Funktion zur Definition von Farben für den entsprechenden Autotyp. Diese werden in der Leaflet-Karte als Markerfarben genutzt.
getColor <- 'blue'

# Auswahl eines Icons als Marker auf der Karte
icons <- awesomeIcons(
  icon = 'ion-android-car',
  iconColor = 'white',
  library = 'ion',
  markerColor = getColor
)

#User Interface: Benutzeroberfläche, Input
ui <- fluidPage(
  useShinyjs(),
  
  fluidRow(
    
    column(6,
           # Titel
           titlePanel(
             br(),
             title = "CASE STUDY 06" 
           ),
           h4("Identify popularity of leather cover T14 in Germany"),
           h5("seat cover mancufacturer 213")
    ),
    column(6,
           h6("TU Berlin", align = "right"),
           h6("Institut für Werkzeugmaschinen und Fabrikbetrieb", align = "right"),
           h6("Introduction to Engineering Data Analytics with R", align = "right"),
           h6("SS19, 22.08.2019", align = "right")
    )
  ),
  
  br(),
  
  fluidRow(
    # Überschrift für Graph
    h3(" Monthly registration of vehicles ", align = "center"),
    
    column(4,
           
           br(),
           br(),
           br(),
           
           #Seitenleiste Datenauswahl
           wellPanel(
             
             #Gemeinde/n auswählbar, in der/denen Fahrzeuge zugelassen wurden
             selectizeInput(
               inputId = "vehicle",
               label = "Vehicle type",
               choices = sort(unique(Datensatz$Werksnummer_Fahrzeug))
             ),
             
             #Zeitraum auswählen des Zulassungsverlaufs in gewählter/n Gemeinde/n
             sliderInput(
               inputId = "zeitraum",
               label = "Time",
               min = as.Date(erstes),
               max = as.Date(letztes),
               value = c(as.Date(erstes), as.Date(letztes)),
               timeFormat = "%m %Y"
             ),
             
             #Update Button zum Aktualisieren des Main Panel nach Ändern der Datenauswahl
             actionButton(
               inputId = "update",
               label = "Update"
             )
           )
    ),
    
    #Hauptfenster Output
    column(8,
           
           #Graph zeitlicher Zulassungsverlauf für gewählte Gemeinde
           plotlyOutput(
             outputId = "graph"
           )
    ),
    
    br(),
    
    column(12,  
           tabsetPanel(
             
             #zugehörige Tabelle mit Daten zum Graph
             tabPanel("Daten Graph",
                      dataTableOutput(
                        outputId = "tabelle"
                      )
             ),
             
             # gesamter Datensatz
             tabPanel("Daten Gesamt",
                      dataTableOutput(
                        outputId = "daten_gesamt"
                      )
             )
           )
    ),
    
    br(),
    br(),
    
    # Karte
    column(12,
           # Überschrift für Karte
           br(),
           h3("Regional distribution of vehicle registrations", align = "center"),
           br(),  
           
           # Kartengröße an Fenstergröße anpassen.
           tags$style(type = "text/css", "#karte {height: calc(100vh - 120px) !important;}"),
           
           title = "Karte",
           
           leafletOutput(
             outputId = "karte"),
           
           br(),
           hr()
    )
  )
)


#Darstellungsinhalt und Anweisung: Output mit Input von ui
server <- function(input, output, session) {
  
  # Datenaufruf nach Aktualisierung
  daten_nach_input <- reactive({
    
    # Aktualisierungs-Button
    input$update
    
    isolate({
      
      # Filtern der Daten nach eingegebener Fahrzeugtype.
      daten <- filter(Datensatz,
                      Werksnummer_Fahrzeug == input$vehicle)
      
   
    })
  })
  
  # Datensatz für Heatmap variabel durch Slider.
  daten_filter <- reactive({
    input$update
    isolate({
      daten <- filter(Datensatz,
                      Werksnummer_Fahrzeug == input$vehicle)
    })
  })
  
  # Statischer Teil der Karte, wird nur einmal geladen.
  output$karte <- renderLeaflet({
    
    leaflet(Datensatz) %>%
      addProviderTiles("Stamen.TonerLite", 
                       group = "Simpel (Standard)", 
                       options = providerTileOptions(minZoom = 4, maxZoom = 13)) %>%
      addTiles(group = "Open Street Map") %>%
      
      # Konzentration des Zooms auf die Koordinaten im Datensatz.
      fitBounds(
        ~min(Laengengrad)-1, 
        ~min(Breitengrad)-1, 
        ~max(Laengengrad)+1, 
        ~max(Breitengrad)+1) 
  })
  
  #Output1 Graph:  # zeitlicher Zulassungsverlauf der Fahrzeuge 
  output$graph <- renderPlotly({
    
    # Berechnung der Zulassungen pro Monat.
    Datensatz %>% mutate(Zulassung_Datum = ymd(Zulassung_Datum),
                       Year = Zulassung_Datum%>%year(),
                       Month = Zulassung_Datum %>% month()) %>%
      filter(Zulassung_Datum >= input$zeitraum[1]&Zulassung_Datum<=input$zeitraum[2]) %>%
      group_by(Year,Month,Werksnummer_Fahrzeug) %>%summarise(ZD_n=n()) %>%
      
      # # Output Graph Zulassungen pro Monat.
      ggplot(aes(x =factor(Month),y=ZD_n,
                 fill=factor(Werksnummer_Fahrzeug))) + 
      geom_bar(stat = "identity") +facet_grid(Year~.)+
      
      # Achsenbeschriftung.
      labs(x="Time",
           y="The monthly registration of vehicles",
           fill="Vehicle type") +
      theme(axis.text = element_text(size=7),
            axis.title = element_text(size=10),
            legend.title = element_text(size=9),
            legend.text = element_text(size=8)) +
      scale_fill_d3()  
    
  })
  
  #Output2 Tabelle: Daten zum Graph
  output$tabelle <- renderDataTable(
    
    # Aktualisierte Daten.
    select(
      daten_nach_input(), 
      1:6),
    
    # Veränderung des default wie viele Einträge der Tabelle angezeigt werden.
    options = list(
      pageLength = 5, 
      lengthMenu = c(5, 10, 15, -1)
    )
  )
  
  #Output3 Tabelle: Datensatz gesamt.
  output$daten_gesamt <- renderDataTable(
    
    Datensatz[order(Datensatz$Zulassung_Gemeinde),],
    
    options = list(
      pageLength = 5, 
      lengthMenu = c(5, 10, 15, -1)
    )
  )
  
  # Output4 Heatmap: zum gesamten Datensatz
  observe({
    
    leafletProxy("karte", data = daten_filter()) %>%
      
      # Löschen von vorherigen Ausgaben, wenn Daten verändert wurden.
      clearShapes() %>%
      clearPopups() %>%
      clearMarkers() %>%
      clearMarkerClusters %>%
      clearHeatmap %>%
      
      # Heatmap wird in ein extra Layer auf statische Karte gelegt mit im Datensatz vorhandenen Koordinaten.
      addHeatmap(lng = ~Laengengrad, 
                 lat = ~Breitengrad, 
                 max = .6,
                 group = "Heatmap",
                 blur = 60) %>%
      
      # Markereinstellungen.
      addAwesomeMarkers(
        lng = ~Laengengrad, 
        lat = ~Breitengrad,
        icon = icons,
        
        # Popup zeigt Zulassungsort, Postleitzahl und Fahrzeug_ID.
        popup = ~paste(
          "<b>Zulassungsort: </b>", Zulassung_Gemeinde, "<br>",
          "<b>Postleitzahl: </b>", Postleitzahl, "<br>",
          "<b>Fahrzeug: </b>", ID_Fahrzeug, "<br>"),
        
        # Für eine bessere Übersicht, werden die Marker in Cluster zusammengefasst.
        clusterOptions = markerClusterOptions(),
        group = "Detailliert"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Simpel (Standard)", "Open Street Map"),
        overlayGroups = c("Detailliert", "Heatmap"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      # Heatmap wird beim Start nicht angezeigt.
      hideGroup(group = "Heatmap")
    
  })
  
}


#App aufrufen
shinyApp(ui = ui, server = server)


