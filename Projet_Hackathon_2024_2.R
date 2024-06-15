#################################################
###                                           ###
###     Fonction affichant la longueur        ###
###          et le trajet effectu?            ###
###                                           ###   
#################################################


# INSTALLATION DES PACKAGES NECESSAIRES

# install.packages("tidyverse")
 #install.packages("sf")
# install.packages("ggiraph")
# install.packages("viridis")
# install.packages("units")

# CHARGEMENT DES BIBLIOTHEQUES
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggiraph)
library(dplyr)
library(viridis)
library(units)


# LECTURE DES FICHIERS DE DONNEES

  montpellier_commune = st_read(dsn = "C:/Users/sjaziri/Downloads/montpellier/montpellier", layer = "commune")
  montpellier_carreaux = st_read(dsn = "C:/Users/sjaziri/Downloads/montpellier/montpellier", layer = "carreaux")
  montpellier_espaces_verts = st_read(dsn = "C:/Users/sjaziri/Downloads/montpellier/montpellier", layer = "espacesverts")
  montpellier_bpe = st_read(dsn = "C:/Users/sjaziri/Downloads/montpellier/montpellier", layer = "bpe")

# ISOLEMENT DE LA COMMUNE DE MONTPELLIER POUR CALCULER DES BORDURES DE CARTE #
  
  montpellier_commune_seule <- montpellier_commune %>%
    filter(NOM_COM ==  "MONTPELLIER")
  
# EVALUATION DE LA PROPORTION DE VALEURS MANQUANTES POUR LES DEUX BASES DE DONNEES QUI NOUS INTERESSENT

  valeurs_manquantes_carreaux = colMeans(is.na(montpellier_carreaux)) * 100
  valeurs_manquantes_espaces_verts = colMeans(is.na(montpellier_espaces_verts)) * 100

# SUPPRESSION DES LIGNES QUI ONT DES NA AU NIVEAU DES COORDONNEES OU DE LA GEOMETRIE #
  
  montpellier_carreaux <- montpellier_carreaux %>% filter(!is.na(x) & !is.na(y) & !st_is_empty(geometry))
  
  montpellier_espaces_verts <- montpellier_espaces_verts %>% filter(!st_is_empty(geometry))
  
# AJOUT DES TYPES ET DES SURFACES DES ESPACES VERTS #
  
  montpellier_espaces_verts <- montpellier_espaces_verts %>%
    mutate( surface = st_area(geometry),
            TYPEEV = case_match(sub("(\\d+).*", "\\1", ev_typolog),
                                         c("1", "19", "9") ~ "Espaces verts publics",
                                         .default = "Autres espaces verts"))
  
  
  
#################################################
###                                           ###
###        STATISTIQUES DESCRIPTIVES          ###
###                                           ###   
#################################################
  
 
  summary(montpellier_carreaux)
  summary(montpellier_espaces_verts)
  
### NOMBRE D'INDIVIDUS PAR CARREAU  ###

  montpellier_carreaux = montpellier_carreaux %>%
  mutate(index = row_number())
    
  # ON CALCUL LE 90Ã¨me CENTILE POUR LE NOMBRE D'INDIVIDUS AFIN D'AMELIORER LA VISUALISATION ET EVITER L'INFLUENCES DES VALEURS EXTREMES
  max_individu = quantile(montpellier_carreaux$Ind, 0.90, na.rm = TRUE)
    
  # CREATION DU GRAPHIQUE INTERACTIF
  interactive_plot = ggplot() +
    scale_fill_viridis_c(limits = c(0, max_individu), oob = scales::squish) +
    geom_sf(data = montpellier_carreaux, fill = NA, color = "black") +
    geom_sf_interactive(data = montpellier_carreaux,
                          aes(fill = pmin(Ind, max_individu), tooltip = paste("Index:", index, "<br>Nombre d'individus:", Ind), data_id = index),
                          color = "white", size = 0.2) +
    ggtitle("Nombre d'individus par carreau ? Montpellier") +
    coord_sf(crs = 2154, xlim = st_bbox(montpellier_carreaux)[c(1, 3)], ylim = st_bbox(montpellier_carreaux)[c(2, 4)]) +
    labs(fill = "Nombre d'individus par carreau") 
    
  # AFFICHAGE DU GRAPHIQUE INTERACTIF
  girafe(ggobj = interactive_plot)
  
### NOMBRE D'INDIVIDUS PAR MENAGE  ###  

  # ON CALCULE LE NOMBRE DE PERSONNES PAR MENAGE
  montpellier_carreaux = montpellier_carreaux %>%
  mutate(nb_personnes_par_menage = ifelse(Men > 0, Ind / Men, NA))
  
  # CREATION DU GRAPHIQUE INTERACTIF
  interactive_plot = ggplot() +
    geom_sf(data = montpellier_commune, fill = NA, color = "black") +
    geom_sf_interactive(data = montpellier_carreaux,
                        aes(fill = nb_personnes_par_menage, tooltip = paste("Index:", index, "<br>Nombre de personnes par mÃ©nage:", round(nb_personnes_par_menage, 2)), data_id = index),
                        color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "C", name = "Nb de personnes par mÃ©nage") +
    ggtitle("Nombre de personnes par mÃ©nage par carreau Ã  Montpellier") +
    coord_sf(crs = 2154, xlim = st_bbox(montpellier_commune)[c(1, 3)], ylim = st_bbox(montpellier_commune)[c(2, 4)]) +
    labs(fill = "Nombre de personnes par mÃ©nage par carreau Ã  Montpellier") 
  
  # AFFICHAGE DU GRAPHIQUE INTERACTIF
  girafe(ggobj = interactive_plot)

### LA SURFACE PAR INDIVIDU ###    
  
  # ON CALCULE LA SURFACE PAR INDIVIDU
  montpellier_carreaux <- montpellier_carreaux %>%
  mutate(surface_par_personne = ifelse(Ind > 0, Men_surf / Ind, NA))
  
  # CREATION DU GRAPHIQUE INTERACTIF POUR LA SURFACE PAR PERSONNE
  interactive_plot = ggplot() +
    geom_sf(data = montpellier_commune, fill = NA, color = "black") +
    geom_sf_interactive(data = montpellier_carreaux,
                        aes(fill = surface_par_personne, tooltip = paste("Index:", index, "<br>Surface par personne:", round(surface_par_personne, 2), "m?"), data_id = index),
                        color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "C", name = "Surface par individu (m?)") +
    
    ggtitle("Surface par individu par carreau Ã  Montpellier") +
    coord_sf(crs = 2154, xlim = st_bbox(montpellier_carreaux)[c(1, 3)], ylim = st_bbox(montpellier_commune)[c(2, 4)]) +
    labs(fill = "Surface par individu par carreau ? Montpellier") 
    
  # AFFICHAGE DU GRAPHIQUE INTERACTIF
  girafe(ggobj = interactive_plot)
  
### ON RAJOUTE DE NOUVELLES VARIABLES A NOTRE BASE DE DONNEE CARREAUX ###
  
  montpellier_carreaux <- montpellier_carreaux %>% 
    mutate(
           Enfants =  Ind_0_3+Ind_4_5+Ind_6_10,
           surface_par_ind = Men_surf / Ind,
           index = row_number(),
           nb_personnes_par_menage = ifelse(Men > 0, Ind / Men, NA),
           Autres_usagers = Ind_11_17 + Ind_18_24 + Ind_25_39 + Ind_40_54 + Ind_55_64 + Ind_65_79 + Ind_80p + Ind_inc,
           prop_enfants = Enfants / Ind,
           prop_autres_usagers = Autres_usagers / Ind
            ) 
  
  # CARTE INTERACTIVE POUR LES ENFANTS
  interactive_plot_enfants <- ggplot() +
    geom_sf_interactive(data = montpellier_carreaux, 
                        aes(fill = prop_enfants, 
                            tooltip = paste("Proportion d'Enfants :", round(prop_enfants, 2))),
                        color = NA) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "Proportion d'enfants par carreau",
         fill = "Proportion") +
    theme(plot.title = element_text(hjust = 0.5))
  
  girafe(ggobj = interactive_plot_enfants, width_svg = 10, height_svg = 8)
  
  # CARTE INTERACTIVE POUR LES AUTRES USAGERS
  interactive_plot_autres <- ggplot() +
    geom_sf_interactive(data = montpellier_carreaux, 
                        aes(fill = prop_autres_usagers, 
                            tooltip = paste("Proportion Autres Usagers:", round(prop_autres_usagers, 2))),
                        color = NA) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "Proportion des autres usagers par carreau",
         fill = "Proportion") +
    theme(plot.title = element_text(hjust = 0.5))
  
  girafe(ggobj = interactive_plot_autres, width_svg = 10, height_svg = 8)

### ANALYSE DE L'ACCESSIBILITE ###
  
  # ON CALCULER LES CENTROÃ¯DES DES "CARREAUX"
  carreaux_centroids <- st_centroid(montpellier_carreaux)
  
  # ENSUITE ON CALCULE LA DISTANCE ENTRE CHAQUE CENTROÃ¯DE DE 'CARREAU' ET 'L'ESPACE VERT' LE PLUS PROCHE
  distances <- st_distance(carreaux_centroids, montpellier_espaces_verts)
  distance_plus_proche <- apply(distances, 1, min, na.rm = TRUE)
  
  # ON AJOUTE LA DISTANCE LA PLUS PROCHE A NOTRE BASE DE DONNEE 'CARREAUX'
  montpellier_carreaux$espace_vert_plus_proche <- distance_plus_proche
  
  # RESUME DES DISTANCES LES PLUS PROCHES
  summary(montpellier_carreaux$espace_vert_plus_proche)
  
  # ON EXTRAIT LES ESPACES VERTS LES PLUS PROCHES
  indexes <- apply(distances, 1, which.min)
  espace_vert_plus_proche <- montpellier_espaces_verts[indexes,]
  
  # CREEATION D'UNE CARTE AVEC LES "CARREAUX" ET LES "ESPACES VERTS" LES PLUS PROCHES
   interactive_plot <- ggplot() +
     geom_sf_interactive(data = montpellier_carreaux, 
                         aes(fill = espace_vert_plus_proche, tooltip = paste("Distance:", espace_vert_plus_proche, "m")), 
                         color = NA) +
     geom_sf_interactive(data = espace_vert_plus_proche, 
                         aes(tooltip = "Espace Vert le Plus Proche"), 
                         color = 'green', shape = 17, size = 3) +
     scale_fill_viridis_c() +
     theme_minimal() +
     labs(title = "Carte des Carreaux et des Espaces Verts les Plus Proches",
          fill = "Distance au Espace Vert le Plus Proche (m?tres)") +
     theme(plot.title = element_text(hjust = 0.5))
   
   # AFFICHAGE DU GRAPHIQUE INTERACTIF
   girafe(ggobj = interactive_plot, width_svg = 10, height_svg = 8)
  
   # HISTOGRAMME DE L'ACCESSIBILITÃ© AUX ESPACES VERTS
   histogram_plot <- ggplot(montpellier_carreaux, aes(x = espace_vert_plus_proche)) +
     geom_histogram(binwidth = 50, fill = 'blue', color = 'black') +
     theme_minimal() +
     labs(title = "Histogramme de l'AccessibilitÃ© aux Espaces Verts",
          x = "Distance ? l'Espace Vert le Plus Proche (mÃ¨tres)",
          y = "FrÃ©quence") +
     theme(plot.title = element_text(hjust = 0.5))
   
   print(histogram_plot)

   
   
### CALCUL DE LA DEMANDE ###
   
   coef_enfants <- (31/11)/(69/89)
   
   montpellier_carreaux <- montpellier_carreaux %>%
     mutate(
       # On suppose une formule de la demande qui dÃ©pend de 4 paramÃ¨tres : population, nombre d'enfants, nombre de personnes par mÃ©nage et surface par personne
       demande = (coef_enfants*Enfants + Autres_usagers) * log(1 + nb_personnes_par_menage) / log(1 + surface_par_personne)
     )
   
   montpellier_carreaux <- montpellier_carreaux %>%
     mutate(ln_demande = log(demande))
   
   
### CALCUL DE L'OFFRE D'ESPACES VERTS ###
   
   distances <- st_distance(montpellier_carreaux, montpellier_espaces_verts)
   
   seuil <- 1000 # Distance en dessous de laquelle la proximitÃ© n'influence pas la 
   
   distances_seuil <- matrix(data = pmax(as.numeric(distances), seuil), ncol = nrow(montpellier_espaces_verts))
   
   ratios_demande_distance <- montpellier_carreaux$demande / distances_seuil
   
   sommes_ratios_demandes_distance <- apply(ratios_demande_distance, MARGIN = 2, FUN = sum)
   
   utilites_espaces_verts_carreaux <- as.numeric(montpellier_espaces_verts$surface)/(distances_seuil * sommes_ratios_demandes_distance)
   
   montpellier_carreaux <- montpellier_carreaux %>%
     mutate(offre = apply(utilites_espaces_verts_carreaux, MARGIN = 1, FUN = sum))
   
### RATIO DEMANDE/OFFRE ###
   
   montpellier_carreaux <- montpellier_carreaux %>%
     mutate(ratio = demande / offre,
            ln_ratio = log(1 + demande / offre))
   
### R shiny
   ui <- fluidPage(
     titlePanel("Visualisation des indices sur la métropole de Montpellier"),
     sidebarLayout(
       sidebarPanel(
         selectInput("variable", "Sélection de variables :",
                     choices = c("Nombre d'enfants" = "Enfants",
                                 "Nombre d'individus" = "Ind",
                                 "Nombre des personnes par menage" = "nb_personnes_par_menage",
                                 "Demande" = "ln_demande",
                                 "Offre" = "offre",
                                 "Ratio demande/offre"= "ln_ratio"),
                     selected = "enfants")
       ),
       mainPanel(
         leafletOutput("map", width = "100%", height = "800px")
       )
     )
   )
   
   # Definer un serveur
   serveur <- function(input, output) {
     output$map <- renderLeaflet({
       # s'assurer que la base est télechargée correctement
       if (exists("montpellier_commune") && exists("montpellier_carreaux") &&
           exists("montpellier_espaces_verts")&& exists("montpellier_commune_seule")){
         
         
         
         # Montpellier carreaux 
         montpellier_carreaux <- montpellier_carreaux %>%
           mutate(
             Enfants = Enfants,
             nb_personnes_par_menage = nb_personnes_par_menage,
             offre = offre,
             Ind = Ind,
             ln_demande=ln_demande,
             ln_ratio=ln_ratio
            
                )
         
         # vérification des type de géométries 
         print(st_geometry_type(montpellier_commune))
         print(st_geometry_type(montpellier_carreaux))
         print(st_geometry_type(montpellier_espaces_verts))
         
         ####
         montpellier_commune <- st_transform(montpellier_commune, crs = 4326)
         montpellier_carreaux <- st_transform(montpellier_carreaux, crs = 4326)
         montpellier_espaces_verts <- st_transform(montpellier_espaces_verts, crs = 4326)
         montpellier_commune_seule <- st_transform(montpellier_commune_seule, crs = 4326)
         
         # Selection de variable
         variable_a_afficher <- input$variable
         palette <- colorNumeric(palette = "viridis", domain = montpellier_carreaux[[variable_a_afficher]])
         
         # Creation de map
         leaflet() %>%
           addProviderTiles(providers$CartoDB.Positron) %>%
           addPolygons(data = montpellier_commune,
                       fillColor = "transparent",
                       color = "#444444",
                       weight = 2,
                       opacity = 1) %>%
           addPolygons(data = montpellier_carreaux,
                       fillColor = ~palette(montpellier_carreaux[[variable_a_afficher]]),
                       color = "#444444",
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 0.7,
                       label = ~paste0(round(montpellier_carreaux[[variable_a_afficher]], 2)),
                       labelOptions = labelOptions(direction = "auto")) %>%
           addPolygons(data = montpellier_espaces_verts,
                       fillColor = ~colorFactor(palette = "viridis", domain = montpellier_espaces_verts$TYPEEV)(TYPEEV),
                       color = "#444444",
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 0.7,
                       popup = ~paste0("Type: ", TYPEEV)) %>%
           setView(lng = mean(st_bbox(montpellier_commune_seule)[c(1, 3)]),
                   lat = mean(st_bbox(montpellier_commune_seule)[c(2, 4)]),
                   zoom = 12) %>%
           addLegend("bottomright", pal = palette, values = montpellier_carreaux[[variable_a_afficher]],
                     title = input$variable,
                     opacity = 1) %>%
           addLegend("topright", pal = colorFactor(palette = "viridis", domain = montpellier_espaces_verts$TYPEEV),
                     values = montpellier_espaces_verts$TYPEEV,
                     title = "Types d'Espaces Verts",
                     opacity = 1)
       } else {
         leaflet() %>%
           addTiles() %>%
           addPopups(lng = 0, lat = 0, popup = "Data n'est pas téléchargée correctement. Veuillez vérifiez votre base.")
       }
     })
   }
   
   # afficher
   shinyApp(ui = ui, server = serveur)
   
   
   
   
   
   
   
   
