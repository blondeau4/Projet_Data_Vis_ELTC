#### Prétraitement et nettoyage des données pour le cours de Data-Visualisation


myfile = "https://raw.githubusercontent.com/ErwanHB/BDDSTAT/main/miami-housing.csv"

base_originale = read.csv(myfile)

#changement du nom des variables
colnames(base_originale)[] <- c("latitude","longitude","id","prix","surface_terrain","surface_plancher","valeur_sup","dist_transport","dist_ocean","dist_eau","dist_business_district","dist_centre","dist_autoroute","age_maison","bruit_fort","mois_vente","qualite_structure")

#Conversion feet vers m et feet² vers m²
coef_feet2_m2= 10.764
base_originale$surface_terrain = base_originale$surface_terrain / coef_feet2_m2
base_originale$surface_plancher = base_originale$surface_plancher / coef_feet2_m2
base_originale$dist_transport = base_originale$dist_transport / sqrt(coef_feet2_m2)
base_originale$dist_ocean = base_originale$dist_ocean / sqrt(coef_feet2_m2)
base_originale$dist_eau = base_originale$dist_eau / sqrt(coef_feet2_m2)
base_originale$dist_business_district = base_originale$dist_business_district / sqrt(coef_feet2_m2)
base_originale$dist_centre = base_originale$dist_centre / sqrt(coef_feet2_m2)
base_originale$dist_autoroute = base_originale$dist_autoroute / sqrt(coef_feet2_m2)
base_originale$dist_transport = base_originale$dist_transport / sqrt(coef_feet2_m2)

# VALEURS MANQUANTES
for (var in colnames(base_originale)){
  print(paste('Nombre de valeurs manquantes de',var,":",sum(is.na(base_originale[var]))))
}

# DOUBLONS
paste("Nombre de doublons",nrow(base_originale) - length(unique(base_originale$id)))

doublonId=base_originale[base_originale$id %in% unique(base_originale[duplicated(base_originale$id),])$id,]
doublonId=doublonId[order(doublonId$id,decreasing = TRUE),]


base_miami_ssdoublon = base_originale
base_miami_ssdoublon$id = paste0(base_miami_ssdoublon$id,"_",base_miami_ssdoublon$mois_vente)

paste("Nombre de doublons restants",nrow(base_miami_ssdoublon) - length(unique(base_miami_ssdoublon$id)))
doublonIdMois=base_miami_ssdoublon[base_miami_ssdoublon$id %in% unique(base_miami_ssdoublon[duplicated(base_miami_ssdoublon$id),])$id,]
doublonIdMois=doublonIdMois[order(doublonIdMois$id,decreasing = TRUE),]


base_miami_ssdoublon[195,]$id="3022200021291_9"
base_miami_ssdoublon[196,]$id="3022200021291_9"

base_miami_ssdoublon=base_miami_ssdoublon[duplicated(base_miami_ssdoublon$id,fromLast = TRUE)==F,] 

paste("Nombre de doublons restants",nrow(base_miami_ssdoublon) - length(unique(base_miami_ssdoublon$id)))


#### 
miami <- base_miami_ssdoublon
rm(list = setdiff(ls(), c("miami", "base_originale")))
str(miami)
write.csv(miami, file = "miami.csv", row.names = FALSE)










