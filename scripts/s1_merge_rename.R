########################################Ffichiers des codes et trajectoires
codes <- read.csv(paste0(inp_dir,"CON_Codage.csv"),fileEncoding = "ISO-8859-1")
dd    <- read.csv(paste0(inp_dir,"CON_Codage_trajectoire.csv"),fileEncoding = "ISO-8859-1")


######################################## Paramètres
ceo_prefix <- "ceo-CollecteNationale_RoC_IGES_1990-2020"
ceo_type   <- "sample-data"

template   <- paste0(ceo_prefix,"_*-",ceo_type,"-",date,".csv")
template


######################################## Test rapide pour voir le nombre de colonnes et les entetes de fichiers
test <- function(date){
  list_ceo <- list.files(ceo_dir,pattern = glob2rx(paste0(ceo_prefix,"_*-",ceo_type,"-",date,".csv")))
  test <- read.csv(paste0(ceo_dir,list_ceo[1]),encoding = "UTF-8")
  print(names(test)[1:10])
  print(nrow(test))
}

#test("2021-03-12")
#test("2021-04-06")




######################################## Liste de fichier et initialisation de la table
list_ceo <- list.files(ceo_dir,pattern = glob2rx(template))
tmp      <- read.csv(paste0(ceo_dir,list_ceo[1]),encoding = "UTF-8")
d0       <- tmp[0]

list_ceo




######################################## Lire et Fusionner les fichiers dans DF
for(file in list_ceo){
  tmp <- read.csv(paste0(ceo_dir,file),encoding = "UTF-8")
  #print(ncol(tmp))
  d0 <- rbind(d0,tmp)
}



######################################## Renommer les colonnes de DF
names(d0) <- 
  c(
    "plot_id","sample_id","lon","lat","email","flagged",#"analyses", 
    "collection_time","analysis_duration",
    "imagery_title","imagery_attributions",
    "sample_geom","pl_plotid",
    "confidence",
    
    "os_1990_2000","n1_tf_1990_2000","n2_tfn_1990_2000","n3_tfnh_1990_2000",     
    "ac_1990_2000_tf","ac_1990_2000_tc","ac_1990_2000_pr","ac_1990_2000_th","ac_1990_2000_at",
    
    "os_2020","n1_tf_2020","n2_tfn_2020","n3_tfnh_2020", 
    
    "os_2000_2010","n1_tf_2000_2010","n2_tfn_2000_2010","ac_2000_2010_tf","n3_tfnh_2000_2010",     
    "ac_2000_2010_tc","ac_2000_2010_pr","ac_2000_2010_th","ac_2000_2010_at",
    
    "os_2010_2020","n1_tf_2010_2020","n2_tfn_2010_2020","n3_tfnh_2010_2020",     
    "ac_2010_2020_tf","ac_2010_2020_tc","ac_2010_2020_pr","ac_2010_2020_th","ac_2010_2020_at",
    
    "is_degradation","type_degradation","an_degradation",
    
    "n1_et_2020",
    
    "annee_image",
    "os_2020_ai","n1_tf_2020_ai","n2_tfn_2020_ai","n3_tfnh_2020_ai","n1_et_2020_ai",
    "n1_et_1990_2000","ac_1990_2000_et",
    "n1_et_2000_2010","ac_2000_2010_et",
    "n1_et_2010_2020","ac_2010_2020_et",
    
    "conversion","periode_conversion","conversion_fire")



######################################## Reorganiser les colonnes
reorganise <- c(
  "plot_id","sample_id","lon","lat","email","flagged",#"analyses", 
  "collection_time","analysis_duration",
  "imagery_title","imagery_attributions",
  "sample_geom","pl_plotid",
  
  "confidence",
  
  "os_2020","os_2010_2020","os_2000_2010","os_1990_2000",
  
  "n1_tf_2020","n2_tfn_2020","n3_tfnh_2020", 
  "n1_tf_2010_2020","n2_tfn_2010_2020","n3_tfnh_2010_2020",  
  "n1_tf_2000_2010","n2_tfn_2000_2010","n3_tfnh_2000_2010",
  "n1_tf_1990_2000","n2_tfn_1990_2000","n3_tfnh_1990_2000",  
  
  "n1_et_2020",
  "n1_et_2010_2020",
  "n1_et_2000_2010",
  "n1_et_1990_2000",
  
  "ac_2010_2020_tf","ac_2010_2020_tc","ac_2010_2020_th","ac_2010_2020_pr","ac_2010_2020_at","ac_2010_2020_et",
  "ac_2000_2010_tf","ac_2000_2010_tc","ac_2000_2010_th","ac_2000_2010_pr","ac_2000_2010_at","ac_2000_2010_et",
  "ac_1990_2000_tf","ac_1990_2000_tc","ac_1990_2000_th","ac_1990_2000_pr","ac_1990_2000_at","ac_1990_2000_et",
  
  "is_degradation","type_degradation","an_degradation",
  
  "annee_image",
  "os_2020_ai","n1_tf_2020_ai","n2_tfn_2020_ai","n3_tfnh_2020_ai","n1_et_2020_ai",
  
  "conversion","periode_conversion","conversion_fire")



######################################## Enleve les lignes vides, perimètre de la DB
df         <- d0[d0$email != ""   & d0$os_2020 != "",]
df <- df[,reorganise]
taille_db  <- nrow(df)
superficie <- superficie_pays*taille_db/nrow(d0)
head(df)



######################################## Cree une vraie colonne durée de collecte (en secondes)
df$duration <- as.numeric(gsub(" secs","",df$analysis_duration))



######################################## Convertir les NA en ""
df[is.na(df)] <- ""

print(paste0("Database of ",nrow(df)," points"))
print(table(df$email))

