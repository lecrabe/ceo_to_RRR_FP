######################################## Categories SANS CHANGEMENT
cat_no_chg <- c("pas de changements","pas de visibilité","no data")

######################################## Créer des colonnes osc_AAAA pour chaque année entre 1990 et 2020
df[,paste0("osc_",1990:2020)] <- NA


################################################################################ ANNEE 2020
######################################## Par défaut: remplir "osc_2020" par "os_2020"
df$osc_2020 <- df$os_2020


######################################## Si "no data" prendre "os_2020_ai"
df[df$os_2020 == "no data","osc_2020"] <- df[df$os_2020 == "no data","os_2020_ai"]


######################################## Si sous-niveau, le prendre
df[df$n1_tf_2020   == codes[ 7,2],"osc_2020"] <- codes[ 7,2]   # "plantations forestières"
df[df$n2_tfn_2020  == codes[ 8,2],"osc_2020"] <- codes[ 8,2]   # "forêt naturelle sur terre ferme"
df[df$n3_tfnh_2020 == codes[ 9,2],"osc_2020"] <- codes[ 9,2]   # "forêt inondable ou marécageuse"
df[df$n1_et_2020   == codes[10,2],"osc_2020"] <- codes[10,2]   # "bâtis"
df[df$n1_et_2020   == codes[11,2],"osc_2020"] <- codes[11,2]   # "routes"


######################################## Si sous-niveau + no data 2020, le prendre
df[df$n1_tf_2020_ai   == codes[ 7,2],"osc_2020"] <- codes[ 7,2]   # "plantations forestières"
df[df$n2_tfn_2020_ai  == codes[ 8,2],"osc_2020"] <- codes[ 8,2]   # "forêt naturelle sur terre ferme"
df[df$n3_tfnh_2020_ai == codes[ 9,2],"osc_2020"] <- codes[ 9,2]   # "forêt inondable ou marécageuse"
df[df$n1_et_2020_ai   == codes[10,2],"osc_2020"] <- codes[10,2]   # "bâtis"
df[df$n1_et_2020_ai   == codes[11,2],"osc_2020"] <- codes[11,2]   # "routes"



################################################################################ CONVERTIR LES LABELS EN CODE DANS OSC_2020
for(x in 1:nrow(codes)){df[df$osc_2020 == codes[x,2],"osc_2020"] <- codes[x,1]}


table(df$osc_2020,df$os_2020,useNA = "always")


################################################################################ FONCTION DE PROPAGATION DES CODES PAR ANNEE
propagate <- function(df,periode,annee){
  
  df[df[,paste0("os_",periode)]      == codes[ 1,2] & df[,paste0("ac_",periode,"_at")] == annee, paste0("osc_",annee)] <- codes[ 1,1] # "AT"  
  df[df[,paste0("os_",periode)]      == codes[ 2,2] & df[,paste0("ac_",periode,"_et")] == annee, paste0("osc_",annee)] <- codes[ 2,1] # "E"
  df[df[,paste0("os_",periode)]      == codes[ 3,2] & df[,paste0("ac_",periode,"_pr")] == annee, paste0("osc_",annee)] <- codes[ 3,1] # "P"
  df[df[,paste0("os_",periode)]      == codes[ 4,2] & df[,paste0("ac_",periode,"_tc")] == annee, paste0("osc_",annee)] <- codes[ 4,1] # "TC"
  df[df[,paste0("os_",periode)]      == codes[ 5,2] & df[,paste0("ac_",periode,"_tf")] == annee, paste0("osc_",annee)] <- codes[ 5,1] # "TF"
  df[df[,paste0("os_",periode)]      == codes[ 6,2] & df[,paste0("ac_",periode,"_th")] == annee, paste0("osc_",annee)] <- codes[ 6,1] # "TH"
  
  df[df[,paste0("n1_tf_",  periode)] == codes[ 7,2] & df[,paste0("ac_",periode,"_tf")] == annee, paste0("osc_",annee)] <- codes[ 7,1] #"FPLAN"
  df[df[,paste0("n2_tfn_", periode)] == codes[ 8,2] & df[,paste0("ac_",periode,"_tf")] == annee, paste0("osc_",annee)] <- codes[ 8,1] #"FNTF"
  df[df[,paste0("n3_tfnh_",periode)] == codes[ 9,2] & df[,paste0("ac_",periode,"_tf")] == annee, paste0("osc_",annee)] <- codes[ 9,1] #"FM" 
  
  df[df[,paste0("n1_et_",  periode)] == codes[10,2] & df[,paste0("ac_",periode,"_et")] == annee, paste0("osc_",annee)] <- codes[10,1] #"EBAT"
  df[df[,paste0("n1_et_",  periode)] == codes[11,2] & df[,paste0("ac_",periode,"_et")] == annee, paste0("osc_",annee)] <- codes[11,1] #"EROU"
  
  df[is.na(df[,paste0("osc_",annee)]),paste0("osc_",annee)]    <- df[is.na(df[,paste0("osc_",annee)]),paste0("osc_",annee+1)]
  
  return(df)
}

################################################################################ FONCTION DE PROCESS D'UNE PERIODE
process <- function(df,deb,fin){
  
  periode <- paste0(deb,"_",fin)
  
  ######################################## Par défaut: si pas de changement, pour chacune annee de la periode, prendre la derniere annee
  df[df[,paste0("os_",periode)] %in% cat_no_chg , paste0("osc_",deb:(fin-1))] <- df[df[,paste0("os_",periode)] %in% cat_no_chg , paste0("osc_",fin)]
  
  ######################################## Pour chaque annee, prendre changement OU copier l'annee suivante (on va en arriere)
  for(annee in (fin-1):deb){
    print(annee)
    df <- propagate(df,periode,annee)
  }
  
  print(table(df[,paste0("osc_",deb)],df[,paste0("osc_",fin)],useNA = "always"))
  
  return(df)
}

################################################################################ EXECUTER LES TROIS PERIODES
df <- process(df,2010,2020)
df <- process(df,2000,2010)
df <- process(df,1990,2000)

head(df[df$osc_1990 != df$osc_2020,])