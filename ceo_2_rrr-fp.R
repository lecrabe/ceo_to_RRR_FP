########################################################################################################################
######################################## Configuration - A priori ne rien changer
########################################################################################################################

######################################## Chemin du dossier: hors SEPAL, changer pour mettre le chemin jusqu'au dossier voulu
(ges_dir  <- paste0(normalizePath("~"),"/ceo_to_RRR_FP/") )


######################################## Packages et options
source(paste0(ges_dir,"parametres.R"))
source(paste0(ges_dir,"scripts/s0_configuration.R"))


########################################################################################################################
######################################## Faire tourner les etapes dans l'ordre
########################################################################################################################
source(paste0(scr_dir,"s1_merge_rename.R"),echo = F)
source(paste0(scr_dir,"s2_generate_osc.R"),echo = F)
source(paste0(scr_dir,"s3_trajectoires.R"),echo = F)
source(paste0(scr_dir,"s4_extraire_TMF.R"),echo = F) ## Utiliser seulement si le produit TMF est généré


########################################################################################################################
######################################## Exporter la BDD et les trajectoires pour RRR+ FP
########################################################################################################################
write.csv(df,paste0(out_dir,"bdd_ceo_",date,".csv"),row.names = F,fileEncoding = "ISO-8859-1")
write.csv(dd,paste0(out_dir,"traj_",date,".csv"),row.names = F)

View(dd)










