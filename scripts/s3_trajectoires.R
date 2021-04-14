################################################################################ CALCULER LES SUPERFICIES DE CHAQUE TRAJECTOIRE
dd[,paste0("a_",1990:2019)] <- NA

for(ligne in 1:nrow(dd)){
  for(annee in 1990:2019){
    
    dd[ligne,paste0("a_",annee)] <- area(df[df[,paste0("osc_",annee)] == dd[ligne,1] & df[,paste0("osc_",annee+1)] == dd[ligne,2],])
  }
}

print(head(dd))

