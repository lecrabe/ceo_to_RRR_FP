######################################## Vue generale
### Qui a fait quoi
table(d0$email)
hist(log10(df$duration))
summary(df$duration)



######################################## Superficies "STABLE"
(superficies_stables <- sapply(unique(codes$code),function(x){area(stable(x))}))


######################################## Categories Occupation des sols
cat_ipcc <- c("terres forestières",
              "terres cultivées",
              "prairies / savanes",
              "terres humides",
              "autres terres",
              "établissements",
              "pas de changements",
              "pas de visibilité",
              "no data",
              ""
)

######################################## Superficies de forêt "STABLE"
tfnsh <- df[
  df$n2_tfn_2020 =="forêt naturelle sur sol hydromorphe" &
    df$os_2010_2020 == cat_ipcc[7]
  ,]

tfntf <- df[
  df$n2_tfn_2020 =="forêt naturelle sur terre ferme" &
    df$os_2010_2020 == cat_ipcc[7]
  ,]

tfp   <- df[
  df$n1_tf_2020 =="plantations forestières" &
    df$os_2010_2020 == cat_ipcc[7]
  ,]


######################################## Superficies de deforestation
def_all <- df[df$os_1990_2000 == cat_ipcc[1] & df$os_2020 != cat_ipcc[1]
              ,]

######################################## Transitions
area(df[df$osc_1990 != df$osc_2020,]) + area(df[df$osc_1990 == df$osc_2020,])

### Verifie les transitions
table(df$osc_1990,df$osc_2020)

######################################## Quelques résultats
round(superficie)
sum(superficies_stables)

superficies_stables 

(changements <- round(superficie) - sum(superficies_stables))

round(sum(superficies_stables)/round(superficie)*100,1)

area(tfnsh)
area(tfntf)
area(tfp)

area(tfnsh) + area(tfntf) + area(tfp) == area(stable(cat_ipcc[1]))

area(def_all)

table(tr$os_1990_2000,tr$os_2020)
table(df$ac_2000_2010_tf)

#df[!is.na(df$annee_image),]
nrow(df[df$os_1990_2000 == cat_ipcc[8],])

table(df$n3_tfnh_2020,df$n2_tfn_2020)


round(sum(dd$a_1990)) - sum(dd$a_1990)
round(sum(dd$a_2000)) - sum(dd$a_2000)
area(df)
