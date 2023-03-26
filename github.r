###for reviewer and editor only
##for replicated https://npc2ite.shinyapps.io/shinyite/
# "Predicted Individual Treatment Effect of Induction Chemotherapy for Nasopharyngeal Carcinoma"
#by lihaoj@sysucc.org.cn
##################include library and model##################
if(TRUE){
 
    
    library(rms)
    library(Hmisc)
    
    library(survival)
    
    library(ggplot2)
    
    library(glmnet)

 
    load("P://npc3/shinyite/nmodel2.RData")
    load("P://npc3/shinyite/ymodel2.RData")
    load("P://npc3/shinyite/nlasso2.RData")
    load("P://npc3/shinyite/ylasso2.RData")
  
  
}

##################how to used model##################

# one patient for example
if(TRUE){
 
  #prepare variables value for models
  newdb <- data.frame(
    x62x = 0,       #'Musculus Longus Capitis'
    x111x = 0,      #'Infratemporal Fossa'
    x178x = 0,      # 'Jugular Foramen'
    x274x = 0,      #'Bilateral Retropharyngeal Lymph Node'
    x497x = 1,      # 'Central Necrosis of Lymph Nodes',
    x500x = 7,      #'Number of All Lymph Nodes'
    monocyte3f = 2, #'Absolute Monocyte',low  normal high
    Albumin = 49.9, #Albumin
    test2ebv = 1,   #ebv
    x10x = 40,      #age 
    x160x = 0,      #'Mild Skull Base Destruction'
    x169x = 0,      #'Carotid Sheath Invasion'
    x198x = 1,      #'Orbital Invasion'
    x201x = 1,      #'other Intracranial Invasion'
    Albumin_cut = 1 #'High Albumin'
  )
  
  
  #prepare variables(factor or continues) for models
  newdb[,"x62x"]=factor(newdb[,"x62x"],levels=c(0,1) )
  newdb[,"x111x"]=factor(newdb[,"x111x"],levels=c(0,1) )
  newdb[,"x178x"]=factor(newdb[,"x178x"],levels=c(0,1) )
  newdb[,"x274x"]=factor(newdb[,"x274x"],levels=c(0,1) )
  newdb[,"x497x"]=factor(newdb[,"x497x"],levels=c(0,1) )
  newdb[,"x500x"]=as.integer(newdb[,"x500x"])
  newdb[,"monocyte3f"]=factor(newdb[,"monocyte3f"],levels=c(1,2,3) )
  newdb[,"Albumin"]=as.numeric(newdb[,"Albumin"])
  newdb[,"test2ebv"]=factor(newdb[,"test2ebv"],levels=c(0,1,2) )
  newdb[,"x10x"]=as.numeric(newdb[,"x10x"])
  newdb[,"x160x"]=factor(newdb[,"x160x"],levels=c(0,1) )
  newdb[,"x169x"]=factor(newdb[,"x169x"],levels=c(0,1) )
  newdb[,"x198x"]=factor(newdb[,"x198x"],levels=c(0,1) )
  newdb[,"x201x"]=factor(newdb[,"x201x"],levels=c(0,1) )
  newdb[,"Albumin_cut"]=factor(newdb[,"Albumin_cut"],levels=c(0,1) )
  
}

#to calculated the Individual Treatment Effect of this patient
if(TRUE){
  newdb$y_lasso=predict(ylasso2,newdata = newdb)
  newdb$n_lasso=predict(nlasso2,newdata = newdb)
  alldb=newdb 
  
  #if ic
  fit =survfit(ymodel2, newdata =alldb)
  y_fit=fit
  alldb$y5year=as.numeric(mysurvival(fit,60,ireturn="value"))
  alldb$y5year_lower=as.numeric(mysurvival(fit,60,ireturn="lower"))
  alldb$y5year_upper=as.numeric(mysurvival(fit,60,ireturn="upper"))
  
  #if non_ic
  fit = survfit(nmodel2, newdata =alldb)
  n_fit=fit
  alldb$n5year=as.numeric(mysurvival(fit,60,ireturn="value"))
  alldb$n5year_lower=as.numeric(mysurvival(fit,60,ireturn="lower"))
  alldb$n5year_upper=as.numeric(mysurvival(fit,60,ireturn="upper"))
  
  #ite here (Individual Treatment Effect of this patient)
  alldb$minus5year=alldb$y5year-alldb$n5year 
 
}
