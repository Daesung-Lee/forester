library(tidyverse)
library(readxl)

setwd("C:/Daesung_R/taper_volume_functions")
getwd()

# DLee.taper parameter ---------------------------------------------

# Read the article for more information
# Lee, D., Seo, Y., Lee, J. and Choi, J., 2017. 
# Estimation and validation of taper equations for three major coniferous species 
# in Gangwon and north Gyeongsang provinces of South Korea. 
# Journal of Forest and Environmental Science, 33(4), pp.315-321.

daesung_taper_parameters <- read_excel("C:/Daesung_R/taper_volume_functions/daesung taper parameter.xlsx") %>% 
  select(species,mod_no,a1:b6)

daesung_taper_parameters2 <- daesung_taper_parameters %>% 
  select(species,mod_no,a1:b6) %>% 
  pivot_longer(cols = -c(species,mod_no)) %>% 
  mutate(parm = paste0(species,".",mod_no,".",name),
         wider = 1) %>% 
  select(parm, value) %>%
  pivot_wider(names_from=parm, values_from=value)

# DLee.taper.diameter function --------------------------------------------------------------
DLee.taper.diameter <- function(dbh,height,upper_height,model,species, print_info = TRUE) {
  
  if (species=="Pd") {
    
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(daesung_taper_parameters2) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             inflection_point = 0.2,
             Hrt = upper_height/height,
             i1 = case_when(Hrt<=Pd.2.a1 ~ 1,
                            Hrt>Pd.2.a1 ~ 0),
             i2 = case_when(Hrt<=Pd.2.a2 ~ 1,
                            Hrt>Pd.2.a2 ~ 0),
             Xi = (1-Hrt^(1/2))/(1-p^(1/2)),
             Xj = (1-Hrt^(1/4))/(1-0.01^(1/4)),
             Q = 1-Hrt^(1/3),
             Xk = Q/(1-(1.3/H)^(1/3)),
             X = (1-sqrt(upper_height/height))/(1-sqrt(p))
      )
    
    if(model == 1) {
      tempdat2 <- tempdata %>% 
        mutate(mod1_d = Pd.1.a1*(D^Pd.1.a2)*((H-h)^Pd.1.a3)*(H^Pd.1.a4)) %>% 
        select(mod1_d) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdat2 <- tempdata %>%
        mutate(mod2_d = D*sqrt(Pd.2.b1*(Hrt-1)+Pd.2.b2*(Hrt^2-1)+Pd.2.b3*((Pd.2.a1-Hrt)^2)*i1 + Pd.2.b4*((Pd.2.a2-Hrt)^2)*i2)) %>%
        select(mod2_d) %>%
        as.numeric()
    }
    
    else if(model == 3) {
      tempdat2 <- tempdata %>%
        mutate(mod3_d = Pd.3.a1*(D^Pd.3.a2)*(Pd.3.a3^D)*Xi^(Pd.3.b1*(Hrt^2)+Pd.3.b2*log(Hrt+0.001)+Pd.3.b3*(Hrt^(1/2))+Pd.3.b4*exp(Hrt)+Pd.3.b5*(D/H))) %>% 
        select(mod3_d) %>%
        as.numeric()
    }
    
    else if(model == 4) {
      tempdat2 <- tempdata %>%
        mutate(mod4_d = Pd.4.a1*(D^Pd.4.a2)*(1-sqrt(Hrt))^((Pd.4.b1*Hrt)+(Pd.4.b2*(Hrt^2))+(Pd.4.b3/Hrt)+(Pd.4.b4*(Hrt^3))+(Pd.4.b5*D)+(Pd.4.b6*(D/Hrt)))) %>% 
        select(mod4_d) %>%
        as.numeric()
    }
    
    else if(model == 5) {
      tempdat2 <- tempdata %>%
        mutate(mod5_d = Pd.5.a1*(D^Pd.5.a2)*Xj^(Pd.5.b1+Pd.5.b2*(1/exp(D/H))+Pd.5.b3*(D^Xj)+Pd.5.b4*(Xj^(D/H)))) %>% 
        select(mod5_d) %>%
        as.numeric()
    }
    
    else if(model == 6) {
      tempdat2 <- tempdata %>%
        mutate(mod6_d = Pd.6.a1*(D^Pd.6.a2)*(H^Pd.6.a3)*Xk^(Pd.6.b1*(Hrt^4)+Pd.6.b2*(1/exp(D/H))+Pd.6.b3*X^0.1+Pd.6.b4*(1/D)+Pd.6.b5*H^Q+Pd.6.b6*Xk)) %>% 
        select(mod6_d) %>%
        as.numeric()
    }
    
    else if(model == 7) {
      tempdat2 <- tempdata %>%
        mutate(mod7_d = Pd.7.a1*D^Pd.7.a2*(1-Hrt)^(Pd.7.b1*(Hrt^2)+Pd.7.b2*Hrt+Pd.7.b3)) %>% 
        select(mod7_d) %>%
        as.numeric()
      
    }
  }
  
  
  if (species=="Pk") {
    
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(daesung_taper_parameters2) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             inflection_point = 0.2,
             Hrt = upper_height/height,
             i1 = case_when(Hrt<=Pk.2.a1 ~ 1,
                            Hrt>Pk.2.a1 ~ 0),
             i2 = case_when(Hrt<=Pk.2.a2 ~ 1,
                            Hrt>Pk.2.a2 ~ 0),
             Xi = (1-Hrt^(1/2))/(1-p^(1/2)),
             Xj = (1-Hrt^(1/4))/(1-0.01^(1/4)),
             Q = 1-Hrt^(1/3),
             Xk = Q/(1-(1.3/H)^(1/3)),
             X = (1-sqrt(upper_height/height))/(1-sqrt(p))
      )
    
    if(model == 1) {
      tempdat2 <- tempdata %>% 
        mutate(mod1_d = Pk.1.a1*(D^Pk.1.a2)*((H-h)^Pk.1.a3)*(H^Pk.1.a4)) %>% 
        select(mod1_d) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdat2 <- tempdata %>%
        mutate(mod2_d = D*sqrt(Pk.2.b1*(Hrt-1)+Pk.2.b2*(Hrt^2-1)+Pk.2.b3*((Pk.2.a1-Hrt)^2)*i1 + Pk.2.b4*((Pk.2.a2-Hrt)^2)*i2)) %>%
        select(mod2_d) %>%
        as.numeric()
    }
    
    else if(model == 3) {
      tempdat2 <- tempdata %>%
        mutate(mod3_d = Pk.3.a1*(D^Pk.3.a2)*(Pk.3.a3^D)*Xi^(Pk.3.b1*(Hrt^2)+Pk.3.b2*log(Hrt+0.001)+Pk.3.b3*(Hrt^(1/2))+Pk.3.b4*exp(Hrt)+Pk.3.b5*(D/H))) %>% 
        select(mod3_d) %>%
        as.numeric()
    }
    
    else if(model == 4) {
      tempdat2 <- tempdata %>%
        mutate(mod4_d = Pk.4.a1*(D^Pk.4.a2)*(1-sqrt(Hrt))^((Pk.4.b1*Hrt)+(Pk.4.b2*(Hrt^2))+(Pk.4.b3/Hrt)+(Pk.4.b4*(Hrt^3))+(Pk.4.b5*D)+(Pk.4.b6*(D/Hrt)))) %>% 
        select(mod4_d) %>%
        as.numeric()
    }
    
    else if(model == 5) {
      tempdat2 <- tempdata %>%
        mutate(mod5_d = Pk.5.a1*(D^Pk.5.a2)*Xj^(Pk.5.b1+Pk.5.b2*(1/exp(D/H))+Pk.5.b3*(D^Xj)+Pk.5.b4*(Xj^(D/H)))) %>% 
        select(mod5_d) %>%
        as.numeric()
    }
    
    else if(model == 6) {
      tempdat2 <- tempdata %>%
        mutate(mod6_d = Pk.6.a1*(D^Pk.6.a2)*(H^Pk.6.a3)*Xk^(Pk.6.b1*(Hrt^4)+Pk.6.b2*(1/exp(D/H))+Pk.6.b3*X^0.1+Pk.6.b4*(1/D)+Pk.6.b5*H^Q+Pk.6.b6*Xk)) %>% 
        select(mod6_d) %>%
        as.numeric()
    }
    
    else if(model == 7) {
      tempdat2 <- tempdata %>%
        mutate(mod7_d = Pk.7.a1*D^Pk.7.a2*(1-Hrt)^(Pk.7.b1*(Hrt^2)+Pk.7.b2*Hrt+Pk.7.b3)) %>% 
        select(mod7_d) %>%
        as.numeric()
      
    }
    
  }
  
  
  if (species=="Lk") {
    
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(daesung_taper_parameters2) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             inflection_point = 0.2,
             Hrt = upper_height/height,
             i1 = case_when(Hrt<=Lk.2.a1 ~ 1,
                            Hrt>Lk.2.a1 ~ 0),
             i2 = case_when(Hrt<=Lk.2.a2 ~ 1,
                            Hrt>Lk.2.a2 ~ 0),
             Xi = (1-Hrt^(1/2))/(1-p^(1/2)),
             Xj = (1-Hrt^(1/4))/(1-0.01^(1/4)),
             Q = 1-Hrt^(1/3),
             Xk = Q/(1-(1.3/H)^(1/3)),
             X = (1-sqrt(upper_height/height))/(1-sqrt(p))
      )
    
    if(model == 1) {
      tempdat2 <- tempdata %>% 
        mutate(mod1_d = Lk.1.a1*(D^Lk.1.a2)*((H-h)^Lk.1.a3)*(H^Lk.1.a4)) %>% 
        select(mod1_d) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdat2 <- tempdata %>%
        mutate(mod2_d = D*sqrt(Lk.2.b1*(Hrt-1)+Lk.2.b2*(Hrt^2-1)+Lk.2.b3*((Lk.2.a1-Hrt)^2)*i1 + Lk.2.b4*((Lk.2.a2-Hrt)^2)*i2)) %>%
        select(mod2_d) %>%
        as.numeric()
    }
    
    else if(model == 3) {
      tempdat2 <- tempdata %>%
        mutate(mod3_d = Lk.3.a1*(D^Lk.3.a2)*(Lk.3.a3^D)*Xi^(Lk.3.b1*(Hrt^2)+Lk.3.b2*log(Hrt+0.001)+Lk.3.b3*(Hrt^(1/2))+Lk.3.b4*exp(Hrt)+Lk.3.b5*(D/H))) %>% 
        select(mod3_d) %>%
        as.numeric()
    }
    
    else if(model == 4) {
      tempdat2 <- tempdata %>%
        mutate(mod4_d = Lk.4.a1*(D^Lk.4.a2)*(1-sqrt(Hrt))^((Lk.4.b1*Hrt)+(Lk.4.b2*(Hrt^2))+(Lk.4.b3/Hrt)+(Lk.4.b4*(Hrt^3))+(Lk.4.b5*D)+(Lk.4.b6*(D/Hrt)))) %>% 
        select(mod4_d) %>%
        as.numeric()
    }
    
    else if(model == 5) {
      tempdat2 <- tempdata %>%
        mutate(mod5_d = Lk.5.a1*(D^Lk.5.a2)*Xj^(Lk.5.b1+Lk.5.b2*(1/exp(D/H))+Lk.5.b3*(D^Xj)+Lk.5.b4*(Xj^(D/H)))) %>% 
        select(mod5_d) %>%
        as.numeric()
    }
    
    else if(model == 6) {
      tempdat2 <- tempdata %>%
        mutate(mod6_d = Lk.6.a1*(D^Lk.6.a2)*(H^Lk.6.a3)*Xk^(Lk.6.b1*(Hrt^4)+Lk.6.b2*(1/exp(D/H))+Lk.6.b3*X^0.1+Lk.6.b4*(1/D)+Lk.6.b5*H^Q+Lk.6.b6*Xk)) %>% 
        select(mod6_d) %>%
        as.numeric()
    }
    
    else if(model == 7) {
      tempdat2 <- tempdata %>%
        mutate(mod7_d = Lk.7.a1*D^Lk.7.a2*(1-Hrt)^(Lk.7.b1*(Hrt^2)+Lk.7.b2*Hrt+Lk.7.b3)) %>% 
        select(mod7_d) %>%
        as.numeric()
      
    }
    
  }
  
 return(tempdat2) 
  
}

# DLee.taper.volume function ----------------------------------------------

# The recommended models are mod_no 3 (Kozak 1988) or mod_no 5 (Kozak 2001) for all species of Pd, Pk, and Lk.
# The best recommended model is mod_no 3 (Kozak 1988) as the same as NIFoS recommendation.

DLee.taper.volume <- function(dbh,height,model,species, print_info = TRUE) {
  
  if (species=="Pd") {
    
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(daesung_taper_parameters2) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             inflection_point = 0.2,
             Hrt = upper_height/height,
             i1 = case_when(Hrt<=Pd.2.a1 ~ 1,
                            Hrt>Pd.2.a1 ~ 0),
             i2 = case_when(Hrt<=Pd.2.a2 ~ 1,
                            Hrt>Pd.2.a2 ~ 0),
             Xi = (1-Hrt^(1/2))/(1-p^(1/2)),
             Xj = (1-Hrt^(1/4))/(1-0.01^(1/4)),
             Q = 1-Hrt^(1/3),
             Xk = Q/(1-(1.3/H)^(1/3))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             mod1_d = Pd.1.a1*(D^Pd.1.a2)*((H-h)^Pd.1.a3)*(H^Pd.1.a4),
             mod2_d = D*sqrt(Pd.2.b1*(Hrt-1)+Pd.2.b2*(Hrt^2-1)+Pd.2.b3*((Pd.2.a1-Hrt)^2)*i1 + Pd.2.b4*((Pd.2.a2-Hrt)^2)*i2),
             mod3_d = Pd.3.a1*(D^Pd.3.a2)*(Pd.3.a3^D)*Xi^(Pd.3.b1*(Hrt^2)+Pd.3.b2*log(Hrt+0.001)+Pd.3.b3*(Hrt^(1/2))+Pd.3.b4*exp(Hrt)+Pd.3.b5*(D/H)),
             mod4_d = Pd.4.a1*(D^Pd.4.a2)*(1-sqrt(Hrt))^((Pd.4.b1*Hrt)+(Pd.4.b2*(Hrt^2))+(Pd.4.b3/Hrt)+(Pd.4.b4*(Hrt^3))+(Pd.4.b5*D)+(Pd.4.b6*(D/Hrt))),
             mod5_d = Pd.5.a1*(D^Pd.5.a2)*Xj^(Pd.5.b1+Pd.5.b2*(1/exp(D/H))+Pd.5.b3*(D^Xj)+Pd.5.b4*(Xj^(D/H))),
             mod6_d = Pd.6.a1*(D^Pd.6.a2)*(H^Pd.6.a3)*Xk^(Pd.6.b1*(Hrt^4)+Pd.6.b2*(1/exp(D/H))+Pd.6.b3*X^0.1+Pd.6.b4*(1/D)+Pd.6.b5*H^Q+Pd.6.b6*Xk),
             mod7_d = Pd.7.a1*D^Pd.7.a2*(1-Hrt)^(Pd.7.b1*(Hrt^2)+Pd.7.b2*Hrt+Pd.7.b3)) %>% 
      group_by(dbh,height) %>% 
      mutate(mod1_ba = (pi/40000)*(mod1_d**2),
             mod1_ba2= lag(mod1_ba),
             mod1_volume= case_when(height > upper_height ~ ((mod1_ba+mod1_ba2)/2)*0.1,
                                    height == upper_height ~ (mod1_ba2/3)*0.1),
             mod2_ba = (pi/40000)*(mod2_d**2),
             mod2_ba2= lag(mod2_ba),
             mod2_volume= case_when(height > upper_height ~ ((mod2_ba+mod2_ba2)/2)*0.1,
                                    height == upper_height ~ (mod2_ba2/3)*0.1),
             mod3_ba = (pi/40000)*(mod3_d**2),
             mod3_ba2= lag(mod3_ba),
             mod3_volume= case_when(height > upper_height ~ ((mod3_ba+mod3_ba2)/2)*0.1,
                                    height == upper_height ~ (mod3_ba2/3)*0.1),
             mod4_ba = (pi/40000)*(mod4_d**2),
             mod4_ba2= lag(mod4_ba),
             mod4_volume= case_when(height > upper_height ~ ((mod4_ba+mod4_ba2)/2)*0.1,
                                    height == upper_height ~ (mod4_ba2/3)*0.1),
             mod5_ba = (pi/40000)*(mod5_d**2),
             mod5_ba2= lag(mod5_ba),
             mod5_volume= case_when(height > upper_height ~ ((mod5_ba+mod5_ba2)/2)*0.1,
                                    height == upper_height ~ (mod5_ba2/3)*0.1),
             mod6_ba = (pi/40000)*(mod6_d**2),
             mod6_ba2= lag(mod6_ba),
             mod6_volume= case_when(height > upper_height ~ ((mod6_ba+mod6_ba2)/2)*0.1,
                                    height == upper_height ~ (mod6_ba2/3)*0.1),
             mod7_ba = (pi/40000)*(mod7_d**2),
             mod7_ba2= lag(mod7_ba),
             mod7_volume= case_when(height > upper_height ~ ((mod7_ba+mod7_ba2)/2)*0.1,
                                    height == upper_height ~ (mod7_ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      pivot_longer(cols = -c(dbh,height,upper_height)) %>% 
      mutate(model = as.numeric(str_sub(name,start=4,end=4))) %>% 
      select(-name) %>% 
      group_by(model) %>%
      rename(volume=value) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    if(model == 1) {
      tempdata3 <- tempdata2 %>% 
        filter(model==1) %>% 
        select(volume) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdata3 <- tempdata2 %>%
        filter(model==2) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 3) {
      tempdata3 <- tempdata2 %>%
        filter(model==3) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 4) {
      tempdata3 <- tempdata2 %>%
        filter(model==4) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 5) {
      tempdata3 <- tempdata2 %>%
        filter(model==5) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 6) {
      tempdata3 <- tempdata2 %>%
        filter(model==6) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 7) {
      tempdata3 <- tempdata2 %>%
        filter(model==7) %>%
        select(volume) %>%
        as.numeric()
    }
    
    tempdata3
    
  }
  
  
  
  if (species=="Pk") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(daesung_taper_parameters2) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             inflection_point = 0.2,
             Hrt = upper_height/height,
             i1 = case_when(Hrt<=Pk.2.a1 ~ 1,
                            Hrt>Pk.2.a1 ~ 0),
             i2 = case_when(Hrt<=Pk.2.a2 ~ 1,
                            Hrt>Pk.2.a2 ~ 0),
             Xi = (1-Hrt^(1/2))/(1-p^(1/2)),
             Xj = (1-Hrt^(1/4))/(1-0.01^(1/4)),
             Q = 1-Hrt^(1/3),
             Xk = Q/(1-(1.3/H)^(1/3))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             mod1_d = Pk.1.a1*(D^Pk.1.a2)*((H-h)^Pk.1.a3)*(H^Pk.1.a4),
             mod2_d = D*sqrt(Pk.2.b1*(Hrt-1)+Pk.2.b2*(Hrt^2-1)+Pk.2.b3*((Pk.2.a1-Hrt)^2)*i1 + Pk.2.b4*((Pk.2.a2-Hrt)^2)*i2),
             mod3_d = Pk.3.a1*(D^Pk.3.a2)*(Pk.3.a3^D)*Xi^(Pk.3.b1*(Hrt^2)+Pk.3.b2*log(Hrt+0.001)+Pk.3.b3*(Hrt^(1/2))+Pk.3.b4*exp(Hrt)+Pk.3.b5*(D/H)),
             mod4_d = Pk.4.a1*(D^Pk.4.a2)*(1-sqrt(Hrt))^((Pk.4.b1*Hrt)+(Pk.4.b2*(Hrt^2))+(Pk.4.b3/Hrt)+(Pk.4.b4*(Hrt^3))+(Pk.4.b5*D)+(Pk.4.b6*(D/Hrt))),
             mod5_d = Pk.5.a1*(D^Pk.5.a2)*Xj^(Pk.5.b1+Pk.5.b2*(1/exp(D/H))+Pk.5.b3*(D^Xj)+Pk.5.b4*(Xj^(D/H))),
             mod6_d = Pk.6.a1*(D^Pk.6.a2)*(H^Pk.6.a3)*Xk^(Pk.6.b1*(Hrt^4)+Pk.6.b2*(1/exp(D/H))+Pk.6.b3*X^0.1+Pk.6.b4*(1/D)+Pk.6.b5*H^Q+Pk.6.b6*Xk),
             mod7_d = Pk.7.a1*D^Pk.7.a2*(1-Hrt)^(Pk.7.b1*(Hrt^2)+Pk.7.b2*Hrt+Pk.7.b3)) %>% 
      group_by(dbh,height) %>% 
      mutate(mod1_ba = (pi/40000)*(mod1_d**2),
             mod1_ba2= lag(mod1_ba),
             mod1_volume= case_when(height > upper_height ~ ((mod1_ba+mod1_ba2)/2)*0.1,
                                    height == upper_height ~ (mod1_ba2/3)*0.1),
             mod2_ba = (pi/40000)*(mod2_d**2),
             mod2_ba2= lag(mod2_ba),
             mod2_volume= case_when(height > upper_height ~ ((mod2_ba+mod2_ba2)/2)*0.1,
                                    height == upper_height ~ (mod2_ba2/3)*0.1),
             mod3_ba = (pi/40000)*(mod3_d**2),
             mod3_ba2= lag(mod3_ba),
             mod3_volume= case_when(height > upper_height ~ ((mod3_ba+mod3_ba2)/2)*0.1,
                                    height == upper_height ~ (mod3_ba2/3)*0.1),
             mod4_ba = (pi/40000)*(mod4_d**2),
             mod4_ba2= lag(mod4_ba),
             mod4_volume= case_when(height > upper_height ~ ((mod4_ba+mod4_ba2)/2)*0.1,
                                    height == upper_height ~ (mod4_ba2/3)*0.1),
             mod5_ba = (pi/40000)*(mod5_d**2),
             mod5_ba2= lag(mod5_ba),
             mod5_volume= case_when(height > upper_height ~ ((mod5_ba+mod5_ba2)/2)*0.1,
                                    height == upper_height ~ (mod5_ba2/3)*0.1),
             mod6_ba = (pi/40000)*(mod6_d**2),
             mod6_ba2= lag(mod6_ba),
             mod6_volume= case_when(height > upper_height ~ ((mod6_ba+mod6_ba2)/2)*0.1,
                                    height == upper_height ~ (mod6_ba2/3)*0.1),
             mod7_ba = (pi/40000)*(mod7_d**2),
             mod7_ba2= lag(mod7_ba),
             mod7_volume= case_when(height > upper_height ~ ((mod7_ba+mod7_ba2)/2)*0.1,
                                    height == upper_height ~ (mod7_ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      pivot_longer(cols = -c(dbh,height,upper_height)) %>% 
      mutate(model = as.numeric(str_sub(name,start=4,end=4))) %>% 
      select(-name) %>% 
      group_by(model) %>%
      rename(volume=value) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    if(model == 1) {
      tempdata3 <- tempdata2 %>% 
        filter(model==1) %>% 
        select(volume) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdata3 <- tempdata2 %>%
        filter(model==2) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 3) {
      tempdata3 <- tempdata2 %>%
        filter(model==3) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 4) {
      tempdata3 <- tempdata2 %>%
        filter(model==4) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 5) {
      tempdata3 <- tempdata2 %>%
        filter(model==5) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 6) {
      tempdata3 <- tempdata2 %>%
        filter(model==6) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 7) {
      tempdata3 <- tempdata2 %>%
        filter(model==7) %>%
        select(volume) %>%
        as.numeric()
    }
    
    tempdata3
    
  }
  
  
  
  if (species=="Lk") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(daesung_taper_parameters2) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             inflection_point = 0.2,
             Hrt = upper_height/height,
             i1 = case_when(Hrt<=Lk.2.a1 ~ 1,
                            Hrt>Lk.2.a1 ~ 0),
             i2 = case_when(Hrt<=Lk.2.a2 ~ 1,
                            Hrt>Lk.2.a2 ~ 0),
             Xi = (1-Hrt^(1/2))/(1-p^(1/2)),
             Xj = (1-Hrt^(1/4))/(1-0.01^(1/4)),
             Q = 1-Hrt^(1/3),
             Xk = Q/(1-(1.3/H)^(1/3))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             mod1_d = Lk.1.a1*(D^Lk.1.a2)*((H-h)^Lk.1.a3)*(H^Lk.1.a4),
             mod2_d = D*sqrt(Lk.2.b1*(Hrt-1)+Lk.2.b2*(Hrt^2-1)+Lk.2.b3*((Lk.2.a1-Hrt)^2)*i1 + Lk.2.b4*((Lk.2.a2-Hrt)^2)*i2),
             mod3_d = Lk.3.a1*(D^Lk.3.a2)*(Lk.3.a3^D)*Xi^(Lk.3.b1*(Hrt^2)+Lk.3.b2*log(Hrt+0.001)+Lk.3.b3*(Hrt^(1/2))+Lk.3.b4*exp(Hrt)+Lk.3.b5*(D/H)),
             mod4_d = Lk.4.a1*(D^Lk.4.a2)*(1-sqrt(Hrt))^((Lk.4.b1*Hrt)+(Lk.4.b2*(Hrt^2))+(Lk.4.b3/Hrt)+(Lk.4.b4*(Hrt^3))+(Lk.4.b5*D)+(Lk.4.b6*(D/Hrt))),
             mod5_d = Lk.5.a1*(D^Lk.5.a2)*Xj^(Lk.5.b1+Lk.5.b2*(1/exp(D/H))+Lk.5.b3*(D^Xj)+Lk.5.b4*(Xj^(D/H))),
             mod6_d = Lk.6.a1*(D^Lk.6.a2)*(H^Lk.6.a3)*Xk^(Lk.6.b1*(Hrt^4)+Lk.6.b2*(1/exp(D/H))+Lk.6.b3*X^0.1+Lk.6.b4*(1/D)+Lk.6.b5*H^Q+Lk.6.b6*Xk),
             mod7_d = Lk.7.a1*D^Lk.7.a2*(1-Hrt)^(Lk.7.b1*(Hrt^2)+Lk.7.b2*Hrt+Lk.7.b3)) %>% 
      group_by(dbh,height) %>% 
      mutate(mod1_ba = (pi/40000)*(mod1_d**2),
             mod1_ba2= lag(mod1_ba),
             mod1_volume= case_when(height > upper_height ~ ((mod1_ba+mod1_ba2)/2)*0.1,
                                    height == upper_height ~ (mod1_ba2/3)*0.1),
             mod2_ba = (pi/40000)*(mod2_d**2),
             mod2_ba2= lag(mod2_ba),
             mod2_volume= case_when(height > upper_height ~ ((mod2_ba+mod2_ba2)/2)*0.1,
                                    height == upper_height ~ (mod2_ba2/3)*0.1),
             mod3_ba = (pi/40000)*(mod3_d**2),
             mod3_ba2= lag(mod3_ba),
             mod3_volume= case_when(height > upper_height ~ ((mod3_ba+mod3_ba2)/2)*0.1,
                                    height == upper_height ~ (mod3_ba2/3)*0.1),
             mod4_ba = (pi/40000)*(mod4_d**2),
             mod4_ba2= lag(mod4_ba),
             mod4_volume= case_when(height > upper_height ~ ((mod4_ba+mod4_ba2)/2)*0.1,
                                    height == upper_height ~ (mod4_ba2/3)*0.1),
             mod5_ba = (pi/40000)*(mod5_d**2),
             mod5_ba2= lag(mod5_ba),
             mod5_volume= case_when(height > upper_height ~ ((mod5_ba+mod5_ba2)/2)*0.1,
                                    height == upper_height ~ (mod5_ba2/3)*0.1),
             mod6_ba = (pi/40000)*(mod6_d**2),
             mod6_ba2= lag(mod6_ba),
             mod6_volume= case_when(height > upper_height ~ ((mod6_ba+mod6_ba2)/2)*0.1,
                                    height == upper_height ~ (mod6_ba2/3)*0.1),
             mod7_ba = (pi/40000)*(mod7_d**2),
             mod7_ba2= lag(mod7_ba),
             mod7_volume= case_when(height > upper_height ~ ((mod7_ba+mod7_ba2)/2)*0.1,
                                    height == upper_height ~ (mod7_ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      pivot_longer(cols = -c(dbh,height,upper_height)) %>% 
      mutate(model = as.numeric(str_sub(name,start=4,end=4))) %>% 
      select(-name) %>% 
      group_by(model) %>%
      rename(volume=value) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    if(model == 1) {
      tempdata3 <- tempdata2 %>% 
        filter(model==1) %>% 
        select(volume) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdata3 <- tempdata2 %>%
        filter(model==2) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 3) {
      tempdata3 <- tempdata2 %>%
        filter(model==3) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 4) {
      tempdata3 <- tempdata2 %>%
        filter(model==4) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 5) {
      tempdata3 <- tempdata2 %>%
        filter(model==5) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 6) {
      tempdata3 <- tempdata2 %>%
        filter(model==6) %>%
        select(volume) %>%
        as.numeric()
    }
    
    else if(model == 7) {
      tempdata3 <- tempdata2 %>%
        filter(model==7) %>%
        select(volume) %>%
        as.numeric()
    }
    
    tempdata3
    
  }
  return(tempdata3)
  
}


# NIFoS.taper parameter ---------------------------------------------------------

NIFoS_taper_parameters <- read_excel("C:/Daesung_R/taper_volume_functions/NIFoS taper parameter.xlsx",
                                     skip = 2)

# Read the NIFoS report for more information.
# 국립산림과학원. 2021. 입목재적 바이오매스 및 임분수확표
# Kozak 1988 Model parameters.

# Pd	Pinus densiflora 강원지방소나무
# Pd2	Pinus densiflora 중부지방소나무
# Pk	Pinus koraiensis
# Pr	Pinus rigida
# Lk	Larix kaempferi
# Qa	Quercus acutissima
# Qm	Quercus mongolica
# Qs	Quercus serrata
# Co	Chamaecyparis obtusa
# Bp	Betula platyphylla var. japonica
# Qv	Quercus variabilis
# Lt	Liriodendron tulipifera L.
# Pt	Pinus thunbergii Parl.
# Cj	Cryptomeria japonica

NIFoS_taper_parameters2 <- NIFoS_taper_parameters %>% 
  pivot_longer(cols = -c(parameter), names_to = "SP") %>% 
  pivot_wider(names_from = parameter, values_from = value)

  # mutate(parm = paste0(species,".",parameter),
  #        wider = 1) %>% 
  # select(parm, value) %>%
  # pivot_wider(names_from=parm, values_from=value)


# NIFoS.taper.diameter function -------------------------------------------------

NIFoS.taper.diameter <- function(dbh,height,upper_height,species, print_info = TRUE) {
  
  if (species=="Pd") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pd",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  
  if (species=="Pd2") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pd2",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Pk") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pk",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Pr") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pr",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Lk") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Lk",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Qa") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qa",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Qm") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qm",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Qs") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qs",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Co") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Co",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Bp") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Bp",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Qv") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qv",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Lt") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Lt",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Pt") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pt",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
  
  if (species=="Cj") {
    tempdata <- data.frame(dbh, height, upper_height, species) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Cj",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H))) %>%
      select(d) %>%
      as.numeric()
    
    tempdata
    
  }
 
   return(tempdata)

}
  
# NIFoS.taper.volume function ---------------------------------------------

NIFoS.taper.volume <- function(dbh,height,species, print_info = TRUE) {
  
  if (species=="Pd") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pd",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
             ) %>% 
      group_by(dbh,height) %>%
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  if (species=="Pd2") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pd2",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  if (species=="Pk") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pk",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  if (species=="Pr") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pr",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  
  if (species=="Lk") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Lk",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  if (species=="Qa") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qa",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  if (species=="Qm") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qm",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  if (species=="Qs") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qs",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  
  if (species=="Co") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Co",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  
  
  if (species=="Bp") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Bp",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  if (species=="Qv") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Qv",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  if (species=="Lt") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Lt",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  if (species=="Pt") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Pt",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  
  
  if (species=="Cj") {
    tempdata <- data.frame(dbh, height, species) %>% 
      expand_grid(upper_height=seq(0,height,0.1)) %>% 
      mutate(upper_height = case_when(height>=upper_height ~ upper_height,
                                      height<upper_height ~ NA_real_)) %>% 
      filter(!is.na(upper_height)) %>% 
      bind_cols(NIFoS_taper_parameters2[NIFoS_taper_parameters2$SP=="Cj",]) %>% 
      mutate(D = dbh,
             H = height,
             h = upper_height,
             p = 0.2,
             Hrt = upper_height/height,
             Xi = (1-Hrt^(1/2))/(1-p^(1/2))
      ) %>% 
      mutate(X = (1-sqrt(upper_height/height))/(1-sqrt(p)),
             d = a1*(D^a2)*(a3^D)*Xi^(b1*(Hrt^2)+b2*log(Hrt+0.001)+b3*(Hrt^(1/2))+b4*exp(Hrt)+b5*(D/H)),
      ) %>% 
      group_by(dbh,height) %>% 
      mutate(ba = (pi/40000)*(d**2),
             ba2= lag(ba),
             volume= case_when(height > upper_height ~ ((ba+ba2)/2)*0.1,
                               height == upper_height ~ (ba2/3)*0.1),
      ) %>% 
      ungroup()
    
    tempdata2 <- tempdata %>% 
      select(dbh,height,upper_height,str_subset(colnames(tempdata), "volume")) %>% 
      summarize(volume = sum(volume,na.rm=T)) %>% 
      ungroup() 
    
    tempdata3 <- tempdata2 %>%
      select(volume) %>%
      as.numeric()
    
    tempdata3
    
  }
  
  return(tempdata3)
  
}


# DLee.volume.equation parameter ---------------------------------------------

# Read the article for more information
# Daesung Lee, Yeongwan Seo & Jungkee Choi (2017).
# Estimation and validation of stem volume equations for Pinus densiflora, Pinus koraiensis, and Larix kaempferi in South Korea
# Forest Science and Technology, 13:2, 77-82, 
# DOI: 10.1080/21580103.2017.1315963

daesung_volume_equation_parameters <- read_excel("C:/Daesung_R/taper_volume_functions/daesung volume equation parameter.xlsx") %>% 
  select(species:d)

# daesung_volume_equation_parameters2 <- daesung_volume_equation_parameters %>% 
#   pivot_longer(cols = -c(species,model_no)) %>% 
#   mutate(parm = paste0(species,".",model_no,".",name),
#          wider = 1) %>% 
#   select(parm, value) %>%
#   pivot_wider(names_from=parm, values_from=value)


# DLee.volume.eq.DH function ----------------------------------------------------
DLee.volume.eq.DH <- function(dbh,height,model,species, print_info = TRUE) {
  
    tempdata <- data.frame(dbh, height, model, species) %>% 
      left_join(daesung_volume_equation_parameters, by = c("model"="model_no", "species"="species")) %>% 
      mutate(model_1_volume = a + b*dbh^2,
             model_2_volume = a + b*dbh + c*dbh^2,
             model_3_volume = a*dbh + b*dbh^2,
             model_4_volume = a + b*(dbh^2)*height,
             model_5_volume = a*(dbh^2)*height,
             model_6_volume = a + b*(dbh^c)*(height^d),
             model_7_volume = exp(a + b*log(dbh) + c*log(height)),
             model_8_volume = (dbh^2)/(a+b/height))
    
    if (species=="Pd") {
      
      tempdata2 <- tempdata %>% 
        filter(species=="Pd")
      
      if(model == 1) {
        tempdata3 <- tempdata2 %>% 
          filter(model==1) %>% 
          select(model_1_volume) %>% 
          as.numeric()
      }
      
      else if(model == 2) {
        tempdata3 <- tempdata2 %>% 
          filter(model==2) %>% 
          select(model_2_volume) %>% 
          as.numeric()
      }
    
    else if(model == 3) {
      tempdata3 <- tempdata2 %>% 
        filter(model==3) %>% 
        select(model_3_volume) %>% 
        as.numeric()
    }
    
    else if(model == 4) {
      tempdata3 <- tempdata2 %>% 
        filter(model==4) %>% 
        select(model_4_volume) %>% 
        as.numeric()
    }
    
    else if(model == 5) {
      tempdata3 <- tempdata2 %>% 
        filter(model==5) %>% 
        select(model_5_volume) %>% 
        as.numeric()
    }
    
    else if(model == 6) {
      tempdata3 <- tempdata2 %>% 
        filter(model==6) %>% 
        select(model_6_volume) %>% 
        as.numeric()
    }
    
    else if(model == 7) {
      tempdata3 <- tempdata2 %>% 
        filter(model==7) %>% 
        select(model_7_volume) %>% 
        as.numeric()
    }
    
      else if(model == 8) {
        tempdata3 <- tempdata2 %>% 
          filter(model==8) %>% 
          select(model_8_volume) %>% 
          as.numeric()
      }
      
    tempdata3
    
    }
    
    
    if (species=="Pk") {
      
      tempdata2 <- tempdata %>% 
        filter(species=="Pk")
      
      if(model == 1) {
        tempdata3 <- tempdata2 %>% 
          filter(model==1) %>% 
          select(model_1_volume) %>% 
          as.numeric()
      }
      
      else if(model == 2) {
        tempdata3 <- tempdata2 %>% 
          filter(model==2) %>% 
          select(model_2_volume) %>% 
          as.numeric()
      }
      
      else if(model == 3) {
        tempdata3 <- tempdata2 %>% 
          filter(model==3) %>% 
          select(model_3_volume) %>% 
          as.numeric()
      }
      
      else if(model == 4) {
        tempdata3 <- tempdata2 %>% 
          filter(model==4) %>% 
          select(model_4_volume) %>% 
          as.numeric()
      }
      
      else if(model == 5) {
        tempdata3 <- tempdata2 %>% 
          filter(model==5) %>% 
          select(model_5_volume) %>% 
          as.numeric()
      }
      
      else if(model == 6) {
        tempdata3 <- tempdata2 %>% 
          filter(model==6) %>% 
          select(model_6_volume) %>% 
          as.numeric()
      }
      
      else if(model == 7) {
        tempdata3 <- tempdata2 %>% 
          filter(model==7) %>% 
          select(model_7_volume) %>% 
          as.numeric()
      }
      
      else if(model == 8) {
        tempdata3 <- tempdata2 %>% 
          filter(model==8) %>% 
          select(model_8_volume) %>% 
          as.numeric()
      }
      
      tempdata3
      
    }
    
    
    if (species=="Lk") {
      
      tempdata2 <- tempdata %>% 
        filter(species=="Lk")
      
      if(model == 1) {
        tempdata3 <- tempdata2 %>% 
          filter(model==1) %>% 
          select(model_1_volume) %>% 
          as.numeric()
      }
      
      else if(model == 2) {
        tempdata3 <- tempdata2 %>% 
          filter(model==2) %>% 
          select(model_2_volume) %>% 
          as.numeric()
      }
      
      else if(model == 3) {
        tempdata3 <- tempdata2 %>% 
          filter(model==3) %>% 
          select(model_3_volume) %>% 
          as.numeric()
      }
      
      else if(model == 4) {
        tempdata3 <- tempdata2 %>% 
          filter(model==4) %>% 
          select(model_4_volume) %>% 
          as.numeric()
      }
      
      else if(model == 5) {
        tempdata3 <- tempdata2 %>% 
          filter(model==5) %>% 
          select(model_5_volume) %>% 
          as.numeric()
      }
      
      else if(model == 6) {
        tempdata3 <- tempdata2 %>% 
          filter(model==6) %>% 
          select(model_6_volume) %>% 
          as.numeric()
      }
      
      else if(model == 7) {
        tempdata3 <- tempdata2 %>% 
          filter(model==7) %>% 
          select(model_7_volume) %>% 
          as.numeric()
      }
      
      else if(model == 8) {
        tempdata3 <- tempdata2 %>% 
          filter(model==8) %>% 
          select(model_8_volume) %>% 
          as.numeric()
      }
      
      tempdata3
      
    }
    
  return(tempdata3)
  
}

# DLee.volume.eq.D function ----------------------------------------------------
DLee.volume.eq.D <- function(dbh,model,species, print_info = TRUE) {
  
  tempdata <- data.frame(dbh, model, species) %>% 
    left_join(daesung_volume_equation_parameters, by = c("model"="model_no", "species"="species")) %>% 
    mutate(model_1_volume = a + b*dbh^2,
           model_2_volume = a + b*dbh + c*dbh^2,
           model_3_volume = a*dbh + b*dbh^2)
  
  if (species=="Pd") {
    
    tempdata2 <- tempdata %>% 
      filter(species=="Pd")
    
    if(model == 1) {
      tempdata3 <- tempdata2 %>% 
        filter(model==1) %>% 
        select(model_1_volume) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdata3 <- tempdata2 %>% 
        filter(model==2) %>% 
        select(model_2_volume) %>% 
        as.numeric()
    }
    
    else if(model == 3) {
      tempdata3 <- tempdata2 %>% 
        filter(model==3) %>% 
        select(model_3_volume) %>% 
        as.numeric()
    }
    
    tempdata3
    
  }
  
  
  if (species=="Pk") {
    
    tempdata2 <- tempdata %>% 
      filter(species=="Pk")
    
    if(model == 1) {
      tempdata3 <- tempdata2 %>% 
        filter(model==1) %>% 
        select(model_1_volume) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdata3 <- tempdata2 %>% 
        filter(model==2) %>% 
        select(model_2_volume) %>% 
        as.numeric()
    }
    
    else if(model == 3) {
      tempdata3 <- tempdata2 %>% 
        filter(model==3) %>% 
        select(model_3_volume) %>% 
        as.numeric()
    }
    
    tempdata3
    
  }
  
  
  if (species=="Lk") {
    
    tempdata2 <- tempdata %>% 
      filter(species=="Lk")
    
    if(model == 1) {
      tempdata3 <- tempdata2 %>% 
        filter(model==1) %>% 
        select(model_1_volume) %>% 
        as.numeric()
    }
    
    else if(model == 2) {
      tempdata3 <- tempdata2 %>% 
        filter(model==2) %>% 
        select(model_2_volume) %>% 
        as.numeric()
    }
    
    else if(model == 3) {
      tempdata3 <- tempdata2 %>% 
        filter(model==3) %>% 
        select(model_3_volume) %>% 
        as.numeric()
    }
    
    tempdata3
    
  }
  
  return(tempdata3)
  
}

# NIFoS biomass parameter --------------------------------------------------

NIFoS_biomass_parameters <- read_excel("C:/Daesung_R/taper_volume_functions/NIFoS Biomass parameter.xlsx")

# Read the NIFoS report for more information.
# 국립산림과학원. 2021. 입목재적 바이오매스 및 임분수확표
# Ⅳ. 입목 바이오매스표
# 부록 333-339 페이지

# Pd	Pinus densiflora Siebold & Zucc.
# Pd2	Pinus densiflora Siebold & Zucc.
# Pt	Pinus thunbergii Parl.
# Pr	Pinus rigida Mill.
# Pk	Pinus koraiensis Siebold & Zucc.
# Lk	Larix kaempferi (Lamb.) Carrière
# Cj	Cryptomeria japonica (Thunb. ex L.f.) D.Don
# Co	Chamaecyparis obtusa (Siebold & Zucc.) Endl.
# Qa	Quercus acutissima Carruth.
# Qv	Quercus variabilis Blume
# Qm	Quercus mongolica Fisch. ex Ledeb. 
# Bp	Betula platyphylla var. japonica


NIFoS_biomass_parameters2 <- NIFoS_biomass_parameters %>% 
  select(species,model_no,factor,a,b,c)

NIFoS_biomass_parameters2_DH <- NIFoS_biomass_parameters2 %>% 
  filter(model_no == 2)

NIFoS_biomass_parameters2_D <- NIFoS_biomass_parameters2 %>% 
  filter(model_no == 1)

# NIFoS.biomass.DH function --------------------------------------------

# the unit of model result is KG.
NIFoS.biomass.DH <- function(dbh,height,species,part, print_info = TRUE) {
  
  if (species=="Pd") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Pd") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Pd2") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Pd2") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Pt") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Pt") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Pr") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Pr") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Pk") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Pk") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Lk") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Lk") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Cj") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Cj") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Co") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Co") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Qa") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Qa") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Qv") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Qv") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Qm") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Qm") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Bp") {
    tempdata <- data.frame(dbh, height, species) %>% 
      left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
      filter(species=="Bp") %>% 
      mutate(biomass = a*(dbh^b)*(height^c))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  return(tempdata2)
}


# NIFoS.biomass.D function --------------------------------------------

# the unit of model result is KG.
NIFoS.biomass.D <- function(dbh,species,part, print_info = TRUE) {
  
  if (species=="Pd") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Pd") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Pd2") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Pd2") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Pt") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Pt") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Pr") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Pr") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Pk") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Pk") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Lk") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Lk") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Cj") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Cj") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Co") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Co") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Qa") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Qa") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Qv") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Qv") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Qm") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Qm") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Bp") {
    tempdata <- data.frame(dbh, species) %>% 
      left_join(NIFoS_biomass_parameters2_D, by = c("species"="species")) %>% 
      filter(species=="Bp") %>% 
      mutate(biomass = a*(dbh^b))
    
    if (part == "stem") {
      tempdata2 <- tempdata %>% 
        filter(factor=="stem") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "whole") {
      tempdata2 <- tempdata %>% 
        summarize(biomass = sum(biomass, na.rm=T)) %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  return(tempdata2)
}

# NIFoS.biomass.conversion parameter --------------------------------------

NIFoS_biomass_conversion_parameters <- read_excel("C:/Daesung_R/taper_volume_functions/NIFoS biomass conversion factor.xlsx")

# Read the NIFoS report for more information.
# 국립산림과학원. 2021. 입목재적 바이오매스 및 임분수확표
# Ⅵ. 입목바이오매스표
# 부록 202-225 페이지

# Pd	Pinus densiflora Siebold & Zucc.
# Pd2	Pinus densiflora Siebold & Zucc.
# Pt	Pinus thunbergii Parl.
# Pr	Pinus rigida Mill.
# Pk	Pinus koraiensis Siebold & Zucc.
# Lk	Larix kaempferi (Lamb.) Carrière
# Cj	Cryptomeria japonica (Thunb. ex L.f.) D.Don
# Co	Chamaecyparis obtusa (Siebold & Zucc.) Endl.
# Qa	Quercus acutissima Carruth.
# Qv	Quercus variabilis Blume
# Qm	Quercus mongolica Fisch. ex Ledeb. 
# Bp	Betula platyphylla var. japonica

NIFoS_biomass_conversion_parameters2 <- NIFoS_biomass_conversion_parameters %>% 
  select(species,twig,leaf,root) %>% 
  pivot_longer(cols = -c(species), names_to = "factor", values_to = "parm")

# NIFoS.biomass.conversion function --------------------------------------------

# the unit of model result is KG.
NIFoS.biomass.conversion <- function(stem_biomass,species,part, print_info = TRUE) {
  
  if (species=="Pd") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Pd") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Pd2") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Pd2") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Pt") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Pt") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }

  }
  
  
  if (species=="Pr") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Pr") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Pk") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Pk") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Lk") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Lk") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Cj") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Cj") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  if (species=="Co") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Co") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Qa") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Qa") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Qv") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Qv") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Qm") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Qm") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  
  if (species=="Bp") {
    tempdata <- data.frame(stem_biomass, species) %>% 
      left_join(NIFoS_biomass_conversion_parameters2, by = c("species"="species")) %>% 
      filter(species=="Bp") %>% 
      mutate(biomass = parm*stem_biomass)
    
    if (part == "twig") {
      tempdata2 <- tempdata %>% 
        filter(factor=="twig") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "leaf") {
      tempdata2 <- tempdata %>% 
        filter(factor=="leaf") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
    if (part == "root") {
      tempdata2 <- tempdata %>% 
        filter(factor=="root") %>% 
        select(biomass) %>% 
        as.numeric()
    }
    
  }
  
  return(tempdata2)
}

# example volume functions -----------------------------------------------------------------

test.data <- expand_grid(dbh = seq(34,36,2),
                         height = seq(25,26,1),
                         model = seq(1,7,1),
                         species = c("Pd","Pk","Lk"))


NIFoS.taper.volume(30,20,"Pd")
NIFoS.taper.volume(30,20,"Pk")
NIFoS.taper.volume(30,20,"Lk")

DLee.taper.volume(30,20,2,"Pd")
DLee.taper.volume(30,20,3,"Pd")
DLee.taper.volume(40,27,3,"Pk")
DLee.taper.volume(40,27,3,"Lk")

DLee.volume.eq.DH(36,30,4,"Pd")
DLee.volume.eq.DH(36,30,4,"Pk")
DLee.volume.eq.DH(36,26,4,"Lk")

DLee.volume.eq.D(36,3,"Pd")
DLee.volume.eq.D(36,3,"Pk")
DLee.volume.eq.D(36,3,"Lk")
DLee.volume.eq.DH(36,26,3,"Lk")

test.data %>%
  rowwise() %>% 
  mutate(
    NIFoS = NIFoS.taper.volume(dbh,height,species),
    DLee = DLee.taper.volume(dbh,height,model,species)) %>% 
  ungroup() %>% 
  print(n=100)


test.data2 <- expand_grid(dbh = seq(34,36,2),
                          height = seq(25,26,1),
                          model = seq(1,8,1),
                          species = c("Pd","Pk","Lk"))

test.data2 %>%
  rowwise() %>% 
  mutate(DLee.eq1 = DLee.volume.eq.DH(dbh,height,model,species)) %>% 
  print(n=35)

test.data3 <- expand_grid(dbh = seq(34,36,2),
                          model = seq(1,3,1),
                          species = c("Pd","Pk","Lk"))

test.data3 %>%
  rowwise() %>% 
  mutate(DLee.eq2 = DLee.volume.eq.D(dbh,model,species)) %>% 
  print(n=35)

# example biomass functions ----------------------------------------------------------------

test.data.bimoass <- expand_grid(dbh = seq(24,24,2),
                                 height = seq(16,16,1),
                                 species = c("Pd","Pd2","Pt","Pr","Pk","Lk","Cj","Co","Qa","Qv","Qm","Bp"))

test.data.bimoass %>% 
  rowwise() %>% 
  mutate(stem1 = NIFoS.biomass.DH(dbh,height,species,"stem"),
         twig1 = NIFoS.biomass.DH(dbh,height,species,"twig"),
         leaf1 = NIFoS.biomass.DH(dbh,height,species,"leaf"),
         root1 = NIFoS.biomass.DH(dbh,height,species,"root"),
         whole1 = NIFoS.biomass.DH(dbh,height,species,"whole"),
         stem2 = NIFoS.biomass.D(dbh,species,"stem"),
         twig2 = NIFoS.biomass.D(dbh,species,"twig"),
         leaf2 = NIFoS.biomass.D(dbh,species,"leaf"),
         root2 = NIFoS.biomass.D(dbh,species,"root"),
         whole2 = NIFoS.biomass.D(dbh,species,"whole"))


test.data.bimoass %>% 
  rowwise() %>% 
  mutate(stem1 = NIFoS.biomass.DH(dbh,height,species,"stem"),
         stem2 = NIFoS.biomass.D(dbh,species,"stem"),
         twig1 = NIFoS.biomass.DH(dbh,height,species,"twig"),
         twig2 = NIFoS.biomass.D(dbh,species,"twig"),
         leaf1 = NIFoS.biomass.DH(dbh,height,species,"leaf"),
         leaf2 = NIFoS.biomass.D(dbh,species,"leaf"),
         root1 = NIFoS.biomass.DH(dbh,height,species,"root"),
         root2 = NIFoS.biomass.D(dbh,species,"root"),
         whole1 = NIFoS.biomass.DH(dbh,height,species,"whole"),
         whole2 = NIFoS.biomass.D(dbh,species,"whole"))

test.data.bimoass2 <- expand_grid(dbh = seq(24,24,2),
                                 height = seq(16,16,1),
                                 species = c("Pd","Pd2","Pt","Pr","Pk","Lk","Cj","Co","Qa","Qv","Qm","Bp"),
                                 part = c("stem","twig","leaf","root","whole"))

test.data.bimoass2 %>% 
  rowwise() %>% 
  mutate(biomass.DH = NIFoS.biomass.DH(dbh,height,species,part),
         biomass.D = NIFoS.biomass.D(dbh,species,part))

test.data.bimoass %>% 
  left_join(NIFoS_biomass_parameters2_DH, by = c("species"="species")) %>% 
  print(n=40)



test.data.bimoass.conversion <- expand_grid(stem_biomass = seq(150,170,20),
                                            species = c("Pd","Pd2","Pt","Pr","Pk","Lk","Cj","Co","Qa","Qv","Qm","Bp"),
                                            part = c("twig","leaf","root")) 
test.data.bimoass.conversion.result <- test.data.bimoass.conversion %>% 
  rowwise() %>% 
  mutate(part_biomass = NIFoS.biomass.conversion(stem_biomass,species,part)) %>% 
  ungroup()
test.data.bimoass.conversion.result %>% 
  pivot_wider(names_from = part, values_from=part_biomass)

# example taper diameter functions --------------------------------------------------

taper.line.test <- expand_grid(dbh = seq(36,36,2),
                               height = seq(26,26,1),
                               upper_height = seq(0,26,0.1),
                               model = seq(1,7,1),
                               species = c("Lk"))

taper.line.test.pred <- taper.line.test %>% 
  rowwise() %>% 
  mutate(d = DLee.taper.diameter(dbh,height,upper_height,model,species))

taper.line.test.pred %>% 
  filter(d<dbh+10) %>% 
  ggplot(aes(x=upper_height,y=d,col=factor(model))) +
  geom_line()


taper.line.test2 <- expand_grid(dbh = seq(36,36,2),
                               height = seq(26,26,1),
                               upper_height = seq(0,26,0.1),
                               species = c("Pd","Pk","Lk"))

taper.line.test.pred2 <- taper.line.test2 %>% 
  rowwise() %>% 
  mutate(daesung = DLee.taper.diameter(dbh,height,upper_height,model=3,species),
         NIFoS = NIFoS.taper.diameter(dbh,height,upper_height,species)) %>% 
  ungroup() 

taper.line.test.pred3 <- taper.line.test.pred2 %>% 
  pivot_longer(cols = -c(dbh,height,upper_height,species), names_to = "paper", values_to = "top_d")

taper.line.test.pred3 %>% 
  ggplot(aes(x=upper_height,y=top_d,col=factor(paper))) +
  geom_line() +
  facet_wrap(~species)

# 분석 결과, 이대성 간곡선식 모델이 근원부 주변에서 상부직경이 더 크게 나타남.
# 이 결과는 강원 경북 지역이 국내 타 남부지역과 비교했을 때의 수간형의 차이로 해석될 수도 있으며,
# 혹은 전국 수집 표본과 국내 강원 경북 지역의 수집 표본간의 임분의 밀도에 따르거나 수령에 따른 수간형에 차이일 수도 있음.
# 이대성 모델은 산림청 모니터링 표본 수집자료를 사용한 것이며, 국립산림과학원 자료는 전국 경급별 27개 국유림관리소로부터 수집된 자료음. 
# 참고로 당시 낙엽송 강원지역 표본은 2015년에 이대성이 현장에서 수집하여 전달한 것임. 

## 결론적으로 강원 및 경북지역의 재적예측을 위해서 이대성 논문으로 쓰는 것은 문제 없다고 판단됨.

# example taper volume functions between Daesung vs NIFoS -------------------------------

comparison.test <- expand_grid(dbh = seq(34,36,2),
                               height = seq(24,26,1),
                               species = c("Lk"))

comparison.test %>%
  rowwise() %>% 
  mutate(
    NIFoS = NIFoS.taper.volume(dbh,height,species),
    DLee1 = DLee.taper.volume(dbh,height,model=3,species),
    DLee.eq = DLee.volume.eq.DH(dbh,height,model=4,species)) %>% 
  ungroup() %>% 
  print(n=100)

# 위 결과를 보다시피, 예측 재적의 크기는 NIFoS < Lee (2017) 이변수재적식 <= Lee (2017) 수간곡선식 임. 이대성 논문 내 이변수 vs 수간곡선 재적예측은 큰 차이는 없음.
# 결론적으로 강원 및 경북지역의 재적예측을 위해서는 이대성 논문의 이변수 재적식이나 수간곡선식으로부터 재적을 쓰는 것이 더 합리적이라고 판단됨.
# 다만 이대성 논문의 이변수재적식 활용 시 V ~ f(D)를 쓸 경우, 경급 범위에 따라서 마이너스 재적예측값이 나오기 때문에, 외삽(extrapolation) 시 주의 요망.


