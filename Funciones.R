# #Set up del directorio en donde se esta trabajando. Siempre abrir en la carpeta del Onedrive
# current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(current_working_dir)
# 
# trabajos <- data.frame(Trabajo = paste("T",1:4,sep = ""),
#                        pj = c(3, 3, 3, 8),
#                        dj = c(4, 5, 8, 9))
# 
# grupos <- data.frame(Grupo = paste("M",1:2,sep = ""),
#                        vi = c(1,1))
# 
# grupos <- read.csv("Grupos.csv")
# trabajos <- read.csv("Trabajos.csv")

runifdisc <- function(n, min, max) sample(min:max, n, replace=T)

tProc <- function(trabajos){
  trabajos$pj <- rep(0,nrow(trabajos))
  trabajos$pj[which(trabajos$Tipo == "Persona")] <- runifdisc(length(which(trabajos$Tipo == "Persona")), 1, 3)
  trabajos$pj[which(trabajos$Tipo == "Inmobiliaria")] <- runifdisc(length(which(trabajos$Tipo == "Inmobiliaria")), 2, 5)
  for(k in which(trabajos$Tipo == "Constructora")){
    trabajos$pj[k] <- runifdisc(1, ceiling(2*trabajos$Apartamentos[k]/3), trabajos$Apartamentos[k])
  }
  trabajos$pj <- as.integer(trabajos$pj)
  
  return(trabajos)
}

transformada <- function(trabajos){
  trabajos$rj <- max(trabajos$dj) - trabajos$dj
  #trabajos$rj[-which.min(trabajos$rj)] <- trabajos$rj[-which.min(trabajos$rj)] + z
  trabajos$RT <- trabajos$pj
  return(trabajos)
}

velocidades <- function(grupos){
  grupos$vi <- max(grupos$Personas)/grupos$Personas
  return(grupos)
}

#Longest Remaining Processing Time on Fastest Machine
LRPT <- function(trabajos, grupos){
  z <- 0
  Cmax <- 1e10
  grupos <- velocidades(grupos)
  trabajos <- tProc(trabajos)
  
  while(Cmax >= max(trabajos$dj)+z){
    trabajos <- transformada(trabajos)
    t <- 0
    sec <- list()
    for(i in grupos$Grupo){
      sec[[i]] <- c("-")
    }
    Maq <- grupos[order(grupos$vi),]
    while(sum(pmax(trabajos$RT,0)) > 0){
      disp <- trabajos[which((trabajos$rj <= t) & (trabajos$RT > 0)),]
      disp <- disp[order(disp$dj, decreasing = TRUE),]
      
      for(i in Maq$Grupo){
        if(nrow(disp) > 0){
          sec[[i]] <- c(sec[[i]],disp[which.max(disp$RT),"Trabajo"])
          trabajos[which(trabajos$Trabajo == disp[which.max(disp$RT),"Trabajo"]),"RT"] <- 
            trabajos[which(trabajos$Trabajo == disp[which.max(disp$RT),"Trabajo"]),"RT"] - 1/grupos[which(grupos$Grupo == i),"vi"]
          disp <- disp[-which.max(disp$RT),]
          
        }else{
          sec[[i]] <- c(sec[[i]],"-")
        }
      }
      t <- t+1
    }
    for(i in grupos$Grupo){
      sec[[i]] <- sec[[i]][length(sec[[i]]):1]
    }
    z <- z+1
    Cmax <- length(sec[[1]])-1
  }
  trabajos <- entrega(trabajos, sec)
  sol <- list(sec = sec, Lmax = z-1, Cmax = Cmax, trabajos = trabajos, grupos = grupos)
  return(sol)
}

gantt <- function(sol){
  
  grupos <- sol$grupos
  trabajos <- sol$trabajos
  
  cols <- rainbow(length(unique(trabajos$Trabajo)), alpha = 0.5)
  trabajos$color <- factor(trabajos$Trabajo, labels = cols)
  
  # Initialize empty plot
  fig <- plot_ly()
  for(i in 1:length(sol$sec)){
    sec <- sol$sec[[i]]
    veces <- 0
    for(t in 1:sol$Cmax){
      if((sec[t] != "-") & (sec[t] != sec[t+1])){
        veces <- veces + 1
        fig <- add_trace(fig,
                         x = c(t-veces, t),  # x0, x1
                         y = c(length(sol$sec)-i+1, length(sol$sec)-i+1),  # y0, y1
                         mode = "lines",
                         line = list(color = trabajos$color[which(trabajos$Trabajo == sec[t])], width = 200/length(sol$sec)),
                         showlegend = F,
                         hoverinfo = "text",
                         
                         # Create custom hover text
                         
                         text = paste("Trabajo: ", sec[t], "<br>",
                                      "Grupo: ", grupos$Grupo[i], "<br>",
                                      "Inicio: ", t-veces, "<br>",
                                      "Fin: ", t, "<br>",
                                      "Procesado: ", veces/grupos$vi[i]),
                         
                         evaluate = T  # needed to avoid lazy loading
        )
        veces <- 0
      }else if(sec[t] == sec[t+1]){
        veces <- veces + 1
      }
    }
  }
  
  fig <- layout(fig,
                xaxis = list(title = "Tiempo", range = c(0,sol$Cmax+0.5)),
                yaxis = list(showgrid = F,
                             tickmode = "array", tickvals = 1:length(sol$sec), ticktext = names(sol$sec)[length(sol$sec):1],
                             range = c(0.7,length(sol$sec)+0.3)))
  
  return(fig)
}

entrega <- function(trabajos, sec){
  trabajos$Fin <- rep(0,nrow(trabajos))
  
  for(i in names(sec)){
    for(j in 1:nrow(trabajos)){
      trabajos[j,"Fin"] <- max(which(sec[[i]] == trabajos[j,"Trabajo"]),trabajos[j,"Fin"])
    }
  }
  trabajos$Fin <- as.integer(trabajos$Fin)
  trabajos$Lj <- as.integer(trabajos$Fin - trabajos$dj)
  return(trabajos)
}

