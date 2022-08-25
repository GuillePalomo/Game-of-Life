
#Hemos decidido no poner caracteres como tildes en el trabajo por si acaso

game_of_life <- function() {
 #Usamos funciones entrelazadas para que sea mas sencilla la edicion del programa
 presentar_datos <- function(tablerocualq){
  colnames(tablerocualq) <- 1:ncol(tablerocualq)
  View(tablerocualq)
 }
 set_game <- function(){
    
  print("Hola, esto es nuestra version del Game of life de Conway.")
  
  #Comprobaciones para que filas y columnas sean numeros enteros basadas en los ejercicios de clase
  #Tambien diferente a los ejercicios de clase esta la comprobacion para que sean diferente de 0
  
  filas <- readline("Introduce el numero de filas del tablero: ")
  if (grepl("^[0-9]+$",filas)) {
    entero <- TRUE
    filas <- as.integer(filas)}
  else {entero <- FALSE}
  
  while (!entero){
    filas = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
    if (grepl("^[0-9]+$",filas)) {
      entero <- TRUE
      filas <- as.integer(filas)
    }
    else {entero <- FALSE}
  }
  while(filas == 0){
    filas <- readline("El 0 no vale, prueba otra vez.")
    if (grepl("^[0-9]+$",filas)) {
      entero <- TRUE
      filas <- as.integer(filas)}
    else {entero <- FALSE}
    
    while (!entero){
      filas = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
      if (grepl("^[0-9]+$",filas)) {
        entero <- TRUE
        filas <- as.integer(filas)
      }
      else {entero <- FALSE}
    }
  }
  
  print("Vale, perfecto.")
  
  columnas <- readline("Ahora introduce el numero de columnas que quieras: ")
  if (grepl("^[0-9]+$",columnas)) {
    entero <- TRUE
    columnas <- as.integer(columnas)}
  else {entero <- FALSE}
  
  while (!entero){
    columnas = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
    if (grepl("^[0-9]+$",columnas)) {
      entero <- TRUE
      columnas <- as.integer(columnas)
    }
    else {entero <- FALSE}
  }
  while(columnas == 0){
    columnas <- readline("El 0 no vale, prueba otra vez.")
    if (grepl("^[0-9]+$",columnas)) {
      entero <- TRUE
      columnas <- as.integer(columnas)}
    else {entero <- FALSE}
    
    while (!entero){
      columnas = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
      if (grepl("^[0-9]+$",filas)) {
        entero <- TRUE
        columnas <- as.integer(columnas)
      }
      else {entero <- FALSE}
    }
  }
  
  #Creamos el tablero de la generacion 0
  
  tablero <- matrix(data = 0, nrow = filas, ncol = columnas)
  num_celdas <- filas * columnas
  print(paste("El numero de celdas maximo que puedes introducir es", num_celdas, "."))
  print("Cuando quieras terminar y pasar a producir la primera generacion deberas escribir 't'.") 
  
  #Creamos las opciones y nos aseguramos que las coordenadas al añadir y eliminar sean enteros dentro del tablero
  
  opciones <- ""
  while(opciones != "t"){
  opciones <- readline("añadir('a'), eliminar('e'), terminar('t'), editar manualmente('m') o añadir celulas nodriza('n'): ")
  
   if(opciones == "a"){
     
     print("Recuerda que tus coordenadas no pueden estar fuera del tablero.")
     
     cell_fila <- readline("Primero dime en que fila quieres la celula.")
     if (grepl("^[0-9]+$",cell_fila)) {
       entero <- TRUE
       cell_fila <- as.integer(cell_fila)}
     else {entero <- FALSE}
     
     while (!entero){
       cell_fila = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
       if (grepl("^[0-9]+$",cell_fila)) {
         entero <- TRUE
         cell_fila <- as.integer(cell_fila)
       }
       else {entero <- FALSE}
     }
     while(cell_fila > filas | cell_fila < 1){
       cell_fila <- readline("Este numero esta fuera de la dimension del tablero. Prueba otra vez.")
       if (grepl("^[0-9]+$",cell_fila)) {
         entero <- TRUE
         cell_fila <- as.integer(cell_fila)}
       else {entero <- FALSE}
       
       while (!entero){
         cell_fila = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
         if (grepl("^[0-9]+$",cell_fila)) {
           entero <- TRUE
           cell_fila <- as.integer(cell_fila)
         }
         else {entero <- FALSE}
       }
     }
     
     cell_columna <- readline("Ahora dime en que columna quieres tu celula.")
     if (grepl("^[0-9]+$",cell_columna)) {
       entero <- TRUE
       cell_columna <- as.integer(cell_columna)}
     else {entero <- FALSE}
     
     while (!entero){
       cell_columna = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
       if (grepl("^[0-9]+$",cell_columna)) {
         entero <- TRUE
         cell_columna <- as.integer(cell_columna)
       }
       else {entero <- FALSE}
     }
     while(cell_columna > columnas | cell_columna < 1){
       cell_columna <- readline("Este numero esta fuera de la dimension del tablero. Prueba otra vez.")
       if (grepl("^[0-9]+$",cell_columna)) {
         entero <- TRUE
         cell_columna <- as.integer(cell_columna)}
       else {entero <- FALSE}
       
       while (!entero){
         cell_columna = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
         if (grepl("^[0-9]+$",cell_columna)) {
           entero <- TRUE
           cell_columna <- as.integer(cell_columna)
         }
         else {entero <- FALSE}
       }
     }
     
     #Aqui se introducen las coordenadas en el tablero
     
     celulas <- c(cell_fila,cell_columna)

     if(tablero[celulas[1], celulas[2]] == 0){
       tablero[celulas[1], celulas[2]] <- 1
       presentar_datos(tablero)
      } else {
       presentar_datos(tablero)
       print("Aqui ya hay una celula.")
      }
     
   } 
   else if(opciones == "e"){
     
     print("Recuerda que tus coordenadas no pueden estar fuera del tablero.")
     
     cell_fila <- readline("Primero dime la fila en la que se encuentra la celula a eliminar.")
     if (grepl("^[0-9]+$",cell_fila)) {
       entero <- TRUE
       cell_fila <- as.integer(cell_fila)}
     else {entero <- FALSE}
     
     while (!entero){
       cell_fila = readline ("Eso no es un número entero positivo, vamos a probar otra vez.")
       if (grepl("^[0-9]+$",cell_fila)) {
         entero <- TRUE
         cell_fila <- as.integer(cell_fila)
       }
       else {entero <- FALSE}
     }
     while(cell_fila > filas | cell_fila < 1){
       cell_fila <- readline("Este numero esta fuera de la dimension del tablero. Prueba otra vez.")
       if (grepl("^[0-9]+$",cell_fila)) {
         entero <- TRUE
         cell_fila <- as.integer(cell_fila)}
       else {entero <- FALSE}
       
       while (!entero){
         cell_fila = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
         if (grepl("^[0-9]+$",cell_fila)) {
           entero <- TRUE
           cell_fila <- as.integer(cell_fila)
         }
         else {entero <- FALSE}
       }
     }
     
     cell_columna <- readline("Ahora dime la columna de la celula a eliminar.")
     if (grepl("^[0-9]+$",cell_columna)) {
       entero <- TRUE
       cell_columna <- as.integer(cell_columna)}
     else {entero <- FALSE}
     
     while (!entero){
       cell_columna = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
       if (grepl("^[0-9]+$",cell_columna)) {
         entero <- TRUE
         cell_columna <- as.integer(cell_columna)
       }
       else {entero <- FALSE}
     }
     while(cell_columna > columnas | cell_columna < 1){
       cell_columna <- readline("Este numero esta fuera de la dimension del tablero. Prueba otra vez.")
       if (grepl("^[0-9]+$",cell_columna)) {
         entero <- TRUE
         cell_columna <- as.integer(cell_columna)}
       else {entero <- FALSE}
       
       while (!entero){
         cell_columna = readline ("Eso no es un numero entero positivo, vamos a probar otra vez.")
         if (grepl("^[0-9]+$",cell_columna)) {
           entero <- TRUE
           cell_columna <- as.integer(cell_columna)
         }
         else {entero <- FALSE}
       }
     }
     
     #Aqui se eliminan las celulas del tablero
     
     celulas <- c(cell_fila,cell_columna)
     
     if(tablero[celulas[1], celulas[2]] == 1 | tablero[celulas[1], celulas[2]] == 2){
       tablero[celulas[1], celulas[2]] <- 0
       presentar_datos(tablero)
     } else {
       presentar_datos(tablero)
       print("Aqui no hay una celula que eliminar.")
     }
     
   } 
   else if(opciones == "t"){
     
     print("Esta es la generacion 0")
     presentar_datos(tablero)
     return(tablero)
 
   } 
   else if(opciones == "m"){
     
     print("Recuerda poner un '2' en las celulas que quieres que sean nodriza.")
     
     #creamos la opcion de que el usuario modifique el tablero con edit
     tablero <- edit(tablero)
     
     while(!all(tablero == 0 | tablero == 1 | tablero == 2)){
       print("El tablero solo puede tener 0, 1 o 2.")
       tablero <- edit(tablero)
     }
     
   }
   else if(opciones == "n"){
   
   print("Has decido añadir celulas nodriza, recuerda que tienen que estar dentro del tablero.")
   print("Las nodriza se representaran con un 2 en el tablero.")
    
    fila_nodriza <- readline("¿En que fila quieres la celula nodriza?: ")
    if (grepl("^[0-9]+$",fila_nodriza)) {
      entero <- TRUE
      fila_nodriza <- as.integer(fila_nodriza)}
    else {entero <- FALSE}
    
    while (!entero | fila_nodriza > filas | fila_nodriza < 1 ){
      fila_nodriza = readline ("Ese numero no es valido, vamos a probar otra vez: ")
      if (grepl("^[0-9]+$",fila_nodriza)) {
        entero <- TRUE
        fila_nodriza <- as.integer(fila_nodriza)
      }
      else {entero <- FALSE}
    }
    
    columna_nodriza <- readline("¿Ahora en que columna quieres tu celula nodriza?: ")
    if (grepl("^[0-9]+$",columna_nodriza)) {
      entero <- TRUE
      columna_nodriza <- as.integer(columna_nodriza)}
    else {entero <- FALSE}
    
    while (!entero | columna_nodriza > columnas | columna_nodriza < 1){
      columna_nodriza = readline ("Ese numero no es valido, vamos a probar otra vez: ")
      if (grepl("^[0-9]+$",columna_nodriza)) {
        entero <- TRUE
        columna_nodriza <- as.integer(columna_nodriza)
      }
      else {entero <- FALSE}
    }
    
    #Aqui se añaden celulas nodriza
    
    celulas <- c(fila_nodriza,columna_nodriza)
    
    if(tablero[celulas[1], celulas[2]] == 0){
      
      tablero[celulas[1], celulas[2]] <- 2
      presentar_datos(tablero)
      
    } else if(tablero[celulas[1], celulas[2]] == 2 | tablero[celulas[1], celulas[2]] == 1){
      
      print("Aqui ya hay una celula.")
      presentar_datos(tablero)
    }
    } 
   else {
     print("Este valor no vale, prueba otra vez con 'a', 'e', 'm', 'n' o 't'.")
   }
 }
 }
 tablerogen0 <- set_game()
 calcular_tableros <- function(tablerogen0){
  filas <- nrow(tablerogen0)
  columnas <- ncol(tablerogen0)
  tablero1 <- matrix(data = tablerogen0, nrow = filas, ncol = columnas)
      
      for (i in 1:filas){
        for (j in 1:columnas){
          
          vecinas<-0
          vecinonodriza <- FALSE
          
          #Usando la ayuda del foro
          
          for (n in -1:1){
            for (m in -1:1){
              
              if((n == 0 & m == 0 )|(i + n > filas)|(i + n < 1)|(j + m > columnas)|(j + m < 1)){
                
              }else if(tablerogen0[i + n, j + m]){
                
                vecinas <- vecinas + 1
              }
              #Condicion para las nodrizas
              if((n == 0 & m == 0 )|(i + n > filas)|(i + n < 1)|(j + m > columnas)|(j + m < 1)){
                
              }else if(tablerogen0[i + n, j + m] == 2){
                
                vecinonodriza <- TRUE
                
              }
            }
          }
          
          #Supervivencia
          if(tablerogen0[i,j] == 2){
            tablero1[i,j] <- 2
            
          }else if(vecinonodriza == TRUE & tablerogen0[i,j] == 1){
            
            tablero1[i,j] <- 1
            
          }else if(vecinas == 3 | vecinas == 2 & tablerogen0[i,j] == 1){
            tablero1[i,j] <- 1
            
          #Sobrepoblacion y soledad
          }else if(vecinas <= 1 | vecinas >= 4 & tablerogen0[i,j] == 1){
            tablero1[i,j] <- 0
          }
          #Reproduccion
          else if(vecinas == 3 & tablerogen0[i,j] == 0){
            tablero1[i,j] <- 1
          }
        }
      }
      return(tablero1)
   
 }
 calcular_tableros_vecindarioext <- function(tablerogen0){
   filas <- nrow(tablerogen0)
   columnas <- ncol(tablerogen0)
   tablero1 <- matrix(data = tablerogen0, nrow = filas, ncol = columnas)
   
   for (i in 1:filas){
     for (j in 1:columnas){
       
       vecinas<-0
       vecinonodriza <- FALSE
       
       #Usando la ayuda del foro
       
       for (n in -2:2){
         for (m in -2:2){
           
           if((n == 0 & m == 0 )|(i + n > filas)|(i + n < 1)|(j + m > columnas)|(j + m < 1)){
             
           }else if(tablerogen0[i + n, j + m]){
             
             vecinas <- vecinas + 1
             
           }
           #Condicion para las nodrizas
           if((n == 0 & m == 0 )|(i + n > filas)|(i + n < 1)|(j + m > columnas)|(j + m < 1)){
             
           }else if(tablerogen0[i + n, j + m] == 2){
             
             vecinonodriza <- TRUE
             
           }
         }
       }
       
       #Supervivencia
       if(tablerogen0[i,j] == 2){
         tablero1[i,j] <- 2 
       }else if(vecinonodriza == TRUE & tablerogen0[i,j] == 1){
         
         tablero1[i,j] <- 1
         
       }else if(vecinas == 3 | vecinas == 2 & tablerogen0[i,j] == 1){
         tablero1[i,j] <- 1
         
       #Sobrepoblacion y soledad
       }else if(vecinas <= 1 | vecinas >= 4 & tablerogen0[i,j] == 1){
         tablero1[i,j] <- 0
       }
       #Reproduccion
       else if(vecinas == 3 & tablerogen0[i,j] == 0){
         tablero1[i,j] <- 1
       }
     }
   }
   
   return(tablero1)
 }
 finalizar_programa <- function(){
 numgen <- 1  
 
 #Este es el bucle para contiuar o finalizar
 finalizar <- ""
 while (finalizar != "f") {
   finalizar <- readline("¿Deseas continuar con la siguiente generacion(enter) o finalizar el programa('f')?.")
   
   if(finalizar == ""){
     
     #Aqui creamos una cuenta para las generaciones
     
     print(paste("Esta es la generacion", numgen))
     numgen <- numgen + 1
     tablerogen0 <- calcular_tableros(tablerogen0)
     calcular_tableros(tablerogen0)
     presentar_datos(tablerogen0)
     
   } else if(finalizar == "f"){
     
    print("El juego acaba aqui.")
     
   } else {
     
     print("Esto no vale, enter o 'f'.")
     
   }
 }
 }
 finalizar_programa_vecext <- function(){
   numgen <- 1
   
   #Este es el bucle para continuar o finalizar
   finalizar <- ""
   while (finalizar != "f") {
     finalizar <- readline("¿Deseas continuar con la siguiente generacion() o finalizar el programa('f')?.")
     
     if(finalizar == ""){
       
       #Aqui creamos una cuenta para las generaciones
       
       print(paste("Esta es la generacion", numgen))
       numgen <- numgen + 1
       tablerogen0 <- calcular_tableros_vecindarioext(tablerogen0)
       presentar_datos(tablerogen0)
       calcular_tableros_vecindarioext(tablerogen0)
       
     } else if(finalizar == "f"){
       
       print("El juego acaba aqui.")
       
     } else {
       
       print("Esto no vale, enter o 'f'.")
       
     }
   }
 } 
 set_vecindario <- function(){
   
   #Aqui se elige el vecindario extendido o normal
   
   vecindario <- ""
   while (vecindario != "si" & vecindario != "no") {
     vecindario <- readline("¿Quieres usar el vecindario extendido? 'si' o 'no'.")
     if(vecindario == "si"){
       
       finalizar_programa_vecext()
       
     } else if(vecindario == "no"){
       
       finalizar_programa()
       
     } else {
       print("Eso no es valido, 'si' o 'no'.")
     }
   }
   }
 set_vecindario()
}
game_of_life()