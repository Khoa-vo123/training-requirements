#Function Defined
##########X turn#######################################################################
Xinputcomponent1<- function(){
  #(input row for X) 
  if (symbol == "X" ||  symbol =="O"){
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    cat("What row for", symbol,"?: ")
    place1X <- readLines(con = con, n = 1)
  } 
  #If row input is incorrect
  while (place1X != 1 && place1X != 2 && place1X != 3) {
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    cat("Use only row 1,2,or 3: ")
    place1X <- readLines(con = con, n = 1)
  }
  #(input column for X)
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  cat("What column for", symbol,"?: ")
  place2X <- readLines(con = con, n = 1)
  
  #If column input is incorrect
  while (place2X != 1 && place2X != 2 && place2X != 3) {
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    cat("Use only column 1,2,or 3: ")
    place2X <- readLines(con = con, n = 1)
  }
  #Checks if tile is already in use 
  while (match(df[[as.numeric(place1X),as.numeric(place2X)]],symbol,nomatch = FALSE)||
         match(df[[as.numeric(place1X),as.numeric(place2X)]],symbol2,nomatch = FALSE)){
    cat(paste(place1X,place2X,"tile already in use
"))
    if (symbol == "X" ||  symbol =="O"){
      if (interactive()) {
        con <- stdin()
      } else {
        con <- "stdin"
      }
      cat("What row for", symbol,"?: ")
      place1X <- readLines(con = con, n = 1)
    } 
    #If row input is incorrect
    while (place1X != 1 && place1X != 2 && place1X != 3) {
      if (interactive()) {
        con <- stdin()
      } else {
        con <- "stdin"
      }
      cat("Use only row 1,2,or 3: ")
      place1X <- readLines(con = con, n = 1)
    }
    #third part(input column for X)
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    cat("What column for", symbol,"?: ")
    place2X <- readLines(con = con, n = 1)
    
    #If column input is incorrect
    while (place2X != 1 && place2X != 2 && place2X != 3) {
      if (interactive()) {
        con <- stdin()
      } else {
        con <- "stdin"
      }
      cat("Use only column 1,2,or 3: ")
      place2X <- readLines(con = con, n = 1)
    }
    
  }
  place1X <<- place1X 
  place2X <<- place2X  
}

######################start of game ##############################################################
#Created Data Frame
df <- data.frame(
  "[,1]" = c(NA,NA,NA),
  "[,2]" = c(NA,NA,NA),
  "[,3]" = c(NA,NA,NA)
)
colnames(df) <-c("[,1]","[,2]","[,3]") 
rownames(df) <-c("[1,]","[2,]","[3,]")

#first part (input symbol)
if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}
cat("X or O?: ")
symbol <- readLines(con = con, n = 1)


#If symbol input is incorrect
while (symbol != "X" &&  symbol !="O"){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  cat("Use only X or O: ")
  symbol <- readLines(con = con, n = 1)
} 
#other symbol 
if (symbol == "X" &&  symbol !="O"){
  symbol2 <-"O"
} else{
  if (symbol != "X" &&  symbol =="O") {
    symbol2 <-"X"}
}
#################start of round loop########################################################################

numberofrounds <-c(1:6)
############if player chose X####################
if (symbol=="X"){
for (var in numberofrounds){
  cat(paste("Round#",var," 
"))
  Sys.sleep(2)
  
  cat("Current board:
")
  Sys.sleep(2)
  print(df)
  cat("Player", symbol,"turn","
")
  Sys.sleep(2)  
  
  Xinputcomponent1()
  
  df[[as.numeric(place1X),as.numeric(place2X)]] <- symbol
  
  print(df)
  
  
  x1 <- c(df[1,1],df[1,2],df[1,3])
  x2 <- c(df[1,1],df[2,1],df[3,1])
  x3 <- c(df[1,1],df[2,2],df[3,3])
  x4 <- c(df[1,2],df[2,2],df[3,2])
  x5 <- c(df[1,3],df[2,3],df[3,3])
  x6 <- c(df[2,1],df[2,2],df[2,3])
  x7 <- c(df[3,1],df[3,2],df[3,3])
  x8 <- c(df[3,1],df[2,2],df[1,3])
  yy <- c("X","X","X")
  y <- c(FALSE,FALSE,FALSE)
  
  if (all(match(x1,yy,nomatch = FALSE))|| 
      all(match(x2,yy,nomatch = FALSE))||
      all(match(x3,yy,nomatch = FALSE))|| 
      all(match(x4,yy,nomatch = FALSE))|| 
      all(match(x5,yy,nomatch = FALSE))||
      all(match(x6,yy,nomatch = FALSE))||
      all(match(x7,yy,nomatch = FALSE))||
      all(match(x8,yy,nomatch = FALSE))){
    cat(symbol, "win
")
    break
  }else{
    #Check for Stalemate
    all<- unlist(df)
    
    alltile<- logical()
    
    for (tile in all){
      result<-all(match(tile,symbol,nomatch = FALSE))||all(match(tile,symbol2,nomatch = FALSE))
      
      alltile<-c(alltile,result)
      
    } 
    
    if (all(alltile==TRUE)){
      cat("Stalemate")
      break
    }else{
      
      
      
      
      cat("Next round
")
      
      Sys.sleep(2)
      cat("Player", symbol2, "turn
")
      Sys.sleep(2)
      #finishround for O
      ramdom1 <-sample(c(1:3),1)
      random2 <-sample(c(1:3),1)
      
      if (is.na(df[as.numeric(ramdom1),as.numeric(random2)])){ 
        df[[as.numeric(ramdom1),as.numeric(random2)]]<-symbol2
      }else{while (match(df[[as.numeric(ramdom1),as.numeric(random2)]],symbol,nomatch = FALSE) || 
                   match(df[[as.numeric(ramdom1),as.numeric(random2)]],symbol2,nomatch = FALSE)){
        ramdom1 <-sample(c(1:3),1)
        random2 <-sample(c(1:3),1)
      }}
      cat("Computer chose",ramdom1,random2,"
")
      
      df[[as.numeric(ramdom1),as.numeric(random2)]]<-symbol2
      cat("Computer turn complete
")
      print(df)
      
      
      
      #check for win 
      x1 <- c(df[1,1],df[1,2],df[1,3])
      x2 <- c(df[1,1],df[2,1],df[3,1])
      x3 <- c(df[1,1],df[2,2],df[3,3])
      x4 <- c(df[1,2],df[2,2],df[3,2])
      x5 <- c(df[1,3],df[2,3],df[3,3])
      x6 <- c(df[2,1],df[2,2],df[2,3])
      x7 <- c(df[3,1],df[3,2],df[3,3])
      x8 <- c(df[3,1],df[2,2],df[1,3])
      yy <- c("O","O","O")
      
      if (all(match(x1,yy,nomatch = FALSE))|| 
          all(match(x2,yy,nomatch = FALSE))||
          all(match(x3,yy,nomatch = FALSE))|| 
          all(match(x4,yy,nomatch = FALSE))|| 
          all(match(x5,yy,nomatch = FALSE))||
          all(match(x6,yy,nomatch = FALSE))||
          all(match(x7,yy,nomatch = FALSE))||
          all(match(x8,yy,nomatch = FALSE))){
        cat(symbol2, "win
")
        break
      }else{
        #Check for Stalemate
        all<- unlist(df)
        
        alltile<- logical()
        
        for (tile in all){
          result<-all(match(tile,symbol,nomatch = FALSE))||all(match(tile,symbol2,nomatch = FALSE))
          
          alltile<-c(alltile,result)
          
        } 
        alltile
        
        if (all(alltile==TRUE)){
          cat("Stalemate")
          break
        }else{next
          
          
          
          
          cat("Next round
")
          
          Sys.sleep(2)}
        
      }}}}} else{
##############if player chose O####################
        for (var in numberofrounds){
          cat(paste("Round#",var," 
"))
          Sys.sleep(2)
          
          cat("Current board:
")
          Sys.sleep(2)
          print(df)
          cat("Player", symbol2, "turn","
")
          Sys.sleep(2)  
  #Computer input
          ramdom1 <-sample(c(1:3),1)
          random2 <-sample(c(1:3),1)
          
          if (is.na(df[as.numeric(ramdom1),as.numeric(random2)])){ 
            df[[as.numeric(ramdom1),as.numeric(random2)]]<-symbol2
          }else{while (match(df[[as.numeric(ramdom1),as.numeric(random2)]],symbol,nomatch = FALSE) || 
                       match(df[[as.numeric(ramdom1),as.numeric(random2)]],symbol2,nomatch = FALSE)){
            ramdom1 <-sample(c(1:3),1)
            random2 <-sample(c(1:3),1)
          }}
          cat("Computer chose",ramdom1,random2,"
")
          
          df[[as.numeric(ramdom1),as.numeric(random2)]]<-symbol2
          cat("Computer turn complete
")
          print(df)
          
          
          
          #check for win 
          x1 <- c(df[1,1],df[1,2],df[1,3])
          x2 <- c(df[1,1],df[2,1],df[3,1])
          x3 <- c(df[1,1],df[2,2],df[3,3])
          x4 <- c(df[1,2],df[2,2],df[3,2])
          x5 <- c(df[1,3],df[2,3],df[3,3])
          x6 <- c(df[2,1],df[2,2],df[2,3])
          x7 <- c(df[3,1],df[3,2],df[3,3])
          x8 <- c(df[3,1],df[2,2],df[1,3])
          yy <- c("X","X","X")
          
          if (all(match(x1,yy,nomatch = FALSE))|| 
              all(match(x2,yy,nomatch = FALSE))||
              all(match(x3,yy,nomatch = FALSE))|| 
              all(match(x4,yy,nomatch = FALSE))|| 
              all(match(x5,yy,nomatch = FALSE))||
              all(match(x6,yy,nomatch = FALSE))||
              all(match(x7,yy,nomatch = FALSE))||
              all(match(x8,yy,nomatch = FALSE))){
            cat(symbol2, "win
")
            break
          }else{
            #Check for Stalemate
            all<- unlist(df)
            
            alltile<- logical()
            
            for (tile in all){
              result<-all(match(tile,symbol,nomatch = FALSE))||all(match(tile,symbol2,nomatch = FALSE))
              
              alltile<-c(alltile,result)
              
            } 
            alltile
            
            if (all(alltile==TRUE)){
              cat("Stalemate")
              break
            }else{
              
              
              
              
              cat("Next round
")
              
              Sys.sleep(2)} 
  
            Xinputcomponent1()
            
            df[[as.numeric(place1X),as.numeric(place2X)]] <- symbol
            
            print(df)
            
            
            x1 <- c(df[1,1],df[1,2],df[1,3])
            x2 <- c(df[1,1],df[2,1],df[3,1])
            x3 <- c(df[1,1],df[2,2],df[3,3])
            x4 <- c(df[1,2],df[2,2],df[3,2])
            x5 <- c(df[1,3],df[2,3],df[3,3])
            x6 <- c(df[2,1],df[2,2],df[2,3])
            x7 <- c(df[3,1],df[3,2],df[3,3])
            x8 <- c(df[3,1],df[2,2],df[1,3])
            yy <- c("o","O","O")
            y <- c(FALSE,FALSE,FALSE)
            
            if (all(match(x1,yy,nomatch = FALSE))|| 
                all(match(x2,yy,nomatch = FALSE))||
                all(match(x3,yy,nomatch = FALSE))|| 
                all(match(x4,yy,nomatch = FALSE))|| 
                all(match(x5,yy,nomatch = FALSE))||
                all(match(x6,yy,nomatch = FALSE))||
                all(match(x7,yy,nomatch = FALSE))||
                all(match(x8,yy,nomatch = FALSE))){
              cat(symbol, "win
")
              break
            }else{
              #Check for Stalemate
              all<- unlist(df)
              
              alltile<- logical()
              
              for (tile in all){
                result<-all(match(tile,symbol,nomatch = FALSE))||all(match(tile,symbol2,nomatch = FALSE))
                
                alltile<-c(alltile,result)
                
              } 
              
              if (all(alltile==TRUE)){
                cat("Stalemate")
                break
              }else{
                
                
                
                
                cat("Next round
")
                
                Sys.sleep(2)
                cat("Player", symbol2, "turn
")
                Sys.sleep(2)
  
  
}}}}}
