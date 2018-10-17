globalSinglePlayer <- TRUE

# position 1 is single player games, position 2 is two player games
games <- c(0,0)

# position 1 is for player 1, position 2 for player 2
hits <- c(0,0)
failures <- c(0,0)

# Menu of the game
menu <- function() 
{
  # Show menu options:
  writeLines("Welcome to the Battleship game! Enter the number of an option you want to execute:
  1) Single player game, reading the initial position of the fleet of an external file
  2) Two player game, each user chooses the positions of his fleet by keyboard, which will be against which the other user will play
  3) Statistics
  4) Exit game")
  
  # Ask for user input and validate it. Keep asking for input until it is valid.
  selection <- userInput()
  while ((selection != 1 && selection != 2 && selection != 3 && selection != 4) || !validarSiNro(selection)) 
  {
    print("Please enter one of the following numbers: 1,2,3 o 4")
    selection <- userInput()
  }
  
  # For user input we are calling a function "userInput()" which works with readLine(). This last function returns the user's input as a string and that's why we now convert it to integer (note that validarSiNro(selection) has already checked the user entered a number)
  selection <- as.integer(selection)
  
  # Once the input is validated, now the game will continue until the input is detected to be equals 4 ("Abandonar juego")
  while (selection != 4) 
  {
    if (selection == 1) 
    {
      # Option 1 detected. Call to function:
      option1()
    }
    else if (selection == 2) 
    {
      # Option 2 detected. Call to function:
      option2()
    }
    else if (selection == 3) 
    {
      # Option 3 detected. Call to function:
      option3()
    }
    
    # The following will appear after option 1, 2 or 3 have finished running. That is, users are directed back to the menu.
    writeLines("Enter the number of an option you want to execute:
               1) Single player game, reading the initial position of the fleet of an external file
               2) Two player game, each user chooses the positions of his fleet by keyboard, which will be against which the other user will play
               3) Statistics
               4) Exit game")
    
    # Ask for user input and validate it. Keep ask for input until it is valid.
    selection <- userInput()
    while ((selection != 1 && selection != 2 && selection != 3 && selection != 4) || !validarSiNro(selection)) 
    {
      print("Please enter one of the following numbers: 1,2,3 o 4")
      selection <- userInput()
    }
    selection <- as.integer(selection)
  }
}

# Every time we request an input from a user this function is called. It returns the input in string format.
userInput <- function() 
{
  nro <- readline()
  return(nro)
}

# This function receives a string and validates if it can be converted to a number (otherwishe it will come up with an "na"). Returns TRUE if valid and FALSE if invalid
validarSiNro <- function(nro) 
{
  valido <- TRUE
  nro <- suppressWarnings(as.numeric(nro))
  if (is.na(nro)) 
  {
    valido <- FALSE
  }
  return(valido)
}


# This function receives a valid boat input and places it on the board. Returns the updated board.
actualizarTablero <- function(posicion, tablero) 
{
  if (nchar(posicion) == 4) 
  {
    char1 <- as.integer(substr(posicion, 1, 1))
    char2 <- substr(posicion, 2, 2)
    char3 <- substr(posicion, 3, 3)
    char4 <- as.integer(substr(posicion, 4, 4))
  }
  else if (nchar(posicion) == 5) 
  {
    char1 <- as.integer(substr(posicion, 1, 2))
    char2 <- substr(posicion, 3, 3)
    char3 <- substr(posicion, 4, 4)
    char4 <- as.integer(substr(posicion, 5, 5))
  }
  columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  if (char3 == "H") # HORIZONTAL
  {
    startPos <- 1
    while (char2 != columnas[startPos]) 
    {
      startPos <- startPos + 1
    }
    for (i in startPos:(startPos + char4 - 1))
    {
      tablero[char1, i] <- "x"
    }
  }
  else # VERTICAL
  {
    startPos <- 1
    while (char2 != columnas[startPos]) 
    {
      startPos <- startPos + 1
    }
    for (i in char1:(char1 + char4 - 1))
    {
      tablero[i, startPos] <- "x"
    }
  }
  return(tablero)
}

# Returns the updated array "flota" which contains the number of boats of each size that have already been added.
actualizarFlota <- function(input, flota) 
{
  char4 <- as.integer(substr(input, nchar(input), nchar(input)))
  if (char4 == 5) 
  {
    flota[1] <- flota[1] + 1
  }
  else if (char4 == 4) 
  {
    flota[2] <- flota[2] + 1
  }
  else if (char4 == 3) 
  {
    flota[3] <- flota[3] + 1
  }
  else if (char4 == 2) 
  {
    flota[4] <- flota[4] + 1
  }
  else 
  {
    flota[5] <- flota[5] + 1
  }
  return(flota)
}

# Prints the board it receives as parameter. Since we keep coordinates as numbers for the back-end of the game, in this function column names are turnes into letters for the player to see.
mostrarTablero <- function(tablero) 
{
  colnames(tablero) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  rownames(tablero) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  print(tablero)
}


# Checks if the input of a shot has the proper format
validateShot <- function(input) 
{
  valid <- TRUE
  if (nchar(input) == 3) 
  {
    char1 <- substr(input, 1, 2)
    char2 <- substr(input, 3, 3)
  }
  else if (nchar(input) == 2) 
  {
    char1 <- substr(input, 1, 1)
    char2 <- substr(input, 2, 2)
  }
  else if (nchar(input) == 1) 
  {
    if (input != "R") 
    {
      valid <- FALSE
    }
  }
  else 
  {
    valid <- FALSE
  }
  if (valid && input != "R") 
  {
    if (!validarSiNro(char1)) 
    {
      valid <- FALSE
    }
    else if (char2 != "A" && char2 != "B" && char2 != "C" && char2 != "D" && char2 != "E" && char2 != "F" && char2 != "G" && char2 != "H" && char2 != "I" && char2 != "J") 
    {
      valid <- FALSE
    }
  }
  return(valid)
}

# Receives the coordinate of a shot
# Returns c(0,0) if water ; c(11,11) if already H ; c(x,y) if x touched
shot <- function(input, table) 
{
  result <- c(0, 0)
  if (nchar(input) == 3) 
  {
    x <- as.integer(substr(input, 1, 2))
    y_char <- substr(input, 3, 3)
  }
  else if (nchar(input) == 2) 
  {
    x <- as.integer(substr(input, 1, 1))
    y_char <- substr(input, 2, 2)
  }
  y <- 1
  letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  while (letters[y] != y_char) 
  {
    y <- y + 1
  }
  if (table[x, y] == "x") 
  {
    result <- c(x, y)
  }
  else if (table[x, y] == "H" || table[x, y] == "*") 
  {
    result <- c(11, 11)
  }
  return(result)
}

# Function for player 1. While he/she does not enter a coordinate that is water, the player will continue shooting.
# Receives both boards and, when input coordinate is water, calls function "player2" and sends both boards.
# If a boat is touched or sunk, the board will be updated
plays1 <- function(tablero1, tablero2) 
{
  gameStatusOn <- TRUE
  while (gameStatusOn) 
  {
    touched <- FALSE
    print("Player 1, please enter shooting coordinate")
    input <- userInput()
    while (!validateShot(input)) 
    {
      print("Make sure the format is valid and try again")
      input <- userInput()
    }
    if (input == "R") 
    {
      gameStatusOn <- FALSE
    }
    else 
    {
      outcome <- shot(input, tablero2)
      if (outcome[1] > 0 && outcome[1] < 11) 
      {
        hits[1] <<- hits[1] + 1
        tablero2[outcome[1], outcome[2]] <- "H"
        touched <- TRUE
      }
      else if (outcome[1] == 0) 
      {
        gameStatusOn <- FALSE
        failures[1] <<- failures[1] + 1
        print("AGUA")
      }
    }
    if (touched) 
    {
      tablero2 <- detectSunkBoats(tablero2)
    }
    mostrarTablero(tablero2)
  }
  if (input != "R") 
  {
    plays2(tablero1, tablero2)
  }
}

# Does what "player1" function does, but for player 2
plays2 <- function(tablero1, tablero2) 
{
  gameStatusOn <- TRUE
  while (gameStatusOn) 
  {
    touched <- FALSE
    print("Player 2, please enter shooting coordinate")
    input <- userInput()
    while (!validateShot(input)) {
      print("Make sure the format is valid and try again")
      input <- userInput()
    }
    if (input == "R") 
    {
      gameStatusOn <- FALSE
    }
    else 
    {
      outcome <- shot(input, tablero1)
      if (outcome[1] > 0 && outcome[1] < 11) 
      {
        tablero1[outcome[1], outcome[2]] <- "H"
        hits[2] <<- hits[2] + 1
        touched <- TRUE
      }
      else if (outcome[1] == 0) 
      {
        gameStatusOn <- FALSE
        failures[2] <<- failures[2] + 1
        print("AGUA")
      }
    }
    if (touched) 
    {
      tablero1 <- detectSunkBoats(tablero1)
    }
    mostrarTablero(tablero1)
  }
  if (input != "R") 
  {
    plays1(tablero1, tablero2)
  }
}

# Single player starts shooting
singlePlayer <- function(tablero) 
{
  gameStatusOn <- TRUE
  while (gameStatusOn) 
  {
    touched <- FALSE
    print("Jugador, introduzca coordenada de disparo:")
    input <- userInput()
    while (!validateShot(input)) 
    {
      print("Asegurese de que el formato sea correcto e intente nuevamente:")
      input <- userInput()
    }
    if (input == "R") 
    {
      gameStatusOn <- FALSE
    }
    else 
    {
      outcome <- shot(input, tablero)
      if (outcome[1] > 0 && outcome[1] < 11) 
      {
        hits[1] <<- hits[1] + 1
        tablero[outcome[1], outcome[2]] <- "H"
        touched <- TRUE
      }
      else if (outcome[1] == 0) 
      {
        failures[1] <<- failures[1] + 1
        print("AGUA")
      }
    }
    if (touched) 
    {
      tablero <- detectSunkBoats(tablero)
    }
    mostrarTablero(tablero)
    if(gameFinished(tablero))
    {
      cat("\nYou have sunk all boats. Congratulations!\n\n")
      gameStatusOn = FALSE
    }
  }
}

# Goes though the entire matrix looking for boats that are sunk but still marked as "H" and changes it to the "*" sign.
detectSunkBoats <- function(table) 
{
  for (x in 1:10)
  {
    for (y in 1:10)
    {
      verticalCt <- 0
      horizontalCt <- 0
      if (table[x, y] == "H") 
      {
        valid <- TRUE
        isVertical <- TRUE
        if (y > 1 && y < 10) 
        {
          if (table[x, (y - 1)] != "b" || table[x, (y + 1)] != "b") 
          {
            isVertical <- FALSE
          }
        }
        else if (y > 1) 
        {
          if (table[x, (y - 1)] != "b") 
          {
            isVertical <- FALSE
          }
        }
        else {
          if (table[x, (y + 1)] != "b") 
          {
            isVertical <- FALSE
          }
        }
        
        if (isVertical) 
        {
          ### VERTICAL ###
          
          allH <- TRUE
          
          # check upward
          i <- 0
          status <- TRUE
          while (status & i <= 5) 
          {
            if ((x - i) >= 1) 
            {
              if (table[(x - i), y] == "b") 
              {
                status <- FALSE
              }
              else if (table[(x - i), y] == "x") 
              {
                status <- FALSE
                allH <- FALSE
              }
            }
            i <- i + 1
          }
          
          # check downward
          i <- 0
          status <- TRUE
          while (status & i <= 5) 
          {
            if ((x + i) <= 10)
            {
              if (table[(x + i), y] == "b") 
              {
                status <- FALSE
              }
              else if (table[(x + i), y] == "x") 
              {
                status <- FALSE
                allH <- FALSE
              }
            }
            i <- i + 1
          }
          
          if (allH) 
          {
            table[x, y] <- "*"
          }
        }
        else 
        {
          ### HORIZONTAL ###
          
          allH <- TRUE
          
          # check left
          i <- 0
          status <- TRUE
          while (status & i <= 5) 
          {
            if ((y - i) >= 1) 
            {
              if (table[x, (y - i)] == "b") 
              {
                status <- FALSE
              }
              else if (table[x, (y - i)] == "x") 
              {
                status <- FALSE
                allH <- FALSE
              }
            }
            i <- i + 1
          }
          
          # check downward
          i <- 0
          status <- TRUE
          while (status & i <= 5) 
          {
            if ((y + i) <= 10) 
            {
              if (table[x, (y + i)] == "b") 
              {
                status <- FALSE
              }
              else if (table[x, (y + i)] == "x") 
              {
                status <- FALSE
                allH <- FALSE
              }
            }
            i <- i + 1
          }
          
          if (allH) {
            table[x, y] <- "*"
          }
        }
      }
    }
  }
  return(table)
}

validateEntireTable <- function(table)
{
  
  valid = TRUE
  
  for (x in 1:5)
  {
    for (y in 1:5)
    {
      if (table[x,y] == "x" && table[(x+1),y] == "x"  && table[(x+2),y] == "x"  && table[(x+3),y] == "x"  && table[(x+4),y] == "x" && table[(x+5),y] == "x")
      {
        valid = FALSE 
      }
      else if (table[x,y] == "x" && table[x,(y+1)] == "x" && table[x,(y+2)] == "x" && table[x,(y+3)] == "x" && table[x,(y+4)] == "x" && table[x,(y+5)] == "x")
      {
        valid = FALSE
      }
    }
  }
  
  if (valid)
  {
    table = cbind(table,c("b","b","b","b","b","b","b","b","b","b"))
    table = cbind(c("b","b","b","b","b","b","b","b","b","b"),table)
    table = rbind(table,c("b","b","b","b","b","b","b","b","b","b","b","b"))
    table = rbind(c("b","b","b","b","b","b","b","b","b","b","b","b"),table)
    rownames(table) = 1:12
    colnames(table) = 1:12
    
    for (x in 2:11)
    {
      for (y in 2:11)
      {
        if (table[x,y] == "x" && table[(x+1),y] == "x" && table[x,(y+1)] == "x")
        {
          valid = FALSE
        }
        else if (table[x,y] == "x" && table[(x-1),y] == "x" && table[x,(y+1)] == "x")
        {
          valid = FALSE 
        }
        else if (table[x,y] == "x" && table[(x+1),y] == "x" && table[x,(y-1)] == "x")
        {
          valid = FALSE
        }
        else if (table[x,y] == "x" && table[(x-1),y] == "x" && table[x,(y-1)] == "x")
        {
          valid = FALSE
        }
      }
    }  
  }
  return(valid)
}

countFlota <- function(table)
{
  table = cbind(table,c("b","b","b","b","b","b","b","b","b","b"))
  table = cbind(c("b","b","b","b","b","b","b","b","b","b"),table)
  table = rbind(table,c("b","b","b","b","b","b","b","b","b","b","b","b"))
  table = rbind(c("b","b","b","b","b","b","b","b","b","b","b","b"),table)
  rownames(table) = 1:12
  colnames(table) = 1:12
  
  flota = c(0,0,0,0,0)
  
  for (x in 2:11)
  {
    for (y in 2:11)
    {
      if (table[x,y] == "x")
      {
        ct = 1
        if (table[(x-1),y] == "b" && table[(x+1),y] == "b")
        {
          #HORIZONTAL
          i = 1
          status = TRUE
          while ((y+i) <= 12 && status)
          {
            if (table[x,(y+i)] == "b")
            {
              status = FALSE
            }
            else
            {
              i = i + 1
            }
          }
          ct = ct + (i-1)
          
          i = 1
          status = TRUE
          while ((y-i) >= 0 && status)
          {
            if (table[x,(y-i)] == "b")
            {
              status = FALSE
            }
            else
            {
              i = i + 1
            }
          }
          ct = ct + (i-1)
        }
        else
        {
          #VERTICAL
          i = 1
          status = TRUE
          while ((x+i) <= 12 && status)
          {
            if (table[(x+i),y] == "b")
            {
              status = FALSE
            }
            else
            {
              i = i + 1
            }
          }
          ct = ct + (i-1)
          
          i = 1
          status = TRUE
          while ((x-i) >= 0 && status)
          {
            if (table[(x-i),y] == "b")
            {
              status = FALSE
            }
            else
            {
              i = i + 1
            }
          }
          ct = ct + (i-1)
        }
        if (ct == 5)
        {
          flota[1] = flota[1] + 1
        }
        else if (ct == 4)
        {
          flota[2] = flota[2] + 1
        }
        else if (ct == 3)
        {
          flota[3] = flota[3] + 1
        }
        else if (ct == 2)
        {
          flota[4] = flota[4] + 1
        }
        else if (ct == 1)
        {
          flota[5] = flota[5] + 1
        }
        else
        {
          flota[1] = 999
        }
      }
    }
  }
  flota[1] = flota[1] / 5
  flota[2] = flota[2] / 4
  flota[3] = flota[3] / 3
  flota[4] = flota[4] / 2
  flota[5] = flota[5] / 1
  if (flota[1] != 1 || flota[2] != 2 || flota[3] != 2 || flota[4] != 3 || flota[5] != 4)
  {
    flota[1] = 999
  }
  return(flota)
}

gameFinished <- function(table)
{
  valid = TRUE
  for (x in 1:10)
  {
    for (y in 1:10)
    {
      if (table[x,y] == "H" || table[x,y] == "x")
      {
        valid = FALSE
      }
    }
  }
  return(valid)
}

getPosition <- function(input)
{
  valid = TRUE
  #SEPARATES INPUT INTO ELEMENTS
  if (nchar(input) == 4)
  {
    char1 <- as.integer(substr(input, 1, 1))
    char2 <- substr(input, 2, 2)
    char3 <- substr(input, 3, 3)
    char4 <- as.integer(substr(input, 4, 4))
  }
  else if (nchar(input) == 5)
  {
    char1 <- as.integer(substr(input, 1, 2))
    char2 <- substr(input, 3, 3)
    char3 <- substr(input, 4, 4)
    char4 <- as.integer(substr(input, 5, 5))
  }
  else
  {
    valid = FALSE
  }
  
  if (valid)
  {
    if (!is.na(char1) && !is.na(char2) && !is.na(char3) && !is.na(char4))
    {
      if (char1 >= 1 && char1 <= 10)
      {
        #VALID ROW
        columns = c("A","B","C","D","E","F","G","H","I","J") 
        letter = 1
        found = FALSE
        while (letter <= 10 && !found)
        {
          if (char2 == columns[letter])
          {
            found = TRUE
          }
          else
          {
            letter = letter + 1
          }
        }
        if (letter <= 10)
        {
          #VALID ROW AND VALID COLUMN
          if (char3 == "H" || char3 == "V")
          {
            #VALID ROW, COLUMN AND DIRECTION
            if (char4 < 1 || char4 > 5)
            {
              valid = FALSE
            }
          }
          else
          {
            valid = FALSE
          }
        }
        else
        {
          valid = FALSE
        }
      }
      else
      {
        valid = FALSE
      }
    }
    else
    {
      valid = FALSE
    }
  }
  
  if (!valid)
  {
    result = c(0,0,0,0)
  }
  else
  {
    result = c(char1,letter,char3,char4)
  }
  return(result)
}

noOverlap <- function (table,position)
{
  valid = TRUE
  
  table = cbind(table,c("b","b","b","b","b","b","b","b","b","b"))
  table = cbind(c("b","b","b","b","b","b","b","b","b","b"),table)
  table = rbind(table,c("b","b","b","b","b","b","b","b","b","b","b","b"))
  table = rbind(c("b","b","b","b","b","b","b","b","b","b","b","b"),table)
  rownames(table) = 1:12
  colnames(table) = 1:12
  
  if (position[3] == "H")
  {
    x = (as.integer(position[1])+1)
    for (y in (as.integer(position[2])+1):(as.integer(position[2])+as.integer(position[4])))
    {
      if (table[x,y] != "b" || table[(x+1),y] != "b" || table[(x-1),y] != "b")
      {
        valid = FALSE
      }
    }
    if (valid)
    {
      y = (as.integer(position[2])+1)
      length = as.integer(position[4])
      if (table[x,(y-1)] != "b" || table[x,(y+length)] != "b")
      {
        valid = FALSE
      }
    }
  }
  else
  {
    y = (as.integer(position[2])+1)
    for (x in (as.integer(position[1])+1):(as.integer(position[1])+as.integer(position[4])))
    {
      if (table[x,y] != "b" || table[x,(y+1)] != "b" || table[x,(y-1)] != "b")
      {
        valid = FALSE
      }
    }
    if (valid)
    {
      x = (as.integer(position[1])+1)
      length = as.integer(position[4])
      if (table[(x-1),y] != "b" || table[(x+length),y] != "b")
      {
        valid = FALSE
      }
    }
  }
  return(valid)
}

option1 <- function() {
  if ("readxl" %in% rownames(installed.packages()) == FALSE) 
  {
    install.packages("readxl")
  }
  if ("readr" %in% rownames(installed.packages()) == FALSE) 
  {
    install.packages("readr")
  }
  library(readxl)
  library(readr)
  file = read_excel(file.choose(), skip=1,col_names=c("A","B","C","D","E","F","G","H","I","J"))
  fBoat <- names(file)
  file <- unlist(file, use.name = FALSE)
  file <- c(fBoat, file)
  
  valid = TRUE
  l = 11
  if (length(file) == 110)
  {
    tablero <- matrix(c(1:100), nrow = 10, ncol = 10)
    for (i in 1:10)
    {
      for (j in 1:10)
      {
        tablero[j, i] <- file[l]
        l = l + 1
      }
    }
  }
  else
  {
    valid = FALSE
  }
  
  if (!validateEntireTable(tablero))
  {
    valid = FALSE
    print("ERROR: Boats intersect each other. You will be redirected to the menu...")
  }
  else
  {
    flota = countFlota(tablero)
    if (flota[1] > 1)
    {
      valid = FALSE
      print("ERROR: Boat sizes do not fit requirements. You will be redirected to the menu...")
    }
  }
  
  if (valid) {
    # SINGLE PLAYER
    print("Board successfully loaded.")
    mostrarTablero(tablero)
    games[1] <<- games[1] + 1
    globalSinglePlayer <<- TRUE
    hits[1] <<- 0
    failures[1] <<- 0
    singlePlayer(tablero)
  }
}

option2 <- function() {
  
  continueInOption2 = TRUE
  
  tablero1 <- matrix(c(1:100), nrow = 10, ncol = 10)
  tablero2 <- matrix(c(1:100), nrow = 10, ncol = 10)
  for (i in 1:10)
  {
    for (j in 1:10)
    {
      tablero1[i, j] <- "b"
      tablero2[i, j] <- "b"
    }
  }
  
  # Los vectores "flota1" y "flota2": 1 barco de 5 casillas, 2 barcos de 4 casillas, 2 barcos de 3 casillas, 3 barcos de 2 casillas, 4 barcos de 1 casilla -> comineza c(0,0,0,0,0) y luego va contando cu??ntos barcos se ingresaron
  flota1 <- c(0, 0, 0, 0, 0)
  flota2 <- c(0, 0, 0, 0, 0)
  
  mostrarTablero(tablero1)
  
  ## PLAYER 1
  while ((flota1[1] < 1 || flota1[2] < 2 || flota1[3] < 2 || flota1[4] < 3 || flota1[5] < 4) && continueInOption2) 
  {
    cat("\nYou have the following boats available:",
        "\n5 squares: ", (1 - flota1[1]), 
        "\n4 squares: ", (2 - flota1[2]), 
        "\n3 squares: ", (2 - flota1[3]), 
        "\n2 squares: ", (3 - flota1[4]), 
        "\n1 square:  ", (4 - flota1[5]), "\n\n")
    
    print("Player 1: please enter the position of a ship (eg. 1AV3):")
    
    input <- userInput()
    
    validInput = TRUE
    position = getPosition(input)
    if (position[1] == 0)
    {
      validInput = FALSE
      if (input != "R")
      {
        print("Make sure the format of the input is correct and try again:") 
      }
    }
    else if ((position[4] == 5 && flota1[1] == 1) || (position[4] == 4 && flota1[2] == 2) || (position[4] == 3 && flota1[3] == 2) || (position[4] == 2 && flota1[4] == 3) || (position[4] == 1 && flota1[5] == 4))
    {
      validInput = FALSE
      print("Boat size is not available. Please enter an available boat size:")
    }
    else
    {
      varValid = TRUE
      if (position[3] == "H")
      {
        if ((as.integer(position[2])+as.integer(position[4])) > 11)
        {
          varValid = FALSE
          validInput = FALSE
          print("The boat you entered does not fit in the board. Please enter a new one:")
        }
      }
      else if (position[3] == "V")
      {
        if ((as.integer(position[1])+as.integer(position[4])) > 11)
        {
          varValid = FALSE
          validInput = FALSE
          print("The boat you entered does not fit in the board. Please enter a new one:")
        }
      }
      
      if (varValid)
      {
        if (!noOverlap(tablero1,position))
        {
          validInput = FALSE
          print("Oops, the boats are not allowed to touch or overlap! Please choose another position:")
        }
      }
    }
    while (!validInput && input != "R")
    {
      input <- userInput()
      
      validInput = TRUE
      position = getPosition(input)
      if (position[1] == 0)
      {
        validInput = FALSE
        if (input != "R")
        {
          print("Make sure the format of the input is correct and try again:") 
        }
      }
      else if ((position[4] == 5 && flota1[1] == 1) || (position[4] == 4 && flota1[2] == 2) || (position[4] == 3 && flota1[3] == 2) || (position[4] == 2 && flota1[4] == 3) || (position[4] == 1 && flota1[5] == 4))
      {
        validInput = FALSE
        print("Boat size is not available. Please enter an available boat size:")
      }
      else
      {
        varValid = TRUE
        if (position[3] == "H")
        {
          if ((as.integer(position[2])+as.integer(position[4])) > 11)
          {
            varValid = FALSE
            validInput = FALSE
            print("The boat you entered does not fit in the board. Please enter a new one:")
          }
        }
        else if (position[3] == "V")
        {
          if ((as.integer(position[1])+as.integer(position[4])) > 11)
          {
            varValid = FALSE
            validInput = FALSE
            print("The boat you entered does not fit in the board. Please enter a new one:")
          }
        }
        
        if (varValid)
        {
          if (!noOverlap(tablero1,position))
          {
            validInput = FALSE
            print("Oops, the boats are not allowed to touch or overlap! Please choose another position:")
          }
        }
      }
    }
    
    if (input != "R")
    {
      tablero1 <- actualizarTablero(input, tablero1)
      flota1 <- actualizarFlota(input, flota1)
      mostrarTablero(tablero1)
    }
    else
    {
      continueInOption2 = FALSE
    }
  }
  
  if (input != "R")
  {
    ## PLAYER 2
    while ((flota2[1] < 1 || flota2[2] < 2 || flota2[3] < 2 || flota2[4] < 3 || flota2[5] < 4) && continueInOption2) 
    {
      cat("\nYou have the following boats available:",
          "\n5 squares: ", (1 - flota2[1]), 
          "\n4 squares: ", (2 - flota2[2]), 
          "\n3 squares: ", (2 - flota2[3]), 
          "\n2 squares: ", (3 - flota2[4]), 
          "\n1 square:  ", (4 - flota2[5]), "\n\n")
      
      print("Player 2: please enter the position of a ship (eg. 1AV3):")
      
      input <- userInput()
      
      validInput = TRUE
      position = getPosition(input)
      if (position[1] == 0)
      {
        validInput = FALSE
        if (input != "R")
        {
          print("Make sure the format of the input is correct and try again:") 
        }
      }
      else if ((position[4] == 5 && flota2[1] == 1) || (position[4] == 4 && flota2[2] == 2) || (position[4] == 3 && flota2[3] == 2) || (position[4] == 2 && flota2[4] == 3) || (position[4] == 1 && flota2[5] == 4))
      {
        validInput = FALSE
        print("Boat size is not available. Please enter an available boat size:")
      }
      else
      {
        varValid = TRUE
        if (position[3] == "H")
        {
          if ((as.integer(position[2])+as.integer(position[4])) > 11)
          {
            varValid = FALSE
            validInput = FALSE
            print("The boat you entered does not fit in the board. Please enter a new one:")
          }
        }
        else if (position[3] == "V")
        {
          if ((as.integer(position[1])+as.integer(position[4])) > 11)
          {
            varValid = FALSE
            validInput = FALSE
            print("The boat you entered does not fit in the board. Please enter a new one:")
          }
        }
        
        if (varValid)
        {
          if (!noOverlap(tablero2,position))
          {
            validInput = FALSE
            print("Oops, the boats are not allowed to touch or overlap! Please choose another position:")
          }
        }
      }
      while (!validInput && input != "R")
      {
        input <- userInput()
        
        validInput = TRUE
        position = getPosition(input)
        if (position[1] == 0)
        {
          validInput = FALSE
          if (input != "R")
          {
            print("Make sure the format of the input is correct and try again:") 
          }
        }
        else if ((position[4] == 5 && flota2[1] == 1) || (position[4] == 4 && flota2[2] == 2) || (position[4] == 3 && flota2[3] == 2) || (position[4] == 2 && flota2[4] == 3) || (position[4] == 1 && flota2[5] == 4))
        {
          validInput = FALSE
          print("Boat size is not available. Please enter an available boat size:")
        }
        else
        {
          varValid = TRUE
          if (position[3] == "H")
          {
            if ((as.integer(position[2])+as.integer(position[4])) > 11)
            {
              varValid = FALSE
              validInput = FALSE
              print("The boat you entered does not fit in the board. Please enter a new one:")
            }
          }
          else if (position[3] == "V")
          {
            if ((as.integer(position[1])+as.integer(position[4])) > 11)
            {
              varValid = FALSE
              validInput = FALSE
              print("The boat you entered does not fit in the board. Please enter a new one:")
            }
          }
          
          if (varValid)
          {
            if (!noOverlap(tablero2,position))
            {
              validInput = FALSE
              print("Oops, the boats are not allowed to touch or overlap! Please choose another position:")
            }
          }
        }
      }
      
      if (input != "R")
      {
        tablero2 <- actualizarTablero(input, tablero2)
        flota2 <- actualizarFlota(input, flota2)
        mostrarTablero(tablero2)
      }
      else
      {
        continueInOption2 = FALSE
      }
    }
  }
  
  if (input != "R")
  {
    games[2] <<- games[2] + 1
    globalSinglePlayer <<- FALSE
    hits[1] <<- 0
    hits[2] <<- 0
    failures[1] <<- 0
    failures[2] <<- 0
    plays1(tablero1, tablero2) 
  }
}

option3 <- function() 
{
  cat("Number of games played: \n-One player: ",games[1],"\nTwo players: ",games[2],"\n\n")
  if(globalSinglePlayer)
  {
    cat("Last game played: one player\n-Shots: ",(hits[1]+failures[1]),"\n-Hits: ",hits[1],"\n-Failures: ",failures[1],"\n-Hit Rate: ",((hits[1]/(hits[1]+failures[1]))*100),"%\n\n")
  }
  else
  {
    cat("Last game played: two players\n\nPlayer 1:\n-Shots: ",(hits[1]+failures[1]),"\n-Hits: ",hits[1],"\n-Failures: ",failures[1],"\n-Hit Rate: ",((hits[1]/(hits[1]+failures[1]))*100),"%\n\nPlayer 2:\n-Shots: ",(hits[2]+failures[2]),"\n-Hits: ",hits[2],"\n-Failures: ",failures[2],"\nHit Rate: ",((hits[2]/(hits[2]+failures[2]))*100),"%\n\n")
  }
}

menu() # Calls the main menu
