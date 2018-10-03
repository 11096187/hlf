# menu of the game
menu <- function() {
  # Shows menu options:
  writeLines("Ingrese el numero de la accion que desea realizar:\n1) Jugar 1 usuario, leyendo la posicion inicial de la flota de un fichero externo\n2) Jugar 2 usuarios, cada usuario elige las posiciones de su flota por teclado, que sera contra la que va a jugar el otro usuario\n3) Estadisticas\n4) Abandonar el juego")

  # Ask for user input and validates it. Will keep asking for input until it is valid.
  seleccion <- inputUsuario()
  while ((seleccion != 1 && seleccion != 2 && seleccion != 3 && seleccion != 4) || !validarSiNro(seleccion)) {
    print("Debe ingresar uno de los siguientes numeros 1,2,3 o 4")
    seleccion <- inputUsuario()
  }

  # For user input we are calling a function "inputUsuario()" which works with readLine(). This last function returns the user's input as a string and that's why we now convert it to integer (note that validarSiNro(seleccion) has already checked the user entered a number)
  seleccion <- as.integer(seleccion)

  # Once the input is validated, now the game will continue until the input is detected to be equals 4 ("Abandonar juego")
  while (seleccion != 4) {
    if (seleccion == 1) {
      # Option 1 detected. Call to function:
      opcion1()
    }
    else if (seleccion == 2) {
      # Option 2 detected. Call to function:
      opcion2()
    }
    else if (seleccion == 3) {
      # Option 3 detected. Call to function:
      opcion3()
    }

    # The following will appear after option 1, 2 or 3 have finished running. That is, users are directed back to the menu.

    writeLines("Ingrese el numero de la accion que desea realizar:\n1) Jugar 1 usuario, leyendo la posicion inicial de la flota de un fichero externo\n2) Jugar 2 usuarios, cada usuario elige las posiciones de su flota por teclado, que sera contra la que va a jugar el otro usuario\n3) Estadisticas\n4) Abandonar el juego")
    seleccion <- inputUsuario()
    while ((seleccion != 1 && seleccion != 2 && seleccion != 3 && seleccion != 4) || !validarSiNro(seleccion)) {
      print("Debe ingresar uno de los siguientes numeros 1,2,3 o 4")
      seleccion <- inputUsuario()
    }
    seleccion <- as.integer(seleccion)
  }
}

# Every time we request an input from a user this function is called. It returns the input in string format.
inputUsuario <- function() {
  nro <- readline()
  return(nro)
}

# This function receives a string and validates if it can be converted to a number (otherwishe it will come up with an "na"). Returns TRUE if valid and FALSE if invalid
validarSiNro <- function(nro) {
  valido <- TRUE
  nro <- suppressWarnings(as.numeric(nro))
  if (is.na(nro)) {
    valido <- FALSE
  }
  return(valido)
}

# This function receives a string input and returns TRUE if the input is a valid position to insert a boat. Returns FALSE otherwise.
validarPosicion <- function(posicion) {
  # Variable "valido" is initialized and set to TRUE
  valido <- TRUE

  # If possible, splits the input based on the format specified in the game instructions. char1 will indicate the starting row, char2 the starting column, char3 whether it's horizontal or vertical and char4 the length of the boat
  if (nchar(posicion) == 4) {
    char1 <- substr(posicion, 1, 1)
    char2 <- substr(posicion, 2, 2)
    char3 <- substr(posicion, 3, 3)
    char4 <- substr(posicion, 4, 4)
  }
  else if (nchar(posicion) == 5) {
    char1 <- substr(posicion, 1, 2)
    char2 <- substr(posicion, 3, 3)
    char3 <- substr(posicion, 4, 4)
    char4 <- substr(posicion, 5, 5)
  }
  else {
    valido <- FALSE
  }

  if (valido) {
    if (!validarSiNro(char1) || !validarSiNro(char4)) # char1 and char4 must be numbers for the format to be correct
    {
      valido <- FALSE
    }
    else {
      if (as.integer(char1) < 1 || as.integer(char1) > 10 || as.integer(char4) < 1 || as.integer(char4) > 5) # Min row is 1 and max is 10. Min length is 1 and max is 5.
      {
        valido <- FALSE
      }
      else {
        if ((char2 != "A" && char2 != "B" && char2 != "C" && char2 != "D" && char2 != "E" && char2 != "F" && char2 != "G" && char2 != "H" && char2 != "I" && char2 != "J") || (char3 != "H" && char3 != "V")) # Column must be a letter from A to J. Direction must be either H or V.
        {
          valido <- FALSE
        }
      }
    }
  }
  return(valido)
}

# This function checks if there is no overlap between boats and if the coordinate following end of the boat is equal to "b" (as long as the boat does not reach the edge of the board).
# Returns TRUE is valid and FALSE otherwise
posicionDisponible <- function(posicion, tablero) {
  valido <- TRUE
  if (validarPosicion(posicion)) {
    if (nchar(posicion) == 4) {
      char1 <- as.integer(substr(posicion, 1, 1))
      char2 <- substr(posicion, 2, 2)
      char3 <- substr(posicion, 3, 3)
      char4 <- as.integer(substr(posicion, 4, 4))
    }
    else if (nchar(posicion) == 5) {
      char1 <- as.integer(substr(posicion, 1, 2))
      char2 <- substr(posicion, 3, 3)
      char3 <- substr(posicion, 4, 4)
      char4 <- as.integer(substr(posicion, 5, 5))
    }
    columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
    if (char3 == "H") # HORIZONTAL
    {
      # startPos will be 1 if column is A, 2 if B, 3 if C and so on
      startPos <- 1
      while (char2 != columnas[startPos]) {
        startPos <- startPos + 1
      }
      if ((startPos + char4) <= 10) {
        for (i in startPos:(startPos + char4))
        {
          if (tablero[char1, i] != "b") {
            valido <- FALSE
          }
        }

        if (char1 == 1) {
          if (tablero[(char1 + 1), i] != "b") {
            valido <- FALSE
          }
        }
        else if (char1 == 10) {
          if (tablero[(char1 - 1), i] != "b") {
            valido <- FALSE
          }
        }
        else {
          if (tablero[(char1 + 1), i] != "b" || tablero[(char1 - 1), i] != "b") {
            valido <- FALSE
          }
        }
      }
      else if ((startPos + char4 - 1) <= 10) {
        for (i in startPos:(startPos + char4 - 1))
        {
          if (tablero[char1, i] != "b") {
            valido <- FALSE
          }

          if (char1 == 1) {
            if (tablero[(char1 + 1), i] != "b") {
              valido <- FALSE
            }
          }
          else if (char1 == 10) {
            if (tablero[(char1 - 1), i] != "b") {
              valido <- FALSE
            }
          }
          else {
            if (tablero[(char1 + 1), i] != "b" || tablero[(char1 - 1), i] != "b") {
              valido <- FALSE
            }
          }
        }
      }
      else {
        valido <- FALSE
      }
    }
    else # VERTICAL
    {
      startPos <- 1
      while (char2 != columnas[startPos]) {
        startPos <- startPos + 1
      }
      if ((char1 + char4) <= 10) {
        for (i in char1:(char1 + char4))
        {
          if (tablero[i, startPos] != "b") {
            valido <- FALSE
          }

          if (startPos == 1) {
            if (tablero[i, (startPos + 1)] != "b") {
              valido <- FALSE
            }
          }
          else if (startPos == 10) {
            if (tablero[i, (startPos - 1)] != "b") {
              valido <- FALSE
            }
          }
          else {
            if (tablero[i, (startPos + 1)] != "b" || tablero[i, (startPos - 1)] != "b") {
              valido <- FALSE
            }
          }
        }
      }
      else if ((char1 + char4 - 1) <= 10) {
        for (i in char1:(char1 + char4 - 1))
        {
          if (tablero[i, startPos] != "b") {
            valido <- FALSE
          }

          if (startPos == 1) {
            if (tablero[i, (startPos + 1)] != "b") {
              valido <- FALSE
            }
          }
          else if (startPos == 10) {
            if (tablero[i, (startPos - 1)] != "b") {
              valido <- FALSE
            }
          }
          else {
            if (tablero[i, (startPos + 1)] != "b" || tablero[i, (startPos - 1)] != "b") {
              valido <- FALSE
            }
          }
        }
      }
      else {
        valido <- FALSE
      }
    }
  }
  else {
    valido <- FALSE
  }
  return(valido)
}

# Receives a boat the user is trying to add to the board and an array with 5 elements. Firts element is the number of boats that have already been places consisting of 5 coordinates, the second element shows boats with 4 elements and so on in decreasing order.
# Max number of boats 5 coordinates long is 1 so, for instance, if the user wants to add a boat with a length of 5 and flota = c(1,...) then the size is not available and the function will return FALSE
tamanoDisponible <- function(posicion, flota) {
  valido <- TRUE
  if (validarPosicion(posicion)) {
    if (nchar(posicion) == 4) {
      char4 <- as.integer(substr(posicion, 4, 4))
    }
    else if (nchar(posicion) == 5) {
      char4 <- as.integer(substr(posicion, 5, 5))
    }
    if (char4 > 5) {
      valido <- FALSE
    }
    else if (char4 == 5) {
      if (flota[1] >= 1) {
        valido <- FALSE
      }
    }
    else if (char4 == 4) {
      if (flota[2] >= 2) {
        valido <- FALSE
      }
    }
    else if (char4 == 3) {
      if (flota[3] >= 2) {
        valido <- FALSE
      }
    }
    else if (char4 == 2) {
      if (flota[4] >= 3) {
        valido <- FALSE
      }
    }
    else {
      if (flota[4] >= 4) {
        valido <- FALSE
      }
    }
  }
  else {
    valido <- FALSE
  }
  return(valido)
}

# This function receives a valid boat input and places it on the board. Returns the updated board.
actualizarTablero <- function(posicion, tablero) {
  if (nchar(posicion) == 4) {
    char1 <- as.integer(substr(posicion, 1, 1))
    char2 <- substr(posicion, 2, 2)
    char3 <- substr(posicion, 3, 3)
    char4 <- as.integer(substr(posicion, 4, 4))
  }
  else if (nchar(posicion) == 5) {
    char1 <- as.integer(substr(posicion, 1, 2))
    char2 <- substr(posicion, 3, 3)
    char3 <- substr(posicion, 4, 4)
    char4 <- as.integer(substr(posicion, 5, 5))
  }
  columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  if (char3 == "H") # HORIZONTAL
  {
    startPos <- 1
    while (char2 != columnas[startPos]) {
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
    while (char2 != columnas[startPos]) {
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
actualizarFlota <- function(input, flota) {
  char4 <- as.integer(substr(input, nchar(input), nchar(input)))
  if (char4 == 5) {
    flota[1] <- flota[1] + 1
  }
  else if (char4 == 4) {
    flota[2] <- flota[2] + 1
  }
  else if (char4 == 3) {
    flota[3] <- flota[3] + 1
  }
  else if (char4 == 2) {
    flota[4] <- flota[4] + 1
  }
  else {
    flota[5] <- flota[5] + 1
  }
  return(flota)
}

# Prints the board it receives as parameter. Since we keep coordinates as numbers for the back-end of the game, in this function column names are turnes into letters for the player to see.
mostrarTablero <- function(tablero) {
  colnames(tablero) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  rownames(tablero) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  print(tablero)
}

# Function "posicionDisponible" already checks if spots starting from the first one of the boat do not touch another boat, but since it only does it "looking forward", we now check if the very first coordinate of the boat is also surrounded by water.
boatsDontTouch <- function(input, table) {
  valid <- TRUE
  if (validarPosicion(input) && posicionDisponible(input, table)) {
    if (nchar(input) == 4) {
      row <- as.integer(substr(input, 1, 1))
      col <- substr(input, 2, 2)
    }
    else if (nchar(input) == 5) {
      row <- as.integer(substr(input, 1, 2))
      col <- substr(input, 3, 3)
    }
    columns <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
    startPos <- 1
    while (col != columns[startPos]) {
      startPos <- startPos + 1
    }
    col <- startPos

    # row = row_number & col = col_number
    # With a series of "if" clauses we channel the specific case for the first coordinate and make the necessary validations to make sure it is surrounded by water (not touching another boat).
    if (row == 1) {
      # First row
      if (col == 1) {
        # First row, first col
        if (table[(row + 1), (col)] != "b" || table[(row), (col + 1)] != "b") {
          valid <- FALSE
        }
      }
      else if (col == 10) {
        # First row, last col
        if (table[(row + 1), (col)] != "b" || table[(row), (col - 1)] != "b") {
          valid <- FALSE
        }
      }
      else {
        # First row, middle col
        if (table[(row + 1), (col)] != "b" || table[row, (col + 1)] != "b" || table[row, (col - 1)] != "b") {
          valid <- FALSE
        }
      }
    }
    else if (row == 10) {
      # Last row
      if (col == 1) {
        # Last row, first col
        if (table[(row - 1), (col)] != "b" || table[(row), (col + 1)] != "b") {
          valid <- FALSE
        }
      }
      else if (col == 10) {
        # Last row, last col
        if (table[(row - 1), (col)] != "b" || table[(row), (col - 1)] != "b") {
          valid <- FALSE
        }
      }
      else {
        # Last row, middle col
        if (table[(row - 1), (col)] != "b" || table[row, (col + 1)] != "b" || table[row, (col - 1)] != "b") {
          valid <- FALSE
        }
      }
    }
    else {
      # Not first nor last row
      if (col == 1) {
        # First col, not first nor last row
        if (table[(row + 1), col] != "b" || table[(row - 1), col] != "b" || table[row, (col + 1)] != "b") {
          valid <- FALSE
        }
      }
      else if (col == 10) {
        # Last col, not first nor last row
        if (table[(row + 1), col] != "b" || table[(row - 1), col] != "b" || table[row, (col - 1)] != "b") {
          valid <- FALSE
        }
      }
      else {
        # Not first or last row or col
        if (table[(row + 1), col] != "b" || table[(row - 1), col] != "b" || table[row, (col - 1)] != "b" || table[row, (col + 1)] != "b") {
          valid <- FALSE
        }
      }
    }
  }
  else {
    valid <- FALSE
  }

  return(valid)
}

# Checks if the input of a shot has the proper format
validateShot <- function(input) {
  valid <- TRUE
  if (nchar(input) == 3) {
    char1 <- substr(input, 1, 2)
    char2 <- substr(input, 3, 3)
  }
  else if (nchar(input) == 2) {
    char1 <- substr(input, 1, 1)
    char2 <- substr(input, 2, 2)
  }
  else if (nchar(input) == 1) {
    if (input != "R") {
      valid <- FALSE
    }
  }
  else {
    valid <- FALSE
  }
  if (valid && input != "R") {
    if (!validarSiNro(char1)) {
      valid <- FALSE
    }
    else if (char2 != "A" && char2 != "B" && char2 != "C" && char2 != "D" && char2 != "E" && char2 != "F" && char2 != "G" && char2 != "H" && char2 != "I" && char2 != "J") {
      valid <- FALSE
    }
  }
  return(valid)
}

# Receives the coordinate of a shot
# Returns c(0,0) if water ; c(11,11) if already H ; c(x,y) if x touched
shot <- function(input, table) {
  result <- c(0, 0)
  if (nchar(input) == 3) {
    x <- as.integer(substr(input, 1, 2))
    y_char <- substr(input, 3, 3)
  }
  else if (nchar(input) == 2) {
    x <- as.integer(substr(input, 1, 1))
    y_char <- substr(input, 2, 2)
  }
  y <- 1
  letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  while (letters[y] != y_char) {
    y <- y + 1
  }
  if (table[x, y] == "x") {
    result <- c(x, y)
  }
  else if (table[x, y] == "H" || table[x, y] == "*") {
    result <- c(11, 11)
  }
  return(result)
}

# Function for player 1. While he/she does not enter a coordinate that is water, the player will continue shooting.
# Receives both boards and, when input coordinate is water, calls function "player2" and sends both boards.
# If a boat is touched or sunk, the board will be updated
plays1 <- function(tablero1, tablero2) {
  gameStatusOn <- TRUE
  while (gameStatusOn) {
    touched <- FALSE
    print("Jugador 1, introduzca coordenada de disparo:")
    input <- inputUsuario()
    while (!validateShot(input)) {
      print("Asegurese de que el formato sea correcto e intente nuevamente:")
      input <- inputUsuario()
    }
    if (input == "R") {
      gameStatusOn <- FALSE
    }
    else {
      outcome <- shot(input, tablero2)
      if (outcome[1] > 0 && outcome[1] < 11) {
        tablero2[outcome[1], outcome[2]] <- "H"
        touched <- TRUE
      }
      else if (outcome[1] == 0) {
        gameStatusOn <- FALSE
        print("AGUA")
      }
    }
    if (touched) {
      tablero2 <- detectSunkBoats(tablero2)
    }
    mostrarTablero(tablero2)
  }
  if (input != "R") {
    plays2(tablero1, tablero2)
  }
}

# Does what "player1" function does, but for player 2
plays2 <- function(tablero1, tablero2) {
  gameStatusOn <- TRUE
  while (gameStatusOn) {
    touched <- FALSE
    print("Jugador 2, introduzca coordenada de disparo:")
    input <- inputUsuario()
    while (!validateShot(input)) {
      print("Asegurese de que el formato sea correcto e intente nuevamente:")
      input <- inputUsuario()
    }
    if (input == "R") {
      gameStatusOn <- FALSE
    }
    else {
      outcome <- shot(input, tablero1)
      if (outcome[1] > 0 && outcome[1] < 11) {
        tablero1[outcome[1], outcome[2]] <- "H"
        touched <- TRUE
      }
      else if (outcome[1] == 0) {
        gameStatusOn <- FALSE
        print("AGUA")
      }
    }
    if (touched) {
      tablero1 <- detectSunkBoats(tablero1)
    }
    mostrarTablero(tablero1)
  }
  if (input != "R") {
    plays1(tablero1, tablero2)
  }
}

# Single player starts shooting
singlePlayer <- function(tablero) {
  gameStatusOn <- TRUE
  while (gameStatusOn) {
    touched <- FALSE
    print("Jugador, introduzca coordenada de disparo:")
    input <- inputUsuario()
    while (!validateShot(input)) {
      print("Asegurese de que el formato sea correcto e intente nuevamente:")
      input <- inputUsuario()
    }
    if (input == "R") {
      gameStatusOn <- FALSE
    }
    else {
      outcome <- shot(input, tablero)
      if (outcome[1] > 0 && outcome[1] < 11) {
        tablero[outcome[1], outcome[2]] <- "H"
        touched <- TRUE
      }
      else if (outcome[1] == 0) {
        print("AGUA")
      }
    }
    if (touched) {
      tablero <- detectSunkBoats(tablero)
    }
    mostrarTablero(tablero)
  }
}

# Goes though the entire matrix looking for boats that are sunk but still marked as "H" and changes it to the "*" sign.
detectSunkBoats <- function(table) {
  for (x in 1:10)
  {
    for (y in 1:10)
    {
      verticalCt <- 0
      horizontalCt <- 0
      if (table[x, y] == "H") {
        valid <- TRUE
        isVertical <- TRUE
        if (y > 1 && y < 10) {
          if (table[x, (y - 1)] != "b" || table[x, (y + 1)] != "b") {
            isVertical <- FALSE
          }
        }
        else if (y > 1) {
          if (table[x, (y - 1)] != "b") {
            isVertical <- FALSE
          }
        }
        else {
          if (table[x, (y + 1)] != "b") {
            isVertical <- FALSE
          }
        }

        if (isVertical) {
          ### VERTICAL ###

          allH <- TRUE

          # check upward
          i <- 0
          status <- TRUE
          while (status & i <= 5) {
            if ((x - i) >= 1) {
              if (table[(x - i), y] == "b") {
                status <- FALSE
              }
              else if (table[(x - i), y] == "x") {
                status <- FALSE
                allH <- FALSE
              }
            }
            i <- i + 1
          }

          # check downward
          i <- 0
          status <- TRUE
          while (status & i <= 5) {
            if ((x + i) <= 10) {
              if (table[(x + i), y] == "b") {
                status <- FALSE
              }
              else if (table[(x + i), y] == "x") {
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
        else {
          ### HORIZONTAL ###

          allH <- TRUE

          # check left
          i <- 0
          status <- TRUE
          while (status & i <= 5) {
            if ((y - i) >= 1) {
              if (table[x, (y - i)] == "b") {
                status <- FALSE
              }
              else if (table[x, (y - i)] == "x") {
                status <- FALSE
                allH <- FALSE
              }
            }
            i <- i + 1
          }

          # check downward
          i <- 0
          status <- TRUE
          while (status & i <= 5) {
            if ((y + i) <= 10) {
              if (table[x, (y + i)] == "b") {
                status <- FALSE
              }
              else if (table[x, (y + i)] == "x") {
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
  for(x in 2:9)
  {
    for(y in 2:9)
    {
      if(table[x,y] != "b" && table[x,y] != "x")
      {
        valid = FALSE
      }
    }
  }
  if(valid)
  {
    for(x in 2:9)
    {
      for(y in 2:9)
      {
        if(table[x,y] == "x")
        {
          if(table[(x-1),y] == "x" && table[x,(y+1)] == "x")
          {
            valid = FALSE
          }
          if(table[(x-1),y] == "x" && table[x,(y-1)] == "x")
          {
            valid = FALSE
          }
          if(table[(x+1),y] == "x" && table[x,(y+1)] == "x")
          {
            valid = FALSE
          }
          if(table[(x+1),y] == "x" && table[x,(y-1)] == "x")
          {
            valid = FALSE
          }
        }
      }
    }
    
    if(valid)
    {
      x = 1
      for(y in 1:9)
      {
        if(table[(x+1),y] == "x" && table[x,(y+1)] == "x" && table[x,y] == "x")
        {
          valid = FALSE
        }
      }
      x = 10
      for(y in 1:9)
      {
        if(table[(x-1),y] == "x" && table[x,(y+1)] == "x" && table[x,y] == "x")
        {
          valid = FALSE
        }
      }
      if(table[1,1] == "x" && table[1,2] == "x" && table[2,1] == "x")
      {
        valid = FALSE
      }
      if(table[10,1] == "x" && table[9,1] == "x" && table[10,2] == "x")
      {
        valid = FALSE
      }
      if(table[1,10] == "x" && table[1,9] == "x" && table[2,10] == "x")
      {
        valid = FALSE
      }
      if(table[10,10] == "x" && table[10,9] == "x" && table[9,10] == "x")
      {
        valid = FALSE
      }
    }
  }
  return(valid)
}

countFlota<-function(table)
{
  table = cbind(table,c("b","b","b","b","b","b","b","b","b","b"))
  table = cbind(c("b","b","b","b","b","b","b","b","b","b"),table)
  table = rbind(table,c("b","b","b","b","b","b","b","b","b","b","b","b"))
  table = rbind(c("b","b","b","b","b","b","b","b","b","b","b","b"),table)
  rownames(table) = 1:12
  colnames(table) = 1:12
  
  for(x in 2:11)
  {
    for(y in 2:11)
    {
      if(table[x,y] == "x")
      {
        if(table[(x-1),y] == "b" && table[(x+1),y] == "b")
        {
          #HORIZONTAL
          ct = 0
          
          i = 1
          status = TRUE
          while((y+i) <= 12 && status)
          {
            if(table[x,(y+i)] == "b")
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
          while((y-i) >= 0 && status)
          {
            if(table[x,(y+i)] == "b")
            {
              status = FALSE
            }
            else
            {
              i = i + 1
            }
          }
          ct = ct + (i-1)
          
          print(ct)
        }
        else
        {
          #VERTICAL
          
        }
      }
    }
  }
}


opcion1 <- function() {
  if ("readxl" %in% rownames(installed.packages()) == FALSE) {
    install.packages("readxl")
  }
  if ("readr" %in% rownames(installed.packages()) == FALSE) {
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
  if(length(file) == 110)
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

  if(!validateEntireTable(tablero))
  {
    valid = FALSE
  }
  else
  {
    countFlota(tablero) #WORKING ON THIS FUNCTION (Julian)!
  }

  if (valid) {
    # SINGLE PLAYER
    print("Board successfully loaded.")
    mostrarTablero(tablero)
  }
  else {
    print("There has been a problem with the board you have uploaded. You are going to be redirected back to the menu so that you can try the 2 player mode. You can otherwise correct the .xls file and return to option 1.")
  }
}

opcion2 <- function() {
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
  while (flota1[1] < 1 || flota1[2] < 2 || flota1[3] < 2 || flota1[4] < 3 || flota1[5] < 4) {
    cat("\nTiene la siguiente cantidad de barcos disponibles:\n5 casillas: ", (1 - flota1[1]), "\n4 casillas: ", (2 - flota1[2]), "\n3 casillas: ", (2 - flota1[3]), "\n2 casillas: ", (3 - flota1[4]), "\n1 casilla: ", (4 - flota1[5]), "\n\n")
    print("Usuario nro 1: ingrese la posicion de un barco")
    input <- inputUsuario()
    while (!validarPosicion(input) || !tamanoDisponible(input, flota1) || !posicionDisponible(input, tablero1) || !boatsDontTouch(input, tablero1)) {
      print("Asegurese ingresar en formato valido un tama??o de barco disponible en una posicion vacia. Intentelo nuevamente:")
      input <- inputUsuario()
    }
    tablero1 <- actualizarTablero(input, tablero1)
    flota1 <- actualizarFlota(input, flota1)
    mostrarTablero(tablero1)
  }

  ## PLAYER 2
  while (flota2[1] < 1 || flota2[2] < 2 || flota2[3] < 2 || flota2[4] < 3 || flota2[5] < 4) {
    cat("\nTiene la siguiente cantidad de barcos disponibles:\n5 casillas: ", (1 - flota2[1]), "\n4 casillas: ", (2 - flota2[2]), "\n3 casillas: ", (2 - flota2[3]), "\n2 casillas: ", (3 - flota2[4]), "\n1 casilla: ", (4 - flota2[5]), "\n\n")
    print("Usuario nro 2: ingrese la posicion de un barco")
    input <- inputUsuario()
    while (!validarPosicion(input) || !posicionDisponible(input, tablero2) || !tamanoDisponible(input, flota2) || !boatsDontTouch(input, tablero2)) {
      print("Asegurese ingresar en formato valido un tama??o de barco disponible en una posicion vacia. Intentelo nuevamente:")
      input <- inputUsuario()
    }
    tablero2 <- actualizarTablero(input, tablero2)
    flota2 <- actualizarFlota(input, flota2)
    mostrarTablero(tablero2)
  }

  plays1(tablero1, tablero2)
}

opcion3 <- function() {
  print("You're in option 3")
}


menu() # Calls the main menu
