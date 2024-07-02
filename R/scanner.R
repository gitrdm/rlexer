#' Scanner Class
#'
#' This class is designed for lexical analysis, converting a source string into a list of tokens.
#'
#' @field source The source code as a string to be scanned.
#' @field tokens A list to store the tokens generated from the source code.
#' @field start The index of the first character in the current lexeme being scanned.
#' @field current The index of the current character being scanned.
#' @field line The current line number in the source code.
#'
#'
#' @import R6
#'
#' @export
Scanner <- R6::R6Class(
  classname = "Scanner",
  public = list(
    source = NULL,
    tokens = list(),
    start = 0,
    current = 0,
    line = 1,


    #'
    #' @param source
    #'
    #'
    initialize = function(source) {
      self$source <- source
    },

    #' Scan Tokens
    #'
    #' Scans the source code and generates a list of tokens.
    #' @return A list of tokens.
    scanTokens = function() {
      while (!self$isAtEnd()) {
        self$start <- self$current
        self$scanToken()
      }

      self$tokens[[length(self$tokens) + 1]] <- list(
        type = "EOF",
        lexeme = NULL,
        literal = NULL,
        line = self$line
      )
      return(self$tokens)
    },


    #' Scan a Single Token
    #'
    #' Scans and identifies the next token in the source code.
    #'
    #'
    #'
    #'
    #' @return The next token.
    scanToken = function() {
      c <- self$advance()
      switch(
        c,
        '(' = self$addToken("LEFT_PAREN"),
        ')' = self$addToken("RIGHT_PAREN"),
        '{' = self$addToken("LEFT_BRACE"),
        '}' = self$addToken("RIGHT_BRACE"),
        ',' = self$addToken("COMMA"),
        '.' = self$addToken("DOT"),
        '-' = self$addToken("MINUS"),
        '+' = self$addToken("PLUS"),
        ';' = self$addToken("SEMICOLON"),
        '*' = self$addToken("STAR"),
        '!' = if (self$match('='))
          self$addToken("BANG_EQUAL")
        else
          self$addToken("BANG"),
        '=' = if (self$match('='))
          self$addToken("EQUAL_EQUAL")
        else
          self$addToken("EQUAL"),
        '<' = if (self$match('='))
          self$addToken("LESS_EQUAL")
        else
          self$addToken("LESS"),
        '>' = if (self$match('='))
          self$addToken("GREATER_EQUAL")
        else
          self$addToken("GREATER"),
        '/' = {
          if (self$match('/')) {
            while (self$peek() != '\n' && !self$isAtEnd())
              self$advance()
          } else {
            self$addToken("SLASH")
          }
        },
        ' ' = {
        },
        '\r' = {
        },
        '\t' = {
        },
        '\n' = {
          self$line <- self$line + 1
        },
        '"' = self$string(),
        {
          if (self$isDigit(c)) {
            self$number()
          } else if (self$isAlpha(c)) {
            self$identifier()
          } else {
            stop(paste("Line", self$line, ": Unexpected character."))
          }
        }
      )
    },


    #' Scan Identifier
    #'
    #' Scans and identifies an identifier token.
    #'
    #'
    #'
    #'
    #' @return The identifier token.
    identifier = function() {
      while (self$isAlphaNumeric(self$peek()))
        self$advance()

      text <- substr(self$source, self$start + 1, self$current)
      type <- ifelse(!is.null(keywords[[text]]), keywords[[text]], "IDENTIFIER")
      self$addToken(type)
    },

    #' Scan Number
    #'
    #' Scans and identifies a numeric token.
    #'
    #' @return The numeric token.
    #'
    #'
    #'
    number = function() {
      while (self$isDigit(self$peek()))
        self$advance()

      if (self$peek() == '.' && self$isDigit(self$peekNext())) {
        self$advance()
        while (self$isDigit(self$peek()))
          self$advance()
      }

      literal <- as.numeric(substr(self$source, self$start + 1, self$current))
      self$addToken("NUMBER", literal)
    },
    #' Scan String
    #'
    #' Scans and identifies a string token.
    #'
    #' @return The string token.
    #'
    #'
    #'
    string = function() {
      #' Scan String
      #'
      #' Scans and identifies a string token.
      while (self$peek() != '"' && !self$isAtEnd()) {
        if (self$peek() == '\n')
          self$line <- self$line + 1
        self$advance()
      }

      if (self$isAtEnd()) {
        stop(paste("Line", self$line, ": Unterminated string."))
        return()
      }

      self$advance()
      value <- substr(self$source, self$start + 2, self$current - 1)
      self$addToken("STRING", value)
    },

    #' Match Character
    #'
    #' Checks if the next character matches the expected character.
    #' @param expected The expected character.
    #' @return TRUE if the next character matches the expected character, FALSE otherwise.
    match = function(expected) {
      if (self$isAtEnd())
        return(FALSE)
      debug1 <- substring(self$source, self$current + 1, self$current + 1)
      if (substring(self$source, self$current + 1, self$current + 1) != expected)
        return(FALSE)

      self$current <- self$current + 1
      return(TRUE)
    },

    #' Peek
    #'
    #' Returns the current character without advancing the scanner.
    #' @return The current character.
    peek = function() {
      if (self$isAtEnd())
        return("")
      return(substring(self$source, self$current + 1, self$current + 1))
    },

    #' Peek Next
    #'
    #' Returns the next character without advancing the scanner.
    #' @return The next character.
    peekNext = function() {
      if (self$current + 1 >= nchar(self$source))
        return("")
      return(substring(self$source, self$current + 2, self$current + 2))
    },

    #' Check if Alpha
    #'
    #' Checks if the character is an alphabetical character.
    #' @param c The character to check.
    #' @return TRUE if the character is alphabetical, FALSE otherwise.
    isAlpha = function(c) {
      return(grepl("^[a-zA-Z_]$", c))
    },

    #' Check if AlphaNumeric
    #'
    #' Checks if the character is alphanumeric.
    #' @param c The character to check.
    #' @return TRUE if the character is alphanumeric, FALSE otherwise.
    isAlphaNumeric = function(c) {
      return(self$isAlpha(c) || self$isDigit(c))
    },

    #' Check if Digit
    #'
    #' Checks if the character is a digit.
    #' @param c The character to check.
    #' @return TRUE if the character is a digit, FALSE otherwise.
    isDigit = function(c) {
      return(grepl("^[0-9]$", c))
    },

    #' Check if At End
    #'
    #' Checks if the scanner has reached the end of the source code.
    #' @return TRUE if the scanner is at the end of the source code, FALSE otherwise.
    isAtEnd = function() {
      return(self$current >= nchar(self$source))
    },

    #' Advance
    #'
    #' Advances the scanner to the next character.
    #' @return The next character.
    advance = function() {
      self$current <- self$current + 1
      return(substring(self$source, self$current, self$current))
    },
    #' Add Token
    #'
    #' Adds a new token to the list of tokens.
    #' @param type The type of the token.
    #' @param literal The literal value of the token (optional).
    addToken = function(type, literal = NULL) {
      text <- substr(self$source, self$start + 1, self$current)
      self$tokens[[length(self$tokens) + 1]] <- list(
        type = type,
        lexeme = text,
        literal = literal,
        line = self$line
      )
    }
  ),
  private = list(),
  active = list()
)

#' Keywords
#'
#' A list of reserved keywords and their corresponding token types.
keywords <- list(
  "and" = "AND",
  "class" = "CLASS",
  "else" = "ELSE",
  "false" = "FALSE",
  "for" = "FOR",
  "fun" = "FUN",
  "if" = "IF",
  "nil" = "NIL",
  "or" = "OR",
  "print" = "PRINT",
  "return" = "RETURN",
  "super" = "SUPER",
  "this" = "THIS",
  "true" = "TRUE",
  "var" = "VAR",
  "while" = "WHILE"
)
