# Check identifiers #######################################################
# https://github.com/munificent/craftinginterpreters/blob/master/test/scanning/identifiers.lox
test_that("Scanner correctly identifies various identifiers and EOF", {
  input <- "andy formless fo _ _123 _abc ab123 abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
  scanner <- Scanner$new(input)
  tokens <- scanner$scanTokens() # Assuming scanTokens() returns all tokens including EOF

  expected_identifiers <- c("andy", "formless", "fo", "_", "_123", "_abc", "ab123",
                            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_")

  # Check each identifier
  for (i in seq_along(expected_identifiers)) {
    expect_equal(tokens[[i]]$type, "IDENTIFIER")
    expect_equal(tokens[[i]]$lexeme, expected_identifiers[i])
    expect_null(tokens[[i]]$literal)
  }

  # Check for EOF token at the end
  expect_equal(tokens[[length(tokens)]]$type, "EOF")
  expect_null(tokens[[length(tokens)]]$literal)
})

# Check keywords #######################################################
# https://github.com/munificent/craftinginterpreters/blob/master/test/scanning/keywords.lox
test_that("Scanner correctly identifies reserved keywords and EOF", {
  input <- "and class else false for fun if nil or return super this true var while"
  scanner <- Scanner$new(input)
  tokens <- scanner$scanTokens() # Assuming scanTokens() returns all tokens including EOF

  expected_types <- c("AND", "CLASS", "ELSE", "FALSE", "FOR", "FUN", "IF", "NIL",
                      "OR", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE")

  # Check each reserved keyword
  for (i in seq_along(expected_types)) {
    expect_equal(tokens[[i]]$type, expected_types[i])
    expect_equal(tokens[[i]]$lexeme, tolower(expected_types[i]))
    expect_null(tokens[[i]]$literal)
  }

  # Check for EOF token at the end
  expect_equal(tokens[[length(tokens)]]$type, "EOF")
  expect_null(tokens[[length(tokens)]]$literal)
})

# Check numbers and dots #######################################################
# https://github.com/munificent/craftinginterpreters/blob/master/test/scanning/numbers.lox
test_that("Scanner correctly identifies numbers, dots, and EOF", {
  inputs <- "123 123.456 .456 123."

  expected_outputs <- list(
    list(type = "NUMBER", lexeme = "123", literal = 123.0),
    list(type = "NUMBER", lexeme = "123.456", literal = 123.456),
    list(type = "DOT", lexeme = ".", literal = NULL),
    list(type = "NUMBER", lexeme = "456", literal = 456.0),
    list(type = "NUMBER", lexeme = "123", literal = 123.0),
    list(type = "DOT", lexeme = ".", literal = NULL),
    list(type = "EOF", lexeme = NULL, literal = NULL)
  )

  for (input in inputs) {
    scanner <- Scanner$new(input)
    tokens <- scanner$scanTokens()

    for (i in seq_along(tokens)) {
      expected <- expected_outputs[[i]]
      result_type <- tokens[[i]]$type
      expected_type <- expected$type
      expect_equal(result_type, expected_type)
      if (!is.null(expected$lexeme)) {
        result_lexeme <- tokens[[i]]$lexeme
        expected_lexeme <- expected$lexeme
        expect_equal(result_lexeme, expected_lexeme)
      }
      if (!is.null(expected$literal)) {
        result_literal <- tokens[[i]]$literal
        expected_literal <- expected$literal
        expect_equal(result_literal, expected_literal)
      } else {
        expect_null(tokens[[i]]$literal)
      }
    }

    # Reset expected_outputs for the next input
    expected_outputs <- expected_outputs[-seq(length(tokens))]
  }

  # Check for EOF token at the end of the last input
  expect_equal(tokens[[length(tokens)]]$type, "EOF")
  expect_null(tokens[[length(tokens)]]$literal)
})

# Check punctuation and operators ##############################################
# https://github.com/munificent/craftinginterpreters/blob/master/test/scanning/punctuators.lox
test_that("Scanner correctly identifies symbols and EOF", {
  input <- "(){};,+-*!===<=>=!=<>/."
  scanner <- Scanner$new(input)
  tokens <- scanner$scanTokens() # Assuming scanTokens() returns all tokens including EOF

  expected_tokens <- list(
    list(type = "LEFT_PAREN", lexeme = "(", literal = NULL),
    list(type = "RIGHT_PAREN", lexeme = ")", literal = NULL),
    list(type = "LEFT_BRACE", lexeme = "{", literal = NULL),
    list(type = "RIGHT_BRACE", lexeme = "}", literal = NULL),
    list(type = "SEMICOLON", lexeme = ";", literal = NULL),
    list(type = "COMMA", lexeme = ",", literal = NULL),
    list(type = "PLUS", lexeme = "+", literal = NULL),
    list(type = "MINUS", lexeme = "-", literal = NULL),
    list(type = "STAR", lexeme = "*", literal = NULL),
    list(type = "BANG_EQUAL", lexeme = "!=", literal = NULL),
    list(type = "EQUAL_EQUAL", lexeme = "==", literal = NULL),
    list(type = "LESS_EQUAL", lexeme = "<=", literal = NULL),
    list(type = "GREATER_EQUAL", lexeme = ">=", literal = NULL),
    list(type = "BANG_EQUAL", lexeme = "!=", literal = NULL),
    list(type = "LESS", lexeme = "<", literal = NULL),
    list(type = "GREATER", lexeme = ">", literal = NULL),
    list(type = "SLASH", lexeme = "/", literal = NULL),
    list(type = "DOT", lexeme = ".", literal = NULL),
    list(type = "EOF", lexeme = NULL, literal = NULL)
  )

  for (i in seq_along(expected_tokens)) {
    expect_equal(tokens[[i]]$type, expected_tokens[[i]]$type)
    if (!is.null(expected_tokens[[i]]$lexeme)) {
      expect_equal(tokens[[i]]$lexeme, expected_tokens[[i]]$lexeme)
    }
    expect_null(tokens[[i]]$literal)
  }
})

# Check strings #######################################################
# https://github.com/munificent/craftinginterpreters/blob/master/test/scanning/strings.lox
test_that("Scanner correctly identifies string literals and EOF", {
  inputs <- '"" "string"'
  expected_outputs <- list(
    list(type = "STRING", lexeme = '""', literal = ""),
    #list(type = "EOF", lexeme = NULL, literal = NULL),
    list(type = "STRING", lexeme = '"string"', literal = "string"),
    list(type = "EOF", lexeme = NULL, literal = NULL)
  )

  for (input in inputs) {
    scanner <- Scanner$new(input)
    tokens <- scanner$scanTokens()

    for (i in seq_along(tokens)) {
      expected <- expected_outputs[[i]]
      result_type <- tokens[[i]]$type
      expected_type <- expected$type
      expect_equal(result_type, expected_type)
      if (!is.null(expected$lexeme)) {
        result_lexeme <- tokens[[i]]$lexeme
        expected_lexeme <- expected$lexeme
        expect_equal(result_lexeme, expected_lexeme)
      }
      if (!is.null(expected$literal)) {
        result_literal <- tokens[[i]]$literal
        expected_literal <- expected$literal
        expect_equal(result_literal, expected_literal)
      } else {
        expect_null(tokens[[i]]$literal)
      }
    }

    # Reset expected_outputs for the next input
    expected_outputs <- expected_outputs[-seq(length(tokens))]
  }

  # Check for EOF token at the end of the last input
  expect_equal(tokens[[length(tokens)]]$type, "EOF")
  expect_null(tokens[[length(tokens)]]$literal)
})

test_that("Scanner correctly identifies string literals and EOF", {
  inputs <- c('""', '"string"')
  expected_outputs <- list(
    list(type = "STRING", lexeme = '""', literal = ""),
    list(type = "EOF", lexeme = NULL, literal = NULL),
    list(type = "STRING", lexeme = '"string"', literal = "string"),
    list(type = "EOF", lexeme = NULL, literal = NULL)
  )

  for (input in inputs) {
    scanner <- Scanner$new(input)
    tokens <- scanner$scanTokens()

    for (i in seq_along(tokens)) {
      expected <- expected_outputs[[i]]
      result_type <- tokens[[i]]$type
      expected_type <- expected$type
      expect_equal(result_type, expected_type)
      if (!is.null(expected$lexeme)) {
        result_lexeme <- tokens[[i]]$lexeme
        expected_lexeme <- expected$lexeme
        expect_equal(result_lexeme, expected_lexeme)
      }
      if (!is.null(expected$literal)) {
        result_literal <- tokens[[i]]$literal
        expected_literal <- expected$literal
        expect_equal(result_literal, expected_literal)
      } else {
        expect_null(tokens[[i]]$literal)
      }
    }

    # Reset expected_outputs for the next input
    expected_outputs <- expected_outputs[-seq(length(tokens))]
  }

  # Check for EOF token at the end of the last input
  expect_equal(tokens[[length(tokens)]]$type, "EOF")
  expect_null(tokens[[length(tokens)]]$literal)
})

# Check correctly handles spaces, tabs, and newlines ###########################
# https://github.com/munificent/craftinginterpreters/blob/master/test/scanning/whitespace.lox
test_that("Scanner correctly spaces, tabs, and newlines", {
  input <- "space    tabs\t\t\t\tnewlines\n\n\n\n\nend"
  scanner <- Scanner$new(input)
  tokens <- scanner$scanTokens() # Assuming scanTokens() returns all tokens including EOF

  expected_tokens <- list(
    list(type = "IDENTIFIER", lexeme = "space", literal = NULL),
    list(type = "IDENTIFIER", lexeme = "tabs", literal = NULL),
    list(type = "IDENTIFIER", lexeme = "newlines", literal = NULL),
    list(type = "IDENTIFIER", lexeme = "end", literal = NULL),
    list(type = "EOF", lexeme = NULL, literal = NULL)
  )

  for (i in seq_along(expected_tokens)) {
    expect_equal(tokens[[i]]$type, expected_tokens[[i]]$type)
    if (!is.null(expected_tokens[[i]]$lexeme)) {
      expect_equal(tokens[[i]]$lexeme, expected_tokens[[i]]$lexeme)
    }
    expect_null(tokens[[i]]$literal)
  }
})

# Check Initializer #######################################################
test_that("Scanner is initialized correctly", {
  scanner <- Scanner$new("var x = 10;")
  expect_equal(scanner$source, "var x = 10;")
  expect_equal(scanner$current, 0)
  expect_equal(scanner$line, 1)
})

# Check scanner methods #######################################################
test_that("Scanner identifies identifiers correctly", {
  scanner <- Scanner$new("varName")
  tokens <- scanner$scanTokens()
  expect_equal(length(tokens), 2)
  expect_equal(tokens[[1]]$type, "IDENTIFIER")
  expect_equal(tokens[[1]]$lexeme, "varName")
})

