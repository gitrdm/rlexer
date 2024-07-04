
<!-- README.md is generated from README.Rmd. Please edit that file -->

**NOTE: This project only serves as a means for me to learn R, R
Packaging, github usage and programming concepts. There is no real worth
to the code here beyond this.**

# rlexer

<!-- badges: start -->
<!-- badges: end -->

The goal of rlexer is to be a lexer for the programming language LOX as
described in the book Crafting Interpreters by Bob Nystrom.

The scanner is complete and passes all of the tests in the book. The
next step is to implement the parser and interpreter.

## Installation

You can install the development version of rlexer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gitrdm/rlexer")
```

## Example

This is a basic example which shows you how to the scanner is called:

``` r
library(rlexer)
## basic example code
input <- "andy _ _123 _abc ab123 abc_"
scanner <- Scanner$new(input)
tokens <- scanner$scanTokens() 
print(tokens)
#> [[1]]
#> [[1]]$type
#> [1] "IDENTIFIER"
#> 
#> [[1]]$lexeme
#> [1] "andy"
#> 
#> [[1]]$literal
#> NULL
#> 
#> [[1]]$line
#> [1] 1
#> 
#> 
#> [[2]]
#> [[2]]$type
#> [1] "IDENTIFIER"
#> 
#> [[2]]$lexeme
#> [1] "_"
#> 
#> [[2]]$literal
#> NULL
#> 
#> [[2]]$line
#> [1] 1
#> 
#> 
#> [[3]]
#> [[3]]$type
#> [1] "IDENTIFIER"
#> 
#> [[3]]$lexeme
#> [1] "_123"
#> 
#> [[3]]$literal
#> NULL
#> 
#> [[3]]$line
#> [1] 1
#> 
#> 
#> [[4]]
#> [[4]]$type
#> [1] "IDENTIFIER"
#> 
#> [[4]]$lexeme
#> [1] "_abc"
#> 
#> [[4]]$literal
#> NULL
#> 
#> [[4]]$line
#> [1] 1
#> 
#> 
#> [[5]]
#> [[5]]$type
#> [1] "IDENTIFIER"
#> 
#> [[5]]$lexeme
#> [1] "ab123"
#> 
#> [[5]]$literal
#> NULL
#> 
#> [[5]]$line
#> [1] 1
#> 
#> 
#> [[6]]
#> [[6]]$type
#> [1] "IDENTIFIER"
#> 
#> [[6]]$lexeme
#> [1] "abc_"
#> 
#> [[6]]$literal
#> NULL
#> 
#> [[6]]$line
#> [1] 1
#> 
#> 
#> [[7]]
#> [[7]]$type
#> [1] "EOF"
#> 
#> [[7]]$lexeme
#> NULL
#> 
#> [[7]]$literal
#> NULL
#> 
#> [[7]]$line
#> [1] 1
```

## Code of Conduct
