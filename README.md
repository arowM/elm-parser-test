# elm-parser-test

[![Build Status](https://travis-ci.org/arowM/elm-parser-test.svg?branch=master)](https://travis-ci.org/arowM/elm-parser-test)

Helper functions to develop/test your own parser using elm/parser.
It checks whether the result of parser is backtrackable or node.


```elm
run (keyword "import") "imp"
--> (Err _, True)

run (keyword "import") "import"
--> (Ok (), False)

run spaces "  "
--> (Ok (), False)

run (backtrackable spaces) "  "
--> (Ok (), True)
```
