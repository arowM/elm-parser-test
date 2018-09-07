module Parser.Test exposing (run)

{-| Helper functions to develop/test your own parser using elm/parser.


# Core

@docs run

-}

import Parser exposing ((|.), DeadEnd, Parser, backtrackable, oneOf, problem, succeed)


{-| Check if a parser is backtrackable or not.

    import Parser

    run (Parser.keyword "import") "imp" |> Tuple.mapFirst (Result.mapError (\_ -> ()))
    --> (Err (), True)

    run (Parser.keyword "import") "import"
    --> (Ok (), False)

    run Parser.spaces "  "
    --> (Ok (), False)

    run (Parser.backtrackable Parser.spaces) "  "
    --> (Ok (), True)

-}
run : Parser a -> String -> ( Result (List DeadEnd) a, Bool )
run p str =
    case Parser.run p str of
        Ok a ->
            ( Ok a, isBacktrackableErr (p |. problem "fail") str )

        Err e ->
            ( Err e, isBacktrackableErr p str )


isBacktrackableErr : Parser a -> String -> Bool
isBacktrackableErr p str =
    let
        checker =
            oneOf
                [ Parser.map (\_ -> ()) p
                , succeed ()
                ]
    in
    Parser.run checker str == Ok ()
