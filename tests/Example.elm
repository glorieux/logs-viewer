module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (extract, splitCaseInsensitive)
import Test exposing (..)


suite : Test
suite =
    describe "string"
        [ describe "extract"
            [test "whatever" <| 
                \_ ->
                    let
                        testString = "my super code"
                        indexes = [0, 3 , 8, 13]
                        expected = [ "my ", "super", " code" ]
                    in
                        Expect.equal expected (extract indexes testString [])
                    
            ]
        , describe "splitCaseIsensitive"
            [ test "case match" <|
                \_ ->
                    let
                        testString =
                            "my super code"

                        delimiter =
                            "super"

                        expected =
                            [ "my ", "super", " code" ]
                    in
                    Expect.equal expected (splitCaseInsensitive delimiter testString)
            , test "case match multiple matchs" <|
                \_ ->
                    let
                        testString =
                            "my super code, my super string"

                        delimiter =
                            "Super"

                        expected =
                            [ "my ", "super", " code, my ", "super", " string" ]
                    in
                    Expect.equal expected (splitCaseInsensitive delimiter testString)
            , test "delimiter case does not match" <|
                \_ ->
                    let
                        testString =
                            "my super code"

                        delimiter =
                            "Super"

                        expected =
                            [ "my ", "super", " code" ]
                    in
                    Expect.equal expected (splitCaseInsensitive delimiter testString)
            , test "delimiter case does not match multiple" <|
                \_ ->
                    let
                        testString =
                            "my super code, my SupeR string"

                        delimiter =
                            "Super"

                        expected =
                            [ "my ", "super", " code, my ", "SupeR", " string" ]
                    in
                    Expect.equal expected (splitCaseInsensitive delimiter testString)
            ]
        ]
