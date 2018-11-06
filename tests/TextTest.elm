module TextTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Text exposing (TextResult(..), splitMatchCaseInsensitive)


suite : Test
suite =
    describe "string"
        [ describe "splitCaseIsensitive"
            [ test "case match" <|
                \_ ->
                    let
                        testString =
                            "my super code"

                        delimiter =
                            "super"

                        expected =
                            [ Rest "my ", Match "super", Rest " code" ]
                    in
                    Expect.equal expected (splitMatchCaseInsensitive delimiter testString)
            , test "first case match" <|
                \_ ->
                    let
                        testString =
                            "my super code"

                        delimiter =
                            "my"

                        expected =
                            [ Match "my", Rest " super code" ]
                    in
                    Expect.equal expected (splitMatchCaseInsensitive delimiter testString)
            , test "last case match" <|
                \_ ->
                    let
                        testString =
                            "my super code"

                        delimiter =
                            "code"

                        expected =
                            [ Rest "my super ", Match "code" ]
                    in
                    Expect.equal expected (splitMatchCaseInsensitive delimiter testString)
            , test "case match multiple matchs" <|
                \_ ->
                    let
                        testString =
                            "my super code, my super string"

                        delimiter =
                            "Super"

                        expected =
                            [ Rest "my ", Match "super", Rest " code, my ", Match "super", Rest " string" ]
                    in
                    Expect.equal expected (splitMatchCaseInsensitive delimiter testString)
            , test "delimiter case does not match" <|
                \_ ->
                    let
                        testString =
                            "my super code"

                        delimiter =
                            "Super"

                        expected =
                            [ Rest "my ", Match "super", Rest " code" ]
                    in
                    Expect.equal expected (splitMatchCaseInsensitive delimiter testString)
            , test "delimiter case does not match multiple" <|
                \_ ->
                    let
                        testString =
                            "my super code, my SupeR string"

                        delimiter =
                            "Super"

                        expected =
                            [ Rest "my ", Match "super", Rest " code, my ", Match "SupeR", Rest " string" ]
                    in
                    Expect.equal expected (splitMatchCaseInsensitive delimiter testString)
            ]
        ]
