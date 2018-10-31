module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main exposing (markContent)

suite : Test
suite =
    describe "markContent"
        test "name" <|
            \_ ->
                
