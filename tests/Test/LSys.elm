module Test.LSys exposing (suite)

import Array
import Expect
import LSys
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LSys"
        [ describe "generateSequence"
            [ test "a 0L system" <|
                \_ ->
                    --
                    -- See https://paulbourke.net/fractals/lsys/ for more testing ideas.
                    --
                    LSys.generateSequence
                        1
                        "F+F+F+F"
                        [ ( 'F', String.toList "F+F-F-FF+F+F-F" )
                        ]
                        |> Expect.equal (Array.fromList <| String.toList "F+F-F-FF+F+F-F+F+F-F-FF+F+F-F+F+F-F-FF+F+F-F+F+F-F-FF+F+F-F")
            ]
        ]
