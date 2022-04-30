module AboutPage exposing (view)

import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

view : Element msg
view =
    column
        [ spacing 24 
        , paddingXY 36 24
        ]
        [ paragraph 
            [ spacing 16 ]
            [ text """
				XcuseMe is the exercise tracking app for real people.
           		Use it to track your exercise -- and your excuses!
           		Ever wonder how many times you've skipped your
           		ab routine because you woke up too late? Or
           		exactly how long that knee injury kept you on the
           		couch? Life happens and sometimes we can't
           		keep up, but now we can keep track.
				"""
            ]
        , paragraph
            [ spacing 16 ]
            [ text """
                To record a new activity, tap the "Log
                Excuse" or "Log Exercise" button, enter
                a text description of your exercise or excuse, and
                press "Save". Only one type of activity can
                be saved on a given day. If you went
                for a run yesterday, but skipped your core workout
                , you can note that in the description of your
                exercise and be proud that you did anything!
                """
            ]
        , paragraph
            [ spacing 16 ]
            [ text """
                XcuseMe is currently in active development. To report a
                bug or request a feature, please create an issue on
                """
            , link [ Font.color Colors.blue, Font.underline ]
                { url = "https://github.com/unterkoefler/xcuseme"
                , label = text "the GitHub repository."
                }
            ]
        , paragraph
            [ spacing 16 ]
            [ text """
                Thank you and have a good day!
                """
            ]
        ]

