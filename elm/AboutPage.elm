module AboutPage exposing (view)

import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes


type HeadingId
    = Background
    | FAQ


headingIdToString : HeadingId -> String
headingIdToString id =
    case id of
        Background ->
            "background"

        FAQ ->
            "faq"


view : Element msg
view =
    column
        [ spacing 24
        , paddingXY 36 24
        ]
        ([ tableOfContents
         , heading "Background" Background
         ]
            ++ List.map bodyParagraph backgroundParagraphs
            ++ [ heading "Frequently Asked Questions" FAQ ]
            ++ List.map qAndA faqs
        )


tableOfContents : Element msg
tableOfContents =
    column
        [ spacing 8
        ]
        [ tocItem "Background" Background
        , tocItem "Frequently Asked Questions" FAQ
        ]


tocItem : String -> HeadingId -> Element msg
tocItem h id =
    paragraph
        []
        [ text "- "
        , link [ Font.color Colors.blue, Font.underline ]
            { url = "#" ++ headingIdToString id
            , label = text h
            }
        ]


heading : String -> HeadingId -> Element msg
heading h id =
    text h
        |> List.singleton
        |> paragraph
            [ Region.heading 2
            , Font.size 28
            , Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
            , htmlAttribute <| Html.Attributes.attribute "id" (headingIdToString id)
            ]


bodyParagraph : String -> Element msg
bodyParagraph =
    text
        >> List.singleton
        >> paragraph
            [ spacing 14 ]


qAndA : ( String, List (Element msg) ) -> Element msg
qAndA ( question, answer ) =
    column
        [ spacing 16
        ]
        [ paragraph
            [ spacing 12
            , Font.italic
            ]
            [ text question ]
        , paragraph
            [ spacing 12 ]
            answer
        ]


backgroundParagraphs : List String
backgroundParagraphs =
    [ """
    During the beginning of the pandemic, I downloaded this home workout app, called something like “30 Day Full Body Workout.” You were supposed to do one workout a day for 30 days. It took me four months to complete it. Some days I ran or biked or hiked or did some other exercise. Other days, it was raining or I was hungover or I had to watch Tiger King instead. I wanted to know where those four months had gone. I needed some way to track my various exercises — and my excuses! Thus, XcuseMe was born: “Exercise tracking for real people.”
    """
    , """
    I envisioned it as a Strava for sane people, with optional social sharing and integrations with Garmin for exercises and GrubHub for excuses. I imagined a sophisticated ML algorithm that would offer non-judgmental advice and encouragement, like “Your last eight excuses have been about waking up too late. Maybe try exercising in the evening instead, you doorknob.” 
    """
    , """
    Today, it is just a two-table CRUD app, but I use it almost every day. I can tell you exactly how many miles I ran in February, how long my cold lasted in March or how many times in the last year I was planning on working out right after work, but was too hungry  and then too full and then too high to do anything more than ten push-ups and one downward dog before bed (thirteen).
    """
    , """
    Yes, I could have used Excel for this or even just a notebook and a pen. But coding and learning new things is fun. Also, I still want to add some ML algorithms one day, which would be a bit harder to do on a notebook. And, maybe you'll like it too!
    """
    ]


faqs : List ( String, List (Element msg) )
faqs =
    [ ( """To whom will you sell my data?"""
      , [ text """No one. """ ]
      )
    , ( """To whom will you leak my data for free?"""
      , [ text """No one, hopefully. I disclaim any and all liability.""" ]
      )
    , ( """Who has access to my precious data?"""
      , [ text """Me, the developer; the people over at Digitally Induced who run """
        , link [ Font.color Colors.blue, Font.underline ]
            { url = "https://ihpcloud.com/"
            , label = text "IHP Cloud,"
            }
        , text """ which I use to host the site; and probably a few people over at Amazon, which is where the database is hosted. If this terrifies you, but you really want to use XcuseMe anyway, you’re welcome to self-host it! """
        , link [ Font.color Colors.blue, Font.underline ]
            { url = "https://github.com/unterkoefler/xcuseme"
            , label = text "The code is open source!"
            }
        ]
      )
    , ( """I found a bug. Where can I report it?"""
      , [ text """Please open an issue on """
        , link [ Font.color Colors.blue, Font.underline ]
            { url = "https://github.com/unterkoefler/xcuseme"
            , label = text "this project’s Github repository."
            }
        ]
      )
    , ( """Why can’t I log an excuse and an exercise on the same day?"""
      , [ text """First, I wouldn’t know what color to color the circle on the calendar. Second, I don’t think it makes a lot of sense to have both. If you exercised, you don’t need to excuse yourself from not exercising.""" ]
      )
    , ( """Why can’t I log multiple exercises on the same day?"""
      , [ text """I’m lazy. If you do this, you can put both exercises into the same text field.""" ]
      )
    , ( """Can I set up daily reminders to use this site?"""
      , [ text """Yes, but not through the site. Your phone or refrigerator should be able to help you with this. Try screaming “Hey Google/Alexa/Siri/Bixby/Ronald Reagan, set a daily reminder to log my excuses at 8pm!” """ ]
      )
    , ( """I forgot my password. Can you help me?"""
      , [ text """No. I’m using the free tier of IHP Cloud which does not support sending emails. Sorry!""" ]
      )
    ]
