module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Colors exposing (..)
import Date exposing (Date)
import Element exposing (..)
import Element.Background as Back
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Http
import Json.Decode as Decode exposing (Decoder)
import Time exposing (Month(..))
import Url



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Skill =
    { name : String
    , level : Int
    }


type alias Experience =
    { company : String
    , position : String
    , startDate : Date
    , endDate : Maybe Date
    , description : String
    , keyPoints : List String
    , link : Maybe String
    }


type alias Flags =
    { width : Int
    , height : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , device : Device
    , skills : List Skill
    , experiences : List Experience
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (classifyDevice flags) [] []
    , Cmd.batch
        [ Http.get
            { url = "assets/skills.json"
            , expect = Http.expectJson GotSkills (Decode.list skillDecoder)
            }
        , Http.get
            { url = "assets/experience.json"
            , expect = Http.expectJson GotExperiences (Decode.list experienceDecoder)
            }
        ]
    )



-- STATIC DATA


type alias GoogleScholarProfile =
    { npubs : Int
    , ncitations : Int
    , hindex : Int
    , i10index : Int
    , citations : List { year : String, count : Int }
    }


scholarProfile : GoogleScholarProfile
scholarProfile =
    { npubs = 13
    , ncitations = 176
    , hindex = 3
    , i10index = 2
    , citations = [ { year = "2019", count = 5 }, { year = "2020", count = 3 }, { year = "2021", count = 24 }, { year = "2022", count = 38 }, { year = "2023", count = 62 }, { year = "2024", count = 44 } ]
    }



-- DECODERS


skillDecoder : Decoder Skill
skillDecoder =
    Decode.map2 Skill
        (Decode.field "name" Decode.string)
        (Decode.field "level" Decode.int)


experienceDecoder : Decoder Experience
experienceDecoder =
    Decode.map7 Experience
        (Decode.field "company" Decode.string)
        (Decode.field "position" Decode.string)
        (Decode.field "startDate" dateDecoder)
        (Decode.field "endDate" (Decode.nullable dateDecoder))
        (Decode.field "description" Decode.string)
        (Decode.field "keyPoints" (Decode.list Decode.string))
        (Decode.field "link" (Decode.nullable Decode.string))


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Date.fromIsoString str of
                    Ok date ->
                        Decode.succeed date

                    Err _ ->
                        Decode.fail "Invalid date format"
            )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DeviceClassified Device
    | GotSkills (Result Http.Error (List Skill))
    | GotExperiences (Result Http.Error (List Experience))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        DeviceClassified device ->
            ( { model | device = device }
            , Cmd.none
            )

        GotSkills result ->
            case result of
                Ok skills ->
                    ( { model | skills = List.sortBy (\s -> ( negate s.level, s.name )) skills }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotExperiences result ->
            case result of
                Ok experiences ->
                    ( { model | experiences = List.sortBy (\e -> Maybe.withDefault (Date.fromCalendarDate 3000 Time.Jan 1) e.endDate |> Date.toRataDie |> negate) experiences }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize <| \width height -> DeviceClassified (classifyDevice { width = width, height = height })



-- VIEW :: MAIN


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.url.path of
                "/" ->
                    about model

                "/about" ->
                    about model

                "/repositories" ->
                    repos model

                "/reviews" ->
                    reviews model

                "/musings" ->
                    musings model

                _ ->
                    page404

        path =
            if model.url.path == "/" then
                "about"

            else
                String.dropLeft 1 model.url.path
    in
    { title = "My Personal Website"
    , body =
        [ layout []
            (column [ height fill, width fill, Font.color c_fg, Back.color c_bg ]
                [ navbar [ "About", "Repositories", "Reviews", "Musings" ] path
                , page
                , footer
                ]
            )
        ]
    }



-- VIEW :: NAVIGATION


navlink : String -> Bool -> Element Msg
navlink name active =
    let
        color =
            if active then
                c_string

            else
                inv
    in
    link
        [ centerX
        , paddingXY 0 3
        , Font.color c_tag
        , Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        , Border.color color
        , mouseOver [ Border.color <| c_string ]
        ]
        { url = "/" ++ String.toLower name
        , label = text name
        }


navbar : List String -> String -> Element Msg
navbar paths path =
    row [ width fill, padding 30, spacing 20, Font.size 24, Font.variant Font.smallCaps ]
        (List.map (\p -> navlink p (path == String.toLower p)) paths)



-- VIEW :: FOOTER


footer : Element Msg
footer =
    row [ width fill, padding 30, Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }, Border.color c_fg, Font.size 16, Font.variant Font.smallCaps ]
        [ column [ alignLeft, spacing 5 ]
            [ el [ Font.bold ] (text "Joshua Powers")
            , el [] (text "AI Research Scientist")
            , link [ mouseOver [ Font.color c_string ] ]
                { url = "mailto:dev@powerslabs.org", label = text "dev@powerslabs.org" }
            ]
        , column [ alignRight, spacing 5 ]
            [ link [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://github.com/p-w-rs", label = text "⇱ GitHub" }
            , link [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://www.linkedin.com/in/joshua-p-76b30b27b/", label = text "⇱ LinkedIn" }
            , link [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://scholar.google.com/citations?user=O5eVQi0AAAAJ&hl=en", label = text "⇱ Google Scholar" }
            ]
        ]



-- VIEW :: PAGES


profilePic : Element Msg
profilePic =
    image [ moveDown 10, moveLeft 50, centerX, width (px 150), height (px 150), clip, Border.widthEach { top = 1, bottom = 1, left = 1, right = 1 }, Border.rounded 75, Border.color c_func ]
        { src = "assets/me.jpeg", description = "Profile picture" }


about_paragraphs : List (Element Msg)
about_paragraphs =
    [ paragraph [] [ text "I am a passionate AI﹠ML professional, currently leading the AI﹠ML group at Space Dynamics Laboratory, where I work on autonomous systems and projects for government contracts. I also provide guidance for other AI and ML projects and occasionaly deploy low-level, performance-critical software in C. Prior to my current role, I was a professor at MSOE, teaching AI and Operating Systems while also engaging in consulting work on data analytics, recommender systems, and AI-guided heart surgery." ]
    , paragraph [] [ text "My fascination with AI and ML is broad, encompassing deep learning, evolutionary algorithms, reinforcement learning, robotics, and more. I also enjoy exploring unconventional computing methods and applying AI and ML techniques to diverse domains, including biology and physics. I thrive on tackling challenges that require me to learn about new domains or apply novel techniques, and I take pride in my ability to persist in the face of complexity and uncertainty." ]
    , paragraph [] [ text "Beyond my work in AI and ML, I am deeply interested in revolutionizing education systems from elementary to university levels. I constantly challenge the status quo and generate ideas for improving various aspects of life. My straightforward and frank approach allows me to effectively communicate my thoughts and ideas." ]
    , paragraph [] [ text "As a dedicated husband and father, and a devout member of The Church of Jesus Christ of Latter-day Saints, my family is the center of my life. I cherish the time we spend together, whether it's exploring the great outdoors, playing sports, or inventing new games to enjoy at home. I take great joy in supporting my children's artistic and academic pursuits and being involved in their sports activities." ]
    ]


viewCiteYear : { a | year : String, count : Int } -> Int -> Element Msg
viewCiteYear citation max =
    row [ width fill ]
        [ el [ Font.size 14, width (px 50) ] (text citation.year)
        , el [ alignLeft, width (fillPortion citation.count), height fill, Back.color c_string ] none
        , el [ width (fillPortion (max - citation.count)) ] none
        , el [ alignRight, Font.alignRight, paddingXY 5 0, width (px 50), Font.size 14 ] (text <| String.fromInt citation.count)
        ]


scholarSummary : Element Msg
scholarSummary =
    column [ centerX, width fill, alignTop, spacing 20, Font.size 18, Font.variant Font.smallCaps ]
        [ row [ centerX, spacing 20 ]
            [ el [] (text <| "Publications: " ++ String.fromInt scholarProfile.npubs)
            , el [] (text <| "Citations: " ++ String.fromInt scholarProfile.ncitations)
            , el [] (text <| "h-index: " ++ String.fromInt scholarProfile.hindex)
            , el [] (text <| "i10-index: " ++ String.fromInt scholarProfile.i10index)
            ]
        , row [ centerX, spacing 20 ]
            [ link [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://scholar.google.com/citations?user=O5eVQi0AAAAJ&hl=en", label = text "⇱ Google Scholar" }
            , link [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://github.com/jpp46", label = text "⇱ PhD Code" }
            , link [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://github.com/p-w-rs", label = text "⇱ GitHub" }
            ]
        , column [ centerX, width fill, spacing 10 ] <| List.map (\cite -> viewCiteYear cite 68) scholarProfile.citations
        ]


viewSkill : Skill -> Element Msg
viewSkill skill =
    row [ width fill ]
        [ el [ Font.size 14, Font.variant Font.smallCaps, width (px 200) ] (text skill.name)
        , el [ alignLeft, width (fillPortion skill.level), height fill, Back.color c_string ] none
        , el [ width (fillPortion (5 - skill.level)) ] none
        , el [ alignRight, Font.alignRight, paddingXY 5 0, width (px 50), Font.size 14 ] (text <| String.fromInt skill.level ++ "/5")
        ]


viewExperience : Experience -> Element Msg
viewExperience experience =
    column [ centerX, spacing 15, width fill, paddingXY 0 20 ]
        [ row [ spacing 10, width fill ]
            [ case experience.link of
                Just url ->
                    link [ Font.color c_tag, mouseOver [ Font.color c_string ] ] { url = url, label = text "⇱" }

                Nothing ->
                    none
            , el [ Font.bold, Font.size 18, Font.variant Font.smallCaps, Font.color c_func ] (text experience.company)
            ]
        , row [ spacing 20, width fill ]
            [ el [ Font.color c_keyword, Font.size 14, Font.variant Font.smallCaps, alignTop ] (text <| formatDateRange experience.startDate experience.endDate)
            , column [ spacing 10, width fill ]
                [ el [ Font.size 16, Font.variant Font.smallCaps, Font.bold, Font.color c_constant ] (text experience.position)
                , paragraph [ width fill, Font.color c_fg, Font.size 14 ] [ text experience.description ]
                , column [ width fill, Font.color c_fg, Font.size 14, spacing 10 ]
                    [ el [ width fill, Font.bold, Font.variant Font.smallCaps, Font.color c_constant ] (text "Acheivments:")
                    , textColumn [ width fill, spacing 5 ] (List.map (\point -> paragraph [ width fill, Font.alignLeft ] [ text ("• " ++ point) ]) experience.keyPoints)
                    ]
                ]
            ]
        ]


formatDateRange : Date -> Maybe Date -> String
formatDateRange startDate endDate =
    Date.format "MMMM yyyy" startDate
        ++ " - "
        ++ (case endDate of
                Just date ->
                    Date.format "MMMM yyyy" date

                Nothing ->
                    "Present"
           )


about : Model -> Element Msg
about model =
    column [ width fill, spacing 50, padding 20 ]
        [ textColumn [ centerX, width (fill |> maximum 800), spacing 20, Font.justify, onLeft profilePic ] about_paragraphs
        , wrappedRow [ width fill, spacing 30, padding 20, Border.width 1, Border.rounded 5, Border.color c_func ]
            [ scholarSummary
            , column [ centerX, width (fill |> minimum 400), spacing 10 ] (List.map viewSkill model.skills)
            ]
        , column [ centerX, width (fill |> maximum 800), spacing 20 ] (List.map viewExperience model.experiences)
        ]



--------------------------------------------------------------------------------


repos model =
    el [] none



--------------------------------------------------------------------------------


reviews model =
    el [] none



--------------------------------------------------------------------------------


musings model =
    el [] none



--------------------------------------------------------------------------------


page404 : Element msg
page404 =
    column [ centerX, height fill ]
        [ el [ padding 45, centerX, Font.color c_err, Font.size 48 ] (text "404 Not Found")
        , hide 0
        , el [ padding 10, alignLeft, Font.size 30 ] (text "The page you're looking for was not found.")
        , paragraph [ padding 10, centerX, Font.center, Font.size 30 ]
            [ text "Please go back to the "
            , link
                [ Font.color <| c_tag, mouseOver [ Font.color <| c_string ] ]
                { url = "/"
                , label = text "home page"
                }
            ]
        ]
