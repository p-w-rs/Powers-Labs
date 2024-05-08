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
    , ncitations = 153
    , hindex = 3
    , i10index = 1
    , citations = [ { year = "2019", count = 6 }, { year = "2020", count = 3 }, { year = "2021", count = 23 }, { year = "2022", count = 36 }, { year = "2023", count = 68 }, { year = "2024", count = 16 } ]
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

                "/projects" ->
                    projects model

                "/books" ->
                    books model

                "/posts" ->
                    posts model

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
                [ navbar [ "About", "Projects", "Books", "Posts" ] path
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
    row [ width fill, padding 20, spacing 20, Border.widthEach { top = 0, bottom = 8, left = 0, right = 0 }, Border.color c_line ]
        (List.map (\p -> navlink p (path == String.toLower p)) paths)



-- VIEW :: FOOTER


footer : Element Msg
footer =
    row [ width fill, padding 20, Border.widthEach { top = 8, bottom = 0, left = 0, right = 0 }, Border.color c_line ]
        [ column [ alignLeft, spacing 5 ]
            [ el [ Font.bold ] (text "Joshua Powers")
            , el [] (text "AI Research Scientist")
            , el [] (text "dev@powerslabs.org")
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


profilePic =
    image [ moveDown 25, moveLeft 10, centerX, width (px 150), height (px 150), clip, Border.widthEach { top = 1, bottom = 1, left = 1, right = 1 }, Border.rounded 75, Border.color c_func ]
        { src = "assets/me.jpeg", description = "Profile picture" }


about_paragraphs =
    [ paragraph [] [ text "I am a passionate AI and ML professional, currently leading the AI ML group at Space Dynamics Laboratory, where I work on autonomous systems and projects for government contracts, ranging from deep reinforcement learning to vision tasks. I also provide guidance for other AI and ML projects and have experience in low-level, performance-critical software development in C." ]
    , paragraph [] [ text "Prior to my current role, I was a professor at MSOE, teaching AI and Operating Systems while also engaging in consulting work on recommender systems, AI-guided heat surgery, and data analytics. I pursued my PhD studies at UVM, focusing on embodied cognition, and earned my Bachelor's degree in Computer Science with Honors from BYU, where I first developed my passion for AI." ]
    , paragraph [] [ text "My fascination with AI and ML is broad, encompassing deep learning, evolutionary algorithms, reinforcement learning, robotics, and more. I enjoy exploring unconventional computing methods and applying AI and ML techniques to diverse domains, including biology and physics. I thrive on tackling challenges that require me to learn about new domains or apply novel techniques, and I take pride in my ability to persist in the face of complexity and uncertainty." ]
    , paragraph [] [ text "Beyond my work in AI and ML, I am deeply interested in revolutionizing education systems from elementary to university levels. I constantly challenge the status quo and generate ideas for improving various aspects of life. My straightforward and frank approach allows me to effectively communicate my thoughts and ideas." ]
    , paragraph [] [ text "As a dedicated husband and father, and a devout member of The Church of Jesus Christ of Latter-day Saints, my family is the center of my life. I cherish the time we spend together, whether it's exploring the great outdoors, playing sports, or inventing new games to enjoy at home. I take great joy in supporting my children's artistic and academic pursuits and being involved in their sports activities." ]
    ]


viewSkill : Skill -> Element Msg
viewSkill skill =
    row [ spacing 10, width fill ]
        [ el [ Font.size 14, width (px 200) ] (text skill.name)
        , row [ alignLeft, width (fillPortion skill.level), height fill, Back.color c_string ] []
        , row [ width (fillPortion (7 - skill.level)) ] [ el [ alignRight, paddingXY 5 0, Font.size 14 ] (text <| String.fromInt skill.level ++ "/5") ]
        ]


viewExperience : Experience -> Element Msg
viewExperience experience =
    column [ spacing 15, width fill, Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.color c_line, paddingXY 0 20 ]
        [ row [ spacing 10, width fill ]
            [ case experience.link of
                Just url ->
                    link [ Font.color c_tag, mouseOver [ Font.color c_string ] ] { url = url, label = text "⇱" }

                Nothing ->
                    none
            , el [ Font.bold, Font.size 18, Font.color c_func ] (text experience.company)
            ]
        , row [ spacing 20, width fill ]
            [ el [ Font.color c_keyword, Font.size 14, alignTop ] (text <| formatDateRange experience.startDate experience.endDate)
            , column [ spacing 10, width fill ]
                [ el [ Font.size 16, Font.bold, Font.color c_constant ] (text experience.position)
                , paragraph [ Font.color c_fg, Font.size 14 ] [ text experience.description ]
                , column [ Font.color c_fg, Font.size 14, spacing 10 ]
                    [ el [ Font.bold, Font.color c_constant ] (text "Acheivments:")
                    , column [ spacing 5 ] (List.map (\point -> el [ Font.alignLeft ] (text ("• " ++ point))) experience.keyPoints)
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
    column [ centerX, spacing 30, padding 20, width (fill |> maximum 800), onLeft profilePic ]
        [ textColumn [ spacing 20 ] about_paragraphs
        , column [ centerX, spacing 20 ]
            [ row [ spacing 20 ]
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
            ]
        , image [ centerX ] { src = "assets/citations.png", description = "Citations Graph" }
        , column [ width fill, spacing 10 ] (List.map viewSkill model.skills)
        , column [ spacing 30, width fill ] (List.map viewExperience model.experiences)
        ]



--------------------------------------------------------------------------------


projects model =
    el [] none



--------------------------------------------------------------------------------


books model =
    el [] none



--------------------------------------------------------------------------------


posts model =
    el [] none



--------------------------------------------------------------------------------


page404 =
    column [ centerX, height fill ]
        [ el [ padding 45, centerX, Font.color c_err, Font.size 48 ] (text "404 Not Found")
        , hide 0
        , el [ padding 10, alignLeft, Font.size 30 ] (text "The page you're looking for was not found.")
        , paragraph [ padding 10, centerX, Font.center, Font.size 30 ]
            [ text "Please go back to the "
            , link
                [ Font.color <| c_fg, mouseOver [ Font.color <| c_string ] ]
                { url = "/"
                , label = text "home page"
                }
            ]
        ]
