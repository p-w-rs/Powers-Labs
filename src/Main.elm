module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Back
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
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


type alias Flags =
    { width : Int
    , height : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , device : Device
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (classifyDevice flags)
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DeviceClassified Device


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url.path == "/admin" || url.path == "/teachers" then
                        ( model, Nav.load (Url.toString url) )

                    else
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize <|
        \width height ->
            DeviceClassified (classifyDevice { width = width, height = height })



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
    in
    { title = "My Personal Website"
    , body =
        [ layout []
            (column [ height fill, width fill, Font.color c_fg, Back.color c_bg ]
                [ navbar [ "About", "Projects", "Books", "Posts" ] (String.dropLeft 1 model.url.path)
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
        , paddingXY 0 8
        , Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        , Border.color color
        , mouseOver [ Border.color <| c_string ]
        ]
        { url = "/" ++ String.toLower name
        , label = text name
        }


navbar : List String -> String -> Element Msg
navbar paths path =
    row [ width fill ] (List.map (\p -> navlink p (path == String.toLower p)) paths)



-- VIEW :: FOOTER


footer : Element Msg
footer =
    el [] none



-- VIEW :: PAGES


about model =
    el [] none



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
