module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Colors exposing (..)
import Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Back
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Http
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)
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
-- MODEL :: EXPERIENCE


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



-- MODEL :: REPOSITORIES


type Status
    = Done
    | Wip
    | Todo


type alias Reference =
    { title : String
    , url : String
    , year : Maybe Int
    }


type alias RepoMeta =
    { description : String
    , status : Status
    , references : Maybe (List Reference)
    , tags : List String
    }


type alias Repo =
    { name : String
    , url : String
    , meta : Maybe RepoMeta
    }


type SortBy
    = Alphabetical
    | Year Bool -- True for ascending



-- MODEL :: MAIN


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
    , repos : List Repo
    , selectedTags : Set String
    , allTags : Set String
    , sortBy : SortBy
    , expandedRepos : Set String
    , reposLoading : Bool
    , reposTotal : Int
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key
        url
        (classifyDevice flags)
        []
        -- skills
        []
        -- experiences
        []
        -- repos
        Set.empty
        -- selectedTags
        Set.empty
        -- allTags
        Alphabetical
        -- sortBy
        Set.empty
        -- expandedRepos
        True
        -- reposLoading
        0
      -- reposTotal
    , Cmd.batch
        [ Http.get
            { url = "assets/skills.json"
            , expect = Http.expectJson GotSkills (Decode.list skillDecoder)
            }
        , Http.get
            { url = "assets/experience.json"
            , expect = Http.expectJson GotExperiences (Decode.list experienceDecoder)
            }
        , Http.get
            { url = "https://api.github.com/users/p-w-rs/repos"
            , expect = Http.expectJson GotRepos (Decode.list githubRepoDecoder)
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
    , ncitations = 182
    , hindex = 3
    , i10index = 2
    , citations = [ { year = "2019", count = 5 }, { year = "2020", count = 3 }, { year = "2021", count = 24 }, { year = "2022", count = 38 }, { year = "2023", count = 62 }, { year = "2024", count = 50 } ]
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


statusDecoder : Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "DONE" ->
                        Decode.succeed Done

                    "WIP" ->
                        Decode.succeed Wip

                    "TODO" ->
                        Decode.succeed Todo

                    _ ->
                        Decode.fail "Invalid status"
            )


referenceDecoder : Decoder Reference
referenceDecoder =
    Decode.map3 Reference
        (Decode.field "title" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "year" (Decode.nullable Decode.int))


repoMetaDecoder : Decoder RepoMeta
repoMetaDecoder =
    Decode.map4 RepoMeta
        (Decode.field "description" Decode.string)
        (Decode.field "status" statusDecoder)
        (Decode.field "references" (Decode.nullable (Decode.list referenceDecoder)))
        (Decode.field "tags" (Decode.list Decode.string))


githubRepoDecoder : Decoder { name : String, url : String }
githubRepoDecoder =
    Decode.map2 (\name url -> { name = name, url = url })
        (Decode.field "name" Decode.string)
        (Decode.field "html_url" Decode.string)



-- meta starts as Nothing


fetchRepoMeta : String -> String -> Cmd Msg
fetchRepoMeta owner repo =
    Http.get
        { url = "https://raw.githubusercontent.com/" ++ owner ++ "/" ++ repo ++ "/main/meta.json"
        , expect = Http.expectJson (GotRepoMeta repo) repoMetaDecoder
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DeviceClassified Device
    | GotSkills (Result Http.Error (List Skill))
    | GotExperiences (Result Http.Error (List Experience))
    | GotRepos (Result Http.Error (List { name : String, url : String }))
    | GotRepoMeta String (Result Http.Error RepoMeta)
    | ToggleTag String
    | SetSortBy SortBy
    | ToggleRepoExpand String


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

        GotRepos result ->
            case result of
                Ok githubRepos ->
                    let
                        repos =
                            List.map
                                (\r ->
                                    { name = r.name
                                    , url = r.url
                                    , meta = Nothing
                                    }
                                )
                                githubRepos
                    in
                    ( { model
                        | repos = repos
                        , reposLoading = True
                        , reposTotal = List.length repos
                      }
                    , Cmd.batch
                        (List.map
                            (\repo ->
                                fetchRepoMeta "p-w-rs" repo.name
                            )
                            repos
                        )
                    )

                Err _ ->
                    ( { model | reposLoading = False }, Cmd.none )

        GotRepoMeta repoName result ->
            case result of
                Ok meta ->
                    let
                        updateRepo repo =
                            if repo.name == repoName then
                                { repo | meta = Just meta }

                            else
                                repo

                        newRepos =
                            List.map updateRepo model.repos

                        allTags =
                            newRepos
                                |> List.filterMap .meta
                                |> List.concatMap .tags
                                |> Set.fromList

                        loadedCount =
                            List.length (List.filter (.meta >> (/=) Nothing) newRepos)

                        stillLoading =
                            loadedCount < model.reposTotal
                    in
                    ( { model
                        | repos = newRepos
                        , allTags = allTags
                        , reposLoading = stillLoading
                      }
                    , Cmd.none
                    )

                Err _ ->
                    let
                        loadedCount =
                            List.length (List.filter (.meta >> (/=) Nothing) model.repos)

                        stillLoading =
                            loadedCount < model.reposTotal
                    in
                    ( { model | reposLoading = stillLoading }
                    , Cmd.none
                    )

        ToggleTag tag ->
            let
                newTags =
                    if Set.member tag model.selectedTags then
                        Set.remove tag model.selectedTags

                    else
                        Set.insert tag model.selectedTags
            in
            ( { model | selectedTags = newTags }, Cmd.none )

        SetSortBy sort ->
            ( { model | sortBy = sort }, Cmd.none )

        ToggleRepoExpand repoName ->
            let
                newExpanded =
                    if Set.member repoName model.expandedRepos then
                        Set.remove repoName model.expandedRepos

                    else
                        Set.insert repoName model.expandedRepos
            in
            ( { model | expandedRepos = newExpanded }, Cmd.none )



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
                    repositories model

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
                [ navbar [ "About", "Repositories" ] path
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
            [ newTabLink [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://github.com/p-w-rs", label = text "⇱ GitHub" }
            , newTabLink [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://www.linkedin.com/in/joshua-p-76b30b27b/", label = text "⇱ LinkedIn" }
            , newTabLink [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
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
    [ paragraph [] [ text "I am a passionate AI﹠ML professional at Space Dynamics Laboratory, where I work on autonomous systems and projects for government contracts. I also provide guidance for other AI and ML projects and occasionaly deploy low-level, performance-critical software in C. Prior to my current role, I was a professor at MSOE, teaching AI and Operating Systems while also engaging in consulting work on data analytics, recommender systems, and AI-guided heart surgery." ]
    , paragraph [] [ text "My fascination with AI and ML is broad, encompassing deep learning, evolutionary algorithms, reinforcement learning, robotics, and more. I also enjoy exploring unconventional computing methods and applying AI and ML techniques to diverse domains. I thrive on tackling challenges that require me to learn about new domains or apply novel techniques, and I take pride in my ability to persist in the face of complexity and uncertainty." ]
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
            [ newTabLink [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://scholar.google.com/citations?user=O5eVQi0AAAAJ&hl=en", label = text "⇱ Google Scholar" }
            , newTabLink [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://github.com/jpp46", label = text "⇱ PhD Code" }
            , newTabLink [ Font.color c_tag, mouseOver [ Font.color c_string ] ]
                { url = "https://github.com/p-w-rs", label = text "⇱ Active GitHub" }
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
                    newTabLink [ Font.color c_tag, mouseOver [ Font.color c_string ] ] { url = url, label = text "⇱" }

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


statusToString : Status -> String
statusToString status =
    case status of
        Done ->
            "DONE"

        Wip ->
            "WIP"

        Todo ->
            "TODO"


statusToColor : Status -> Element.Color
statusToColor status =
    case status of
        Done ->
            c_string

        Wip ->
            c_func

        Todo ->
            c_err


viewStatusDescription : Element msg
viewStatusDescription =
    column [ spacing 10, padding 20, Border.width 1, Border.color c_func, Border.rounded 5 ]
        [ el [ Font.bold ] (text "Status Descriptions:")
        , textColumn [ spacing 5 ]
            [ paragraph [] [ el [ Font.color (statusToColor Done) ] (text "DONE"), text " - Project is complete and ready to use" ]
            , paragraph [] [ el [ Font.color (statusToColor Wip) ] (text "WIP"), text " - Work in progress, may not be fully functional" ]
            , paragraph [] [ el [ Font.color (statusToColor Todo) ] (text "TODO"), text " - Planned project, not yet started or in early stages" ]
            ]
        ]


viewTagFilter : Set String -> Set String -> List Repo -> Element Msg
viewTagFilter allTags selectedTags repos =
    let
        getAvailableTags : Set String -> List String
        getAvailableTags selected =
            repos
                |> List.filterMap .meta
                |> List.filter
                    (\meta ->
                        List.all (\tag -> List.member tag meta.tags) (Set.toList selected)
                    )
                |> List.concatMap .tags
                |> Set.fromList
                |> Set.toList

        availableTags =
            if Set.isEmpty selectedTags then
                Set.toList allTags

            else
                getAvailableTags selectedTags
    in
    wrappedRow [ spacing 10, padding 20 ]
        (List.map
            (\tag ->
                let
                    isSelected =
                        Set.member tag selectedTags

                    isAvailable =
                        List.member tag availableTags

                    attributes =
                        if isSelected then
                            [ padding 5
                            , Border.width 1
                            , Border.rounded 3
                            , Font.color c_line
                            , Border.color c_func
                            , Back.color c_special
                            , pointer
                            ]

                        else if isAvailable then
                            [ padding 5
                            , Border.width 1
                            , Border.rounded 3
                            , Border.color c_func
                            , Back.color c_bg
                            , pointer
                            ]

                        else
                            [ padding 5
                            , Border.width 1
                            , Border.rounded 3
                            , Border.color c_func
                            , Back.color c_bg
                            , Font.color (rgba 1 1 1 0.5)
                            ]
                in
                Input.button attributes
                    { onPress =
                        if isSelected || isAvailable then
                            Just (ToggleTag tag)

                        else
                            Nothing
                    , label = text tag
                    }
            )
            (Set.toList allTags)
        )


viewSortControls : SortBy -> Element Msg
viewSortControls currentSort =
    row [ spacing 20, padding 20 ]
        [ text "Sort by:"
        , Input.button [ Font.color c_tag ]
            { onPress = Just (SetSortBy Alphabetical)
            , label = text "Name"
            }
        , Input.button [ Font.color c_tag ]
            { onPress = Just (SetSortBy (Year True))
            , label = text "Year (Asc)"
            }
        , Input.button [ Font.color c_tag ]
            { onPress = Just (SetSortBy (Year False))
            , label = text "Year (Desc)"
            }
        ]


viewReference : Reference -> Element msg
viewReference ref =
    row [ spacing 10 ]
        [ newTabLink [ Font.color c_tag ]
            { url = ref.url
            , label = text ref.title
            }
        , case ref.year of
            Just y ->
                text ("(" ++ String.fromInt y ++ ")")

            Nothing ->
                none
        ]


viewRepo : Set String -> Repo -> Element Msg
viewRepo expandedRepos repo =
    case repo.meta of
        Just meta ->
            let
                latestYear =
                    meta.references
                        |> Maybe.withDefault []
                        |> List.filterMap .year
                        |> List.maximum
            in
            column [ width fill, spacing 10, padding 20, Border.width 1, Border.color c_func ]
                [ row [ width fill, spacing 10 ]
                    [ newTabLink [ Font.color c_tag ]
                        { url = repo.url
                        , label = text repo.name
                        }
                    , el [ Font.color (statusToColor meta.status) ] (text (statusToString meta.status))
                    , case latestYear of
                        Just year ->
                            el [ alignRight, Font.color c_keyword ] (text (String.fromInt year))

                        Nothing ->
                            none
                    ]
                , paragraph [] [ text meta.description ]
                , wrappedRow [ spacing 10 ] (List.map (\t -> el [ Font.color c_keyword ] (text ("#" ++ t))) meta.tags)
                , case meta.references of
                    Just refs ->
                        if not (List.isEmpty refs) then
                            Input.button [ width fill ]
                                { onPress = Just (ToggleRepoExpand repo.name)
                                , label =
                                    if Set.member repo.name expandedRepos then
                                        column [ spacing 5, width fill ]
                                            [ text "▼ References"
                                            , column [ spacing 5 ] (List.map viewReference refs)
                                            ]

                                    else
                                        text "▶ References"
                                }

                        else
                            none

                    Nothing ->
                        none
                ]

        Nothing ->
            none


repositories : Model -> Element Msg
repositories model =
    column [ width fill, spacing 20, padding 20 ]
        [ viewStatusDescription
        , viewTagFilter model.allTags model.selectedTags model.repos
        , viewSortControls model.sortBy
        , if List.isEmpty model.repos then
            el [ centerX, padding 20 ] (text "Loading repositories...")

          else
            let
                filteredRepos =
                    model.repos
                        |> List.filter
                            (\repo ->
                                case repo.meta of
                                    Just meta ->
                                        Set.isEmpty model.selectedTags
                                            || List.all (\tag -> List.member tag meta.tags) (Set.toList model.selectedTags)

                                    Nothing ->
                                        False
                            )

                sortedRepos =
                    case model.sortBy of
                        Alphabetical ->
                            List.sortWith (\a b -> compare a.name b.name) filteredRepos

                        Year asc ->
                            let
                                hasDate repo =
                                    case repo.meta of
                                        Just meta ->
                                            meta.references
                                                |> Maybe.withDefault []
                                                |> List.filterMap .year
                                                |> List.isEmpty
                                                |> not

                                        Nothing ->
                                            False

                                ( datedRepos, undatedRepos ) =
                                    List.partition hasDate filteredRepos

                                sortByYear repos =
                                    List.sortWith
                                        (\a b ->
                                            case ( a.meta |> Maybe.andThen .references, b.meta |> Maybe.andThen .references ) of
                                                ( Just refs1, Just refs2 ) ->
                                                    let
                                                        getYear =
                                                            List.filterMap .year >> List.maximum >> Maybe.withDefault 3000

                                                        comp =
                                                            if asc then
                                                                compare

                                                            else
                                                                \x y -> compare y x
                                                    in
                                                    comp (getYear refs1) (getYear refs2)

                                                _ ->
                                                    compare a.name b.name
                                        )
                                        repos

                                sortedDated =
                                    sortByYear datedRepos

                                sortedUndated =
                                    List.sortWith (\a b -> compare a.name b.name) undatedRepos
                            in
                            if List.isEmpty sortedDated || List.isEmpty sortedUndated then
                                sortByYear filteredRepos

                            else
                                sortedDated ++ [ { name = "divider", url = "", meta = Nothing } ] ++ sortedUndated
            in
            column [ width fill, spacing 20 ]
                [ if model.reposLoading then
                    el [ centerX ] (text "Loading repository metadata...")

                  else
                    none
                , column [ width fill, spacing 20 ]
                    (List.map
                        (\repo ->
                            if repo.name == "divider" then
                                column [ width fill, spacing 10 ]
                                    [ el [ width fill, Border.width 1, Border.color c_func ] none
                                    , el [ centerX, Font.color c_keyword ] (text "Projects without dates")
                                    ]

                            else
                                viewRepo model.expandedRepos repo
                        )
                        sortedRepos
                    )
                ]
        ]



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
