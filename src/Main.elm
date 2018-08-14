module Main exposing (..)

-- import Draggable
-- import Draggable.Events exposing (onClick, onDragBy, onDragStart)
import Char
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HAttr
import Html.Events as HEvent
import Http
import Json.Decode as JD
import Regex
import Svg exposing (Svg)
import Svg.Attributes as SAttr
-- import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
-- import Svg.Events exposing (onMouseUp)
-- import Svg.Keyed
-- import Svg.Lazy exposing (lazy)
import Types exposing (..)



---- MAIN ----


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { vFile : VFile
    , elmPackageList : List ElmListPackage
    , showElmPackageModal : Bool
    , searchFilter : String
    , vPackage : VPackage
    , packageDocs : Dict Name (List ElmPackageDoc)
    , showSettingsModal : Bool
    , showImportModal : Bool
    }


type Msg
    = ModuleChangeName ModuleName
    | RequestElmPackages
    | ReceiveElmPackageList (Result Http.Error (List ElmListPackage))
    | UpdateSearchFilter String
    | ShowPackageModal Bool
    | AddDependency Name Version
    | RemoveDependency Name
    | ReceiveElmPackageDocs Name (Result Http.Error (List ElmPackageDoc))
    | ShowSettingsModal Bool
    | ChangeVersion Version
    | ChangeSummmary String
    | ChangeLicense String
    | ChangeRepository String
    | ShowImportModal Bool


init : ( Model, Cmd Msg )
init =
    ( { vFile = makeVFile (ModuleDeclaration "TestModule" AllExport) [] [] [] []
      , elmPackageList = []
      , showElmPackageModal = False
      , searchFilter = ""
      , vPackage = defaultVPackage
      , packageDocs = Dict.empty
      , showSettingsModal = False
      , showImportModal = False
      }
    , Cmd.batch [ getPackageDocs "elm-lang/core" (Version 5 1 1)
                ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ vFile, elmPackageList, vPackage, packageDocs } as model) =
    case msg of
        ModuleChangeName newName ->
            ( { model | vFile = updateModuleName vFile newName
              }
            , Cmd.none
            )
        RequestElmPackages ->
            ( { model | showElmPackageModal = True }, getPackages )
        ReceiveElmPackageList (Err err) ->
            log (toString err) ( { model
              | elmPackageList = []
              , showElmPackageModal = False
              }
            , Cmd.none
            )
        ReceiveElmPackageList (Ok listPackages) ->
            ( { model | elmPackageList = listPackages
              }
            , Cmd.none
            )
        UpdateSearchFilter text ->
            ( { model | searchFilter = text }, Cmd.none )
        ShowPackageModal showModal ->
            ( { model | showElmPackageModal = showModal }, Cmd.none )
        AddDependency name version ->
            ( { model
              | vPackage = addDependencyToModel vPackage name version
              , showElmPackageModal = False
              }
            , getPackageDocs name version
            )
        RemoveDependency name ->
              ( { model
                | vPackage = removeDependencyToModel vPackage name
                }
              , Cmd.none
              )
        ReceiveElmPackageDocs name (Err err) ->
            log (toString err) ( model, Cmd.none )
        ReceiveElmPackageDocs name (Ok docs) ->
            ( { model
              | packageDocs = Dict.insert name docs packageDocs
              }
            , Cmd.none
            )
        ShowSettingsModal nextShow ->
            ( { model | showSettingsModal = nextShow }, Cmd.none )
        ChangeVersion v ->
            ( { model | vPackage = changeVersion vPackage v }, Cmd.none )
        ChangeSummmary summary ->
            ( { model | vPackage = changeSummary vPackage summary }, Cmd.none )
        ChangeLicense license ->
            ( { model | vPackage = changeLicense vPackage license }, Cmd.none )
        ChangeRepository repo ->
            ( { model | vPackage = changeRepository vPackage repo }, Cmd.none )
        ShowImportModal nextShow ->
            ( { model | showImportModal = nextShow }, Cmd.none )



changeVersion : VPackage -> Version -> VPackage
changeVersion vPackage version =
    { vPackage | version = version }


changeSummary : VPackage -> String -> VPackage
changeSummary vPackage summary =
    { vPackage | summary = summary }


changeLicense : VPackage -> String -> VPackage
changeLicense vPackage license =
    { vPackage | license = license }


changeRepository : VPackage -> String -> VPackage
changeRepository vPackage repository =
    { vPackage | repository = repository }


addDependencyToModel : VPackage -> Name -> Version -> VPackage
addDependencyToModel ({ dependencies } as vPackage) name version =
    { vPackage | dependencies = addDependencyToVPackage dependencies name version }


addDependencyToVPackage : Dict Name VersionRange -> Name -> Version -> Dict Name VersionRange
addDependencyToVPackage dependencyDict name version =
    if Dict.member name dependencyDict then
        dependencyDict
    else
        Dict.insert name (VersionRange version (Version (version.major + 1) 0 0)) dependencyDict


removeDependencyToModel : VPackage -> Name -> VPackage
removeDependencyToModel ({ dependencies } as vPackage) name =
    { vPackage | dependencies = Dict.remove name dependencies }


updateModuleName : VFile -> ModuleName -> VFile
updateModuleName vFile newName =
    case vFile.moduleDeclaration of
        ModuleDeclaration _ exportSet ->
            { vFile | moduleDeclaration = ModuleDeclaration (formatFirstUpper newName) exportSet }
        PortModuleDeclaration _ exportSet ->
            { vFile | moduleDeclaration = PortModuleDeclaration (formatFirstUpper newName) exportSet }


formatFirstUpper : String -> String
formatFirstUpper string =
    case String.uncons string of
        Just (c, ss) -> onlyLatin <| String.cons (Char.toUpper c) ss
        _ -> string


onlyLatin : String -> String
onlyLatin string =
    Regex.replace Regex.All (Regex.regex "[^a-zA-Z]") (\_ -> "") string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



---- VIEW ----


view : Model -> Html Msg
view  model =
    let
        { vFile
        , showElmPackageModal
        , elmPackageList
        , searchFilter
        , vPackage
        , packageDocs
        , showSettingsModal
        , showImportModal
        } = model
        { moduleDeclaration
        , importStatements
        , typeDeclarations
        , typeAliasDeclarations
        , functionDeclarations
        } = vFile
    in
        Html.div
            [ SAttr.class "app"
            ]
            [ Html.div [ HAttr.class "app__left-pane"
                       ]
                       [ header vPackage packageDocs
                       , moduleView moduleDeclaration
                       , importsView importStatements
                       -- , typeView typeDeclarations TODO
                       -- , typeAliasView typeAliasDeclarations TODO
                       ]
            , modalView
                showImportModal
                (genericModalHeader "Importable Modules" ShowImportModal)
                (importModalBody vPackage)
            , modalView
                showSettingsModal
                (genericModalHeader "Settings" ShowSettingsModal)
                (settingsBody vPackage)
            , modalView
                showElmPackageModal
                (packageListHeader searchFilter)
                (packageListView elmPackageList searchFilter vPackage.dependencies)
            , Svg.svg
                [ SAttr.style "height: 100vh; width: 100vw;"
                , SAttr.class "app__board"
                ]
                [ ]
            -- , Html.p [ HAttr.class "test-output" ] [ Html.text <| toText vFile vPackage ]
            ]


genericModalHeader : String -> (Bool -> Msg) -> Html Msg
genericModalHeader title closeMessage =
    Html.div [ HAttr.class "generic-modal__header" ]
             [ Html.span [ HAttr.class "generic-modal__header__title" ]
                         [ Html.text title ]
             , Html.button [ HAttr.class "generic-modal__header__close-button button--icon"
                           , HEvent.onClick <| closeMessage False
                           ]
                           [ Html.i [ HAttr.class "fas fa-times" ] [] ]
             ]


importModalBody : VPackage -> Html Msg
importModalBody { dependencies } =
    Html.ul [] []


settingsBody : VPackage -> Html Msg
settingsBody { version, summary, repository, license, dependencies } =
    let
        { major, minor, patch } = version
    in
        Html.div [ HAttr.class "settings__body" ]
                 [ Html.div [ HAttr.class "settings__version" ]
                            [ settingsLabel "Version"
                            , Html.input [ HAttr.type_ "number"
                                         , HAttr.step "1"
                                         , HAttr.min "0"
                                         , HAttr.value <| toString major
                                         , HEvent.onInput <| (\v ->
                                                                ChangeVersion { version
                                                                              | major = case String.toInt v of
                                                                                            Ok n -> n
                                                                                            Err _ -> major
                                                                              }
                                                             )
                                         ]
                                         []
                            , Html.input [ HAttr.type_ "number"
                                         , HAttr.step "1"
                                         , HAttr.min "0"
                                         , HAttr.value <| toString minor
                                         , HEvent.onInput <| (\v ->
                                                                ChangeVersion { version
                                                                              | minor = case String.toInt v of
                                                                                            Ok n -> n
                                                                                            Err _ -> minor
                                                                              }
                                                             )
                                         ]
                                         []
                            , Html.input [ HAttr.type_ "number"
                                         , HAttr.step "1"
                                         , HAttr.min "0"
                                         , HAttr.value <| toString patch
                                         , HEvent.onInput <| (\v ->
                                                                ChangeVersion { version
                                                                              | patch = case String.toInt v of
                                                                                            Ok n -> n
                                                                                            Err _ -> patch
                                                                              }
                                                             )
                                         ]
                                         []
                            ]
                 , Html.div [ HAttr.class "settings__summary" ]
                            [ settingsLabel "Summary"
                            , Html.input [ HAttr.value summary
                                         , HAttr.maxlength 80
                                         , HEvent.onInput ChangeSummmary
                                         ]
                                         []
                            ]
                 , Html.div [ HAttr.class "settings__license" ]
                            [ settingsLabel "License"
                            , Html.input [ HAttr.value license
                                         , HEvent.onInput ChangeLicense
                                         ]
                                         []
                            ]
                 , Html.div [ HAttr.class "settings__repository" ]
                            [ settingsLabel "Repository"
                            , Html.input [ HAttr.value repository
                                         , HEvent.onInput ChangeRepository
                                         ]
                                         []
                            ]
                 , Html.div [ HAttr.class "settings__dependencies" ]
                            [ Html.span [ HAttr.class "settings__label" ]
                                        [ Html.text "Dependencies:"
                                        , Html.button [ HAttr.class "button--generic settings__dependencies__add-button"
                                                      , HEvent.onClick RequestElmPackages
                                                      ]
                                                      [ Html.text "Add" ]
                                        ]
                            , Html.ul [ HAttr.class "settings__dependencies__list"
                                      ]
                                      <| List.map dependencyItem (Dict.toList dependencies)
                            ]
                 ]


dependencyItem : (Name, VersionRange) -> Html Msg
dependencyItem (name, { lower, upper }) =
    Html.li [ HAttr.class "settings__dependencies__list__item" ]
            [ Html.button [ HAttr.class "button--generic settings__dependencies__list__item__remove"
                          , HEvent.onClick <| RemoveDependency name
                          , HAttr.disabled (name == "elm-lang/core")
                          ]
                          [ Html.text "Remove" ]
            , Html.text <| name ++ ": " ++ (versionToString lower) ++ "<= v < " ++ (versionToString upper)
            ]


settingsLabel : String -> Html Msg
settingsLabel text =
    Html.span [ HAttr.class "settings__label" ] [ Html.text <| text ++ ":" ]


header : VPackage -> Dict Name (List ElmPackageDoc) ->  Html Msg
header vPackage packageDocs =
    Html.div [ HAttr.class "app__left-pane__settings" ]
             [ Html.button [ HAttr.class "app__left-pane__settings__button button--icon"
                           , HEvent.onClick <| ShowSettingsModal True
                           ]
                           [ Html.i [ HAttr.class "fas fa-cog" ] [] ]
             , Html.span [ HAttr.class "app__left-pane__settings__title" ]
                         [ Html.text "Velma" ]
             ]


modalView : Bool -> Html Msg -> Html Msg -> Html Msg
modalView showModal header child =
    if showModal then
        Html.div [ HAttr.class "modal"
                 ]
                 [ Html.div [ HAttr.class "modal__content" ]
                            [ Html.div [ HAttr.class "modal__content__header"]
                                       [ header ]
                            , Html.div [ HAttr.class "modal__content__body" ]
                                       [ child ]
                            ]
                 ]
    else
        Html.text ""


packageListHeader : String -> Html Msg
packageListHeader searchFilter =
    Html.div [ HAttr.class "package-list__header" ]
             [ Html.span [ HAttr.class "package-list__header__title" ] [ Html.text "Elm Packages" ]
             , packageViewSearch searchFilter
             , Html.button [ HAttr.class "package-list__header__close-button button--icon"
                           , HEvent.onClick <| ShowPackageModal False
                           ]
                           [ Html.i [ HAttr.class "fas fa-times" ] [] ]
             ]


packageListView : List ElmListPackage -> String -> Dict Name VersionRange -> Html Msg
packageListView elmPackages searchFilter addedPackages =
    let
        addedKeys = Dict.keys addedPackages
        availablePackages = List.filter (filterPackageListView searchFilter addedKeys) elmPackages
    in
        Html.ul [ HAttr.class "package-list" ]
                <| if List.length elmPackages > 0 then
                    List.map packageView availablePackages
                else
                    [ Html.text "Loading..." ]


filterPackageListView : String -> List String -> ElmListPackage -> Bool
filterPackageListView searchFilter alreadyAdded { name, summary } =
    (String.contains searchFilter name || String.contains searchFilter summary)
    && (not <| List.member name alreadyAdded)


packageViewSearch : String -> Html Msg
packageViewSearch searchFilter =
    Html.div [ HAttr.class "package-list__header__search" ]
             [ Html.input [ HAttr.placeholder "Search..."
                          , HAttr.class "package-list__search__input"
                          , HAttr.value searchFilter
                          , HEvent.onInput UpdateSearchFilter
                          ]
                          []
             ]


packageView : ElmListPackage -> Html Msg
packageView { name, summary, versions } =
    let
        latestVersion = case List.head versions of
            Just v ->
                versionFromString v
            Nothing ->
                Version 1 0 0
    in
        Html.li [ HAttr.class "package-list__package" ]
                [ Html.button [ HAttr.class "package-list__package__add button--generic"
                              , HEvent.onClick <| AddDependency name latestVersion
                              ]
                              [ Html.text "Add" ]
                , Html.div [ HAttr.class "package-list__package__details" ]
                           [ Html.span [ HAttr.class "package-list__package__details__top" ]
                                       [ Html.select [ HAttr.class "package-list__package__details__version" ]
                                                     <| List.map (\v -> Html.option [ HAttr.value v ] [ Html.text v ]) versions
                                       , Html.a [ HAttr.href <| "http://package.elm-lang.org/packages/" ++ name ++ "/latest"
                                                , HAttr.target "_blank"
                                                ]
                                                [ Html.text name ]
                                       ]
                           , Html.span [] [ Html.text summary ]
                           ]
                ]


moduleView : StatementModule -> Html Msg
moduleView vModule =
    Html.label [ HAttr.class "app__left-pane__module" ]
               [ Html.text "Module Name:"
               , case vModule of
                    ModuleDeclaration moduleName exportSet ->
                        Html.input [ HAttr.value moduleName
                                   , HEvent.onInput ModuleChangeName
                                   ] []
                    PortModuleDeclaration moduleName exportSet ->
                        Html.input [ HAttr.value moduleName
                                   ] []
               ]


importsView : List StatementImport -> Html Msg
importsView importStatements =
    Html.ul [ HAttr.class "app__left-pane__imports" ]
            <| newImportView :: List.map importView importStatements


newImportView : Html Msg
newImportView =
    Html.li [ HAttr.class "app__left-pane__imports__add-new" ]
            [ Html.button [ HAttr.class "button--generic"
                          , HEvent.onClick <| ShowImportModal True
                          ]
                          [ Html.text "Add Import" ]
            ]


importView : StatementImport -> Html Msg
importView importStatement =
    case importStatement of
        StatementImport moduleName exportSetMaybe ->
            Html.li [ HAttr.class "app__left-pane__imports__import" ]
                    [ Html.text moduleName
                    , importExportsView exportSetMaybe
                    ]


importExportsView : Maybe ExportSet -> Html Msg
importExportsView exportSetMaybe =
    case exportSetMaybe of
          Nothing ->
              Html.text ""
          Just AllExport ->
              Html.text "(..)"
          Just (SubsetExport exportList) -> -- TODO
              Html.text ""
          Just (FunctionExport name) ->
              Html.text <| "(" ++ name ++ ")"
          Just (TypeExport name maybeExportSet) ->
              Html.text <| "(" ++ name ++  ")" -- TODO


-- vFunctionView : VFunction -> Svg Msg
-- vFunctionView { id, position, clicked } =
--     let
--         color =
--             if clicked then
--                 "#ffcd4e"
--             else
--                 "#5ef1f6"
--     in
--         Svg.rect
--             [ num SAttr.width <| getX vFunctionSize
--             , num SAttr.height <| getY vFunctionSize
--             , num SAttr.x (getX position)
--             , num SAttr.y (getY position)
--             , SAttr.fill color
--             , SAttr.stroke "#1d1d1d"
--             , SAttr.cursor "move"
--             , Draggable.mouseTrigger id DragMsg
--             , onMouseUp StopDragging
--             ]
--             []



---- OUTPUTING TO TEXT ----


-- toText : VFile -> VPackage -> String
-- toText vFile vPackage =
--     vPackage
--
--
-- vModuleToText : Bool -> VModule -> String
-- vModuleToText hasPorts { name } =
--     "module " ++ name ++ " exposing (..)\n\n"
--
--
-- vImportsToText : List VImport -> String
-- vImportsToText vImports =
--     List.foldl vImportToText "" vImports
--
--
-- vImportToText : VImport -> String -> String
-- vImportToText { name } previousVImports =
--     previousVImports ++ "import " ++ name ++ "\n"


---- EXTERNAL ----


getPackages : Cmd Msg
getPackages =
  let
    url =
      "http://package.elm-lang.org/all-packages"
  in
    Http.send ReceiveElmPackageList requestPackages


requestPackages : Http.Request (List ElmListPackage)
requestPackages =
    Http.request
        { method = "GET"
        , headers = []
        , url = "http://package.elm-lang.org/all-packages"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeElmPackageList
        , timeout = Nothing
        , withCredentials = False
        }


decodeElmPackageList : JD.Decoder (List ElmListPackage)
decodeElmPackageList =
    JD.list
        (JD.map3 ElmListPackage
            (JD.field "name" JD.string)
            (JD.field "summary" JD.string)
            (JD.field "versions" (JD.list JD.string))
        )


getPackageDocs : Name -> Version -> Cmd Msg
getPackageDocs name version =
    let
        versionString = versionToString version
        url = "http://package.elm-lang.org/packages/" ++ name ++ "/" ++ versionString ++ "/documentation.json"
    in
        Http.send (ReceiveElmPackageDocs name) (Http.get url decodePackageDocs)


decodePackageDocs : JD.Decoder (List ElmPackageDoc)
decodePackageDocs =
    JD.list
        (JD.map5 ElmPackageDoc
            (JD.field "aliases"
                (JD.list
                    (JD.map4 ElmPackageAlias
                        (JD.field "args" (JD.list JD.string))
                        (JD.field "comment" JD.string)
                        (JD.field "name" JD.string)
                        (JD.field "type" JD.string)
                    )
                )
            )
            (JD.field "comment" JD.string)
            (JD.field "name" JD.string)
            (JD.field "types"
                (JD.list
                    (JD.map3 ElmPackageType
                        (JD.field "args" (JD.list JD.string))
                        -- (JD.field "cases" (JD.list JD.string)) TODO
                        (JD.field "comment" JD.string)
                        (JD.field "name" JD.string)
                    )
                )
            )
            (JD.field "values"
                (JD.list
                    (JD.map3 ElmPackageValue
                        (JD.field "comment" JD.string)
                        (JD.field "name" JD.string)
                        (JD.field "type" JD.string)
                    )
                )
            )
        )
