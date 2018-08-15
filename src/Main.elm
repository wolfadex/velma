module Main exposing (..)


import Dict exposing (Dict)
import External exposing (getPackageDocs)
import Html exposing (Html)
import Html.Attributes as HAttr
import Html.Events as HEvent
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Types exposing (..)
import Update exposing (update)



---- MAIN ----


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { vFile = makeVFile (ModuleDeclaration "TestModule" AllExport) Dict.empty Dict.empty [] []
      , elmPackageList = []
      , showElmPackageModal = False
      , searchFilter = ""
      , vPackage = defaultVPackage
      , packageDocs = Dict.empty
      , showSettingsModal = False
      , showImportModal = False
      , showNewTypeModal = False
      , newTypeForm = TypeForm "" Set.empty Dict.empty
      }
    , Cmd.batch [ getPackageDocs "elm-lang/core" (Version 5 1 1)
                ]
    )


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
        , showNewTypeModal
        , newTypeForm
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
                       , typesView typeDeclarations
                       -- , typeAliasView typeAliasDeclarations TODO
                       ]
            , modalView
                showImportModal
                (genericModalHeader "Importable Modules" ShowImportModal)
                (importModalBody packageDocs importStatements)
            , modalView
                showNewTypeModal
                (genericModalHeader "New Type" ShowNewTypeModal)
                (newTypeModal newTypeForm)
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


importModalBody : Dict Name (List ElmPackageDoc) -> Dict ModuleName StatementImport -> Html Msg
importModalBody packageDocs importStatements =
    Html.ul [ HAttr.class "import-modal__list" ]
            <| List.map importPackageListView (Dict.toList packageDocs)


importPackageListView : (Name, List ElmPackageDoc) -> Html Msg
importPackageListView (name, packageDocs) =
    Html.li [ HAttr.class "import-modal__list__package" ]
            [ Html.div [ HAttr.class "import-modal__list__package__title" ]
                       [ Html.text <| String.join " / " <| String.split "/" name ]
            , Html.ul [ HAttr.class "import-modal__list__package__list" ]
                      <| List.map (importModuleListView name) <| List.sortBy (\{ name } -> name) packageDocs
            ]


importModuleListView : Name -> ElmPackageDoc -> Html Msg
importModuleListView packageName { name } =
    Html.li [ HAttr.class "import-modal__list__package__list__module" ]
            [ Html.button [ HAttr.class "button--generic import-modal__list__package__list__module__import-button"
                          , HEvent.onClick <| AddImport name packageName
                          ]
                          [ Html.text "Import" ]
            , Html.text name
            ]


newTypeModal : TypeForm -> Html Msg
newTypeModal { name, typeVariables, constructors } =
    Html.div []
             [ Html.div []
                        [ Html.label [] [ Html.text "Result:" ]
                        , Html.p [] [ Html.text "type Name a b\n    = Name a\n    | Name b" ]
                        ]
             , Html.div []
                        [ Html.label [] [ Html.text "Name:" ]
                        , Html.input [ HAttr.placeholder "Unique Name"
                                     , HAttr.value name
                                     , HEvent.onInput NewTypeChangeName
                                     ]
                                     []
                        ]
             , Html.div []
                        [ Html.label [] [ Html.text "Variables:" ]
                        , Html.ul []
                                  <| Html.li []
                                             [ Html.button [ HAttr.class "button--generic" ]
                                                           [ Html.text "Add Variable" ]
                                             ]
                                  :: List.map newTypeVariables []
                        ]
             ]


newTypeVariables : a -> Html Msg
newTypeVariables a =
    Html.li []
            [ Html.button [ HAttr.class "button--generic" ]
                          [ Html.text "Remove" ]
            , Html.input [ HAttr.placeholder "uniqueVariableName" ] []
            ]


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


importsView : Dict ModuleName StatementImport -> Html Msg
importsView importStatements =
    Html.ul [ HAttr.class "app__left-pane__imports" ]
            <| newImportView :: List.map importView (Dict.toList importStatements)


newImportView : Html Msg
newImportView =
    Html.li [ HAttr.class "app__left-pane__imports__add-new" ]
            [ Html.button [ HAttr.class "button--generic"
                          , HEvent.onClick <| ShowImportModal True
                          ]
                          [ Html.text "Add Import" ]
            ]


importView : (ModuleName, StatementImport) -> Html Msg
importView (_, importStatement) =
    case importStatement of
        StatementImport moduleName _ exportSetMaybe ->
            Html.li [ HAttr.class "app__left-pane__imports__import" ]
                    [ Html.button [ HAttr.class "button--generic app__left-pane__imports__import__remove-button"
                                  , HEvent.onClick <| RemoveImport moduleName
                                  ]
                                  [ Html.text "Remove" ]
                    , Html.text moduleName
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


typesView : Dict QualifiedType TypeDeclaration -> Html Msg
typesView typeDeclarations =
    Html.ul [ HAttr.class "types-view" ]
            <| newTypeView :: List.map typeView (Dict.toList typeDeclarations)


newTypeView : Html Msg
newTypeView =
    Html.li []
            [ Html.button [ HAttr.class "button--generic types-view__new-button"
                          , HEvent.onClick <| ShowNewTypeModal True
                          ]
                          [ Html.text "Create New Type" ]
            ]


typeView : (QualifiedType, TypeDeclaration) -> Html Msg
typeView (_, typeDeclaration) =
    case typeDeclaration of
        TypeDeclaration t tList ->
            Html.li [] [ Html.text "Type Stuff Here" ]


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
