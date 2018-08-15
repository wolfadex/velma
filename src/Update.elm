module Update exposing (update)


import Char
import Debug exposing (log)
import Dict exposing (Dict)
import External exposing (getPackages, getPackageDocs)
import Regex
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ vFile, elmPackageList, vPackage, packageDocs, newTypeForm } as model) =
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
        AddImport moduleName packageName ->
            ( { model | vFile = addImport vFile moduleName packageName }, Cmd.none )
        RemoveImport moduleName ->
            ( { model | vFile = removeImport vFile moduleName }, Cmd.none )
        ShowNewTypeModal newShow ->
            ( { model | showNewTypeModal = newShow }, Cmd.none )
        NewTypeChangeName name ->
            ( { model | newTypeForm = changeNewTypeName newTypeForm name }, Cmd.none )


changeNewTypeName : TypeForm -> Name -> TypeForm
changeNewTypeName typeForm name =
    { typeForm | name = formatFirstUpper name }


addImport : VFile -> ModuleName -> PackageName -> VFile
addImport ({ importStatements } as vFile) moduleName packageName =
    { vFile | importStatements = Dict.insert moduleName (StatementImport moduleName packageName Nothing) importStatements}


removeImport : VFile -> ModuleName -> VFile
removeImport ({ importStatements } as vFile) moduleName =
    { vFile | importStatements = Dict.remove moduleName importStatements }


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
