module Types exposing
    -- MESSAGES
    ( Msg(..)
    -- TYPES
    , ModuleName
    , PackageName
    , Name
    , Comment
    , QualifiedType
    , ExportSet(..)
    , FunctionTypeDeclaration(..)
    , FunctionImplementation(..)
    , Type(..)
    , Expression(..)
    , StatementModule(..)
    , StatementImport(..)
    , TypeDeclaration(..)
    , TypeAliasDeclaration(..)
    -- TYPE ALIASES
    , Version
    , VersionRange
    , ElmPackageAlias
    , ElmPackageType
    , ElmPackageValue
    , ElmPackageDoc
    , ElmListPackage
    , FunctionDeclaration
    , VFile
    , VPackage
    , TypeForm
    , Model
    -- HELPERS
    , versionToString
    , versionFromString
    , defaultVPackage
    , makeVFile
    )


import Dict exposing (Dict)
import Http
import Set exposing (Set)


---- MESSAGES ----


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
    | AddImport ModuleName PackageName
    | RemoveImport ModuleName
    | ShowNewTypeModal Bool
    | NewTypeChangeName Name
    -- | NewTypeAddVariable Name
    -- | NewTypeChangeVariable Name
    -- | NewTypeRemoveVariable Name
    -- | NewTypeAddConstructor Name Type
    -- | NewTypeRemoveConstructor Name



---- TYPES & ALIASES ----


type alias ModuleName =
    String


type alias PackageName =
    String


type alias Name =
    String


type alias Comment =
    String


type alias QualifiedType =
    String


type ExportSet
    = AllExport
    | SubsetExport (List ExportSet)
    | FunctionExport Name
    | TypeExport Name (Maybe ExportSet)


type FunctionTypeDeclaration
    = Type


type FunctionImplementation
    = FunctionImplementation (List Expression) Expression


{-| Representations for Elm's type syntax.
-}
type Type
    = TypeConstructor QualifiedType (List Type)
    | TypeVariable Name
    | TypeRecordConstructor Type (List ( Name, Type ))
    | TypeRecord (List ( Name, Type ))
    | TypeTuple (List Type)
    | TypeApplication Type Type


{-| Representations for Elm's expressions.
-}
type Expression
    = Character Char
    | String String
    | Integer Int
    | Float Float
    | Variable (List Name)
    | List (List Expression)
    | Tuple (List Expression)
    | Access Expression (List Name)
    | AccessFunction Name
    | Record (List ( Name, Expression ))
    | RecordUpdate Name (List ( Name, Expression ))
    | If Expression Expression Expression
    | Let (List ( Expression, Expression )) Expression
    | Case Expression (List ( Expression, Expression ))
    | Lambda (List Expression) Expression
    | Application Expression Expression
    | BinOp Expression Expression Expression


type StatementModule
    = ModuleDeclaration ModuleName ExportSet
    | PortModuleDeclaration ModuleName ExportSet
    -- | EffectModuleDeclaration ModuleName (List ( Name, Name )) ExportSet -- TODO: Maybe


type StatementImport
    = StatementImport ModuleName PackageName (Maybe ExportSet)


type TypeDeclaration
    = TypeDeclaration Type (List Type)


type TypeAliasDeclaration
    = TypeAliasDeclaration Type Type


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


type alias VersionRange =
    { lower : Version
    , upper : Version
    }


type alias ElmPackageAlias =
    { args : List String
    , comment : String
    , name : Name
    , aliasType : String
    }


type alias ElmPackageType =
    { args : List String
    -- , cases : List String
    , comment : String
    , name : Name
    }


type alias ElmPackageValue =
    { comment : String
    , name : Name
    , valueType : String
    }


type alias ElmPackageDoc =
    { aliases : List ElmPackageAlias
    , comment : String
    -- , generatedWithElmVersion : Version
    , name : Name
    , types : List ElmPackageType
    , values : List ElmPackageValue
    }


type alias ElmListPackage =
    { name : String
    , summary : String
    , versions : List String
    }


type alias FunctionDeclaration =
    { name : Name
    , typeDeclaration : FunctionTypeDeclaration
    , implementation : FunctionImplementation
    }


type alias VFile =
    { moduleDeclaration : StatementModule
    , importStatements : Dict ModuleName StatementImport
    , typeDeclarations : Dict Name TypeDeclaration
    , typeAliasDeclarations : List TypeAliasDeclaration
    , functionDeclarations : List FunctionDeclaration
    }


type alias VPackage =
    { version : Version
    , summary : String
    , repository : String
    , license : String
    , dependencies : Dict Name VersionRange
    }


type alias TypeForm =
    { name : Name
    , typeVariables : Set Name
    , constructors : Dict Name Type
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
    , showNewTypeModal : Bool
    , newTypeForm : TypeForm
    }



---- HELPERS ----


versionToString : Version -> String
versionToString { major, minor, patch } =
    (toString major) ++ "." ++ (toString minor) ++ "." ++ (toString patch)


versionFromString : String -> Version
versionFromString s =
    let
        parts = String.split "." s
        ( major, minor, patch ) = case listStringToInt parts of
            [ ma, mi, p ] ->
                (ma, mi, p)
            _ ->
                (1, 0, 0)

    in
        { major = major, minor = minor, patch = patch }


listStringToInt : List String -> List Int
listStringToInt l =
    List.map (\s -> case String.toInt s of
                        Ok i -> i
                        Err _ -> 0
             )
             l


defaultVPackage : VPackage
defaultVPackage =
    { version = Version 1 0 0
    , summary = "helpful summary of your project, less than 80 characters"
    , repository = "https://github.com/user/project.git"
    , license = "BSD3"
    , dependencies = Dict.singleton "elm-lang/core" { lower = Version 5 1 1
                                                    , upper = (Version 6 0 0)
                                                    }
    }


makeVFile :
    StatementModule ->
    Dict ModuleName StatementImport ->
    Dict QualifiedType TypeDeclaration ->
    List TypeAliasDeclaration ->
    List FunctionDeclaration ->
    VFile
makeVFile vModule vImports typeDeclarations typeAliasDeclarations functionDeclarations =
    VFile vModule vImports typeDeclarations typeAliasDeclarations functionDeclarations
