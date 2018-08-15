module External exposing (getPackages, getPackageDocs)


import Json.Decode as JD
import Http
import Types exposing (..)


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
