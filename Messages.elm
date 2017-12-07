module Messages exposing (..)
import Navigation

type Msg
    = UrlChange Navigation.Location
    | Receive String
    | PageFilter String


