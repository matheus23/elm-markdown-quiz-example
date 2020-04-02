module Types exposing (Model, Msg(..))

import Set exposing (Set)


type alias Model =
    { revealedQuizzes : Set String }


type Msg
    = RevealQuiz String
    | NoOp
