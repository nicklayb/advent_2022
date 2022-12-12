module Main exposing (..)

import Console
import Debug
import Platform

type alias Model = { input: String }

type Msg =
    Run

update msg model = 
  case msg of
    Run -> (model, Console.log (run model.input))

run : String -> String
run input =
    input

start : Cmd Msg
start =
  Cmd.batch [Console.send Run]

init : Console.Flags -> (Model, Cmd Msg)
init { input } =
    ( { input = input }, start )

main = Platform.worker 
  { init = init
  , update = update
  , subscriptions = (\_ -> Sub.none)
  }
