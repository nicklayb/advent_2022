port module Console exposing (Flags, log, send)

import Task

port log : String -> Cmd msg

send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity

type alias Flags = { input: String }
