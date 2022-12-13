module FileSystem exposing (parse)

import FileSystem.Parser as Parser exposing (Instruction(..), FileEntry(..), Name, Size)

type FileItem 
    = Empty
    | File Name
    | Directory Name (Dict Name (List FileItem))

type Command
    = Cd Name
    | Ls Name
    | LsItem FileEntry

type alias Model = 
    { fileSystem : FileItem
    , commands : List Instruction
    , workingDirectory : List String
    }

init : Model
init =
    { fileSystem = Empty, commands = [], workingDirectory = [] }

parse : String -> FileItem
parse input =
    init
    |> parseCommands input
    |> buildTree

buildTree : Model -> FileItem
buildTree model =
    File "Test"

parseCommands : String -> Model -> Model
parseCommands commandsFile model =
    commandsFile
    |> Parser.parse
    |> Result.withDefault []
    |> (\instructions -> { model | commands = instructions })
    |> buildTree
    |> Debug.log "Parsed"

buildTree : Model -> Model
buildTree model =
    model.operations
    |> applyCommands model.fileSystem
    |> (\fileSystem -> { model | fileSystem = fileSystem })

applyCommands : ListInstruction -> Model -> Model
applyCommands instructions model =
    List.foldl applyCommand model model

applyCommand : Instruction -> Model -> Model
applyCommand instruction model =
    case instruction of
        ChangeDirectory directory ->
            changeDirectory directory model

        ListDirectory entries ->
            listDirectory entries model

changeDirectory : String -> Model -> Model
changeDirectory path model =
    { model | workingDirectory = List.append path model.workingDirectory }

listDirectory : List (FileEntry) -> Model -> Model
listDirectory entries model =


updateCurrentDirectory : (FileEntry -> FileEntry) -> Model -> Model
updateCurrentDirectory func model =
    let
        updateAt fileSystem directory =
            case directory of
                [] -> func fileSystem

                next::rest ->
                    Dict.update next (updateDict rest) fileSystem

    in
    updateAt model.fileSystem model.workingDirectory

