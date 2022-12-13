module FileSystemRaw exposing (findSmallFolders)

import Regex

type alias Name = String
type alias Size = Int

type FileItem 
    = File Name
    | Directory Name (List FileItem)

type FileEntry
    = ListedFile Name Size
    | ListedDir Name

type Command
    = Cd Name
    | Ls Name
    | LsItem LsEntry

type Instruction
    = ChangeDirectory Name
    | ListDirectory Name (List FileEntry)

type alias Model = 
    { fileSystem : FileItem
    , commands : List Instruction
    }

init : FileItem
init =
    Directory "/" []

parse : String -> FileItem
parse input =
    init
    |> parseCommands (String.split "\n" input)
    |> buildTree

parseCommands : List String -> Model -> Model
parseCommands commands model =
    model
    |> List.map parseCommand commands
    |> aggregateCommands

parseCommand : String -> Command
parseCommand rawCommand =
    let
        parseInteractiveCommand cmd =

    in
    if String.startsWith "$" rawCommand then
        parseInteractiveCommand rawCommand

    else
        parseFileItem rawCommand

buildTree : Model -> Model
buildTree model =
    model

findRegex : Maybe Regex.Regex -> String -> List Regex.Match
findRegex regex string =
    regex
    |> Maybe.map (Regex.find

cdRegex : Maybe Regex.Regex
cdRegex =
    Regex.fromString "\$ cd (.+)"

lsRegex : Maybe Regex.Regex
lsRegex =
    Regex.fromString "\$ ls"

dirItemRegex : Maybe Regex.Regex
dirItemRegex =
    Regex.fromString "dir (.+)"

fileItemRegex : Maybe Regex.Regex
fileItemRegex =
    Regex.fromString "(\d+) (.+)"
