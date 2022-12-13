module FileSystem.Parser exposing (Instruction(..), FileEntry(..), Name, Size, parse)

import Parser exposing (..)
import Set

type alias Name = String
type alias Size = Int

type FileEntry
    = ListedFile Size Name
    | ListedDir Name

type Instruction
    = ChangeDirectory Name
    | ListDirectory (List FileEntry)


parse : String -> Result (List DeadEnd) (List Instruction)
parse =
    run parser

parser : Parser (List Instruction)
parser =
    loop [] instructionParser

instructionParser : List Instruction -> Parser (Step (List Instruction) (List Instruction))
instructionParser acc =
    oneOf
        [ succeed (\item -> Loop (item::acc))
            |. symbol "$"
            |. spaces
            |= oneOf
                [ succeed identity
                    |= changeDirectoryParser
                , succeed identity
                    |= listDirectoryParser
                ]
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse acc))
        ]

changeDirectoryParser : Parser Instruction
changeDirectoryParser =
    succeed ChangeDirectory
        |. keyword "cd"
        |. spaces
        |= nameParser

listDirectoryParser : Parser Instruction
listDirectoryParser =
    succeed ListDirectory
        |. keyword "ls"
        |. spaces
        |= loop [] fileContentParser

fileContentParser : List FileEntry -> Parser (Step (List FileEntry) (List FileEntry))
fileContentParser acc =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (\file -> Loop (file::acc))
                |= listedFileParser
            , succeed (\file -> Loop (file::acc))
                |= listedDirParser
            , succeed ()
                |> map (\_ -> Done (List.reverse acc))
            ]

listedFileParser : Parser FileEntry
listedFileParser =
    succeed ListedFile
        |= int
        |. spaces
        |= nameParser

listedDirParser : Parser FileEntry
listedDirParser =
    succeed ListedDir
        |. keyword "dir"
        |. spaces
        |= nameParser

nameParser : Parser String
nameParser =
    variable
        { start = isValidInFileName
        , inner = isValidInFileName
        , reserved = Set.empty
        }

isValidInFileName : Char -> Bool
isValidInFileName char =
    Char.isAlphaNum char || List.member char ['.', '/']
