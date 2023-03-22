module Html.Internal where

import Numeric.Natural

newtype Html = Html String

-- newtype Structure = Structure String

type Document =
  [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

type Title = String

-- * Render
render :: Html -> String
render html =
  case html of
    Html str -> str

-- * EDSL
html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concatMap (el "li" . getStructureString)

-- * Examples

-- Hello, world!
example1 :: Document
example1 =
  [ Paragraph "Hello, world!"
  ]

-- * Welcome

-- To this tutorial about Haskell.
example2 :: Document
example2 =
  [ Heading 1 "Welcome"
  , Paragraph "To this tutorial about Haskell."
  ]

-- Remember that multiple lines with no separation
-- are grouped together to a single paragraph
-- but list items remain separate.

-- # Item 1 of a list
-- # Item 2 of the same list

example3 =
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , OrderedList
      [ "Item 1 of a list"
      , "Item 2 of the same list"
      ]
  ]

-- * Compiling programs with ghc

-- Running ghc invokes the Glasgow Haskell Compiler (GHC),
-- and can be used to compile Haskell modules and programs into native
-- executables and libraries.

-- Create a new Haskell source file named hello.hs, and write
-- the following code in it:

-- > main = putStrLn "Hello, Haskell!"

-- Now, we can compile the program by invoking ghc with the file name:

-- > ➜ ghc hello.hs
-- > [1 of 1] Compiling Main             ( hello.hs, hello.o )
-- > Linking hello ...

-- GHC created the following files:

-- - hello.hi - Haskell interface file
-- - hello.o - Object file, the output of the compiler before linking
-- - hello (or hello.exe on Microsoft Windows) - A native runnable executable.

-- GHC will produce an executable when the source file satisfies both conditions:

-- # Defines the main function in the source file
-- # Defines the module name to be Main, or does not have a module declaration

-- Otherwise, it will only produce the .o and .hi files.

example4 =
  [ Heading 1 "Compiling programs with ghc"
  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
  , CodeBlock "main = putStrLn \"Hello, Haskell!\""
  , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
  , CodeBlock
      [ "➜ ghc hello.hs"
      , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
      , "Linking hello ..."
      ]
  , Paragraph "GHC created the following files:"
  , UnorderedList
      [ "hello.hi - Haskell interface file"
      , "hello.o - Object file, the output of the compiler before linking"
      , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
      ]
  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
  , OrderedList
      [ "Defines the main function in the source file"
      , "Defines the module name to be Main, or does not have a module declaration"
      ]
  , Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]

-- * Utils
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
   in
    concatMap escapeChar

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
   in
    case txts of
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then paragraph : parseLines [] rest
          else parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words

print :: Show a => a -> IO ()
print = putStrLn . show
