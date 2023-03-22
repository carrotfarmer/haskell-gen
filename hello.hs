-- body_ :: String -> String
-- body_ = el "body"

-- head_ :: String -> String
-- head_ = el "head"

-- title_ :: String -> String
-- title_ = el "title"

-- makeHtml :: String -> String -> String
-- makeHtml title content = html_

import Html

myhtml :: Html
myhtml =
  html_
    "hello page"
    ( append_
        (h1_ "Hello, world!")
        ( append_
            (p_ "bruh this language on crack")
            (p_ "boht kri aa raha hai")
        )
    )

main :: IO ()
main = putStrLn (render myhtml)
