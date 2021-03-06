import System.IO

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

hangman :: IO ()
hangman = do putStr "Enter a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '*'
                    xs <- sgetLine
                    return (x:xs)

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                   putStrLn "You got it!"
                else
                   do putStrLn (diff word xs)
                      guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c 
