import System.IO

main = do x <- getLine
          if (last x) == 'a' || (last x) == 'A'
            then putStrLn "Hola maca!"
            else putStrLn "Hola maco!"