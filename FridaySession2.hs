main :: IO ()
main = do
  putStr "Your First Name: "
  fname <- getLine
  putStr "Your Last Name: "
  lname <- getLine
  putStr "Your age: "
  age <- readLn -- 
  putStrLn $ greeting fname lname age

greeting :: String -> String -> Int -> String
greeting fname lname age =
  "Hello, " ++ fname ++ " " ++ lname ++ " (" ++ show age ++ ")!"
