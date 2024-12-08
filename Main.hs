-- Predefined credentials for user and coach
validEmail :: [String]
validEmail = ["user@fitgym.com", "coach@fitgym.com"]

validPassword :: [String]
validPassword = ["user123", "coach123"]

-- Function to perform login
performLogin :: (String -> String -> Maybe String) -> String -> IO()
performLogin validCredentials userType = do
       putStrLn "\n***************************************"
       putStrLn "     Welcome to the FitGym System      "
       putStrLn "***************************************"
       putStr "Enter your email: "
       email <- getLine
       putStr "Enter your password: "
       password <- getLine
       -- Apply the validation function and handle the result using a Functor
       case validCredentials email password of
              Just _ -> putStrLn ("Login successful. Welcome " ++ userType ++ "!")
              Nothing -> do
                     putStrLn "Invalid credentials. Please try again."
                     performLogin validCredentials userType
       

validCredentials :: String -> String -> Maybe String
validCredentials email password = 
       -- Zip the emaill and password lists and lookup the email
       case lookup email (zip validEmail validPassword) of
              Just correctPassword | correctPassword == password -> Just email
              _ -> Nothing

-- Function to display the login menu and get user choice
getChoice :: IO Int
getChoice = do
    putStrLn "***************************************"
    putStrLn "     Welcome to the FitGym System      "
    putStrLn "***************************************"
    putStrLn "1. Login"
    putStrLn "2. Sign Up"
    putStrLn "3. Exit"
    putStr "Enter your choice: "
    choice <- getLine
    return (read choice :: Int)

-- Main function
main :: IO ()
main = do
    choice <- getChoice
    case choice of
        1 -> performLogin validCredentials "User"
        2 -> performLogin validCredentials "Coach"
        3 -> putStrLn "Exiting the system. Goodbye!"
        _ -> do
            putStrLn "Invalid choice. Please try again.\n"
            main -- Recursively call main to allow another attempt
