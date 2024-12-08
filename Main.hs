-- Predefined credentials for user and coach
validUserEmail :: String
validUserEmail = "user@fitgym.com"

validUserPassword :: String
validUserPassword = "user123"

validCoachEmail :: String
validCoachEmail = "coach@fitgym.com"

validCoachPassword :: String
validCoachPassword = "coach123"

-- Function to perform login
performLogin :: (String -> String -> Bool) -> String -> IO()
performLogin validCredentials userType = do
       putStrLn "\n***************************************"
       putStrLn "     Welcome to the FitGym System      "
       putStrLn "***************************************"
       putStr "Enter your email: "
       email <- getLine
       putStr "Enter your password: "
       password <- getLine
       if validCredentials email password
              then putStrLn ("Login successful. Welcome " ++ userType ++ "!")
              else do 
                     putStrLn "Invalid credentials. Please try again."
                     performLogin validCredentials userType

validCredentials :: String -> String -> Bool
validCredentials email password
       | email == validUserEmail && password == validUserPassword = True
       | email == validCoachEmail && password == validCoachPassword = True
       | otherwise = False

-- Function to display the login menu and get user choice
getChoice :: IO Int
getChoice = do
    putStrLn "***************************************"
    putStrLn "     Welcome to the FitGym System      "
    putStrLn "***************************************"
    putStrLn "1. Login as User"
    putStrLn "2. Login as Coach"
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
