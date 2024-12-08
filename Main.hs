-- Predefined credentials for user and coach
validUserEmail :: String
validUserEmail = "user@fitgym.com"

validUserPassword :: String
validUserPassword = "user123"

validCoachEmail :: String
validCoachEmail = "coach@fitgym.com"

validCoachPassword :: String
validCoachPassword = "coach123"

-- Main function to display the login menu and get user choice
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

-- Function to handle user login with validation
loginAsUser :: IO ()
loginAsUser = do
    putStrLn "\nUser Login"
    putStr "Enter your email: "
    email <- getLine
    putStr "Enter your password: "
    password <- getLine
    if email == validUserEmail && password == validUserPassword
        then putStrLn "\nWelcome! You have successfully logged in.\n"
        else do
            putStrLn "\nInvalid email or password. Please try again.\n"
            loginAsUser

-- Function to handle coach login with validation
loginAsCoach :: IO ()
loginAsCoach = do
    putStrLn "\nCoach Login"
    putStr "Enter your email: "
    email <- getLine
    putStr "Enter your password: "
    password <- getLine
    if email == validCoachEmail && password == validCoachPassword
        then putStrLn "\nWelcome, Coach! You have successfully logged in.\n"
        else do
            putStrLn "\nInvalid email or password. Please try again.\n"
            loginAsCoach

main = do
       choice <- getChoice
       case choice of
            1 -> loginAsUser
            2 -> loginAsCoach
            3 -> putStrLn "Exiting"
            _ -> do
              putStrLn "Invalid choice. Please try again.\n"
              main

