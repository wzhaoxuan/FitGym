-- Predefined credentials for user and coach
validEmail :: [String]
validEmail = ["user@fitgym.com", "coach@fitgym.com"]

validPassword :: [String]
validPassword = ["user123", "coach123"]

-- Define the Email type
type Email = String
-- Define the UserType data type
data UserType = User Email | Coach Email

-- Function to perform login
performLogin :: (String -> String -> Maybe UserType) -> IO()
performLogin validCredentials = do
       putStrLn "\n***************************************"
       putStrLn "     Welcome to the FitGym System      "
       putStrLn "***************************************"
       putStr "Enter your email: "
       email <- getLine
       putStr "Enter your password: "
       password <- getLine
       -- Apply the validation function and handle the result using a Functor
       case validCredentials email password of
              Just userType -> putStrLn ("Login successful. Welcome " ++ showUserType userType ++ "!")
              Nothing -> do
                     putStrLn "Invalid credentials. Please try again."
                     performLogin validCredentials 
       
-- Function to display the Usertype 
showUserType :: UserType -> String
showUserType (User _)= "User"
showUserType (Coach _)= "Coach"

-- Validate credentials and return the appropriate Usertype
validCredentials :: String -> String -> Maybe UserType
validCredentials email password = 
       -- Zip the email and password lists and lookup the email
       case lookup email (zip validEmail validPassword) of
              Just correctPassword | correctPassword == password -> 
                     -- Determine whether the user is a User or Coach
                     if email == "user@fitgym.com" 
                     then Just (User email)  -- Return User type for regular user
                     else Just (Coach email)  -- Return Coach type for coach
              _ -> Nothing

-- Function to display the login menu and get user choice
getChoice :: IO Int
getChoice = do
    putStrLn "***************************************"
    putStrLn "     Welcome to the FitGym System      "
    putStrLn "***************************************"
    putStrLn "1. Login"
    putStrLn "2. Exit"
    putStr "Enter your choice: "
    choice <- getLine
    return (read choice :: Int)

-- Main function
main :: IO ()
main = do
    choice <- getChoice
    case choice of
        1 -> performLogin validCredentials
        2 -> putStrLn "Exiting the system. Goodbye!"
        _ -> do
            putStrLn "Invalid choice. Please try again.\n"
            main -- Recursively call main to allow another attempt
