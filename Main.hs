-- Predefined credentials for user and coach
--User
validUserEmail :: [String]
validUserEmail = ["wzhao@fitgym.com", "zhengtan@fitgym.com"]
validUserPassword :: [String]
validUserPassword = ["wz123", "zt123"]
--Coach
validCoachEmail :: [String]
validCoachEmail = ["Jane@fitgym.com", "Jack@fitgym.com"]
validCoachPassword :: [String]
validCoachPassword = ["Jane123", "Jack123"]

-- Define the Email type
type Email = String
type Password = String

-- Define the UserType data type
data UserType = User Email Password | Coach Email Password deriving (Show)

-- Define a class for loginable types
class Loginable a where
    login :: a -> String -> String -> Maybe String

-- Implement the Loginable instance for UserType
instance Loginable UserType where
       login (User email password) enteredEmail enteredPassword
        | email == enteredEmail && password == enteredPassword = Just $ "Login successful. Welcome user: " ++ email ++ "!"
        | otherwise = Nothing
       login (Coach email password) enteredEmail enteredPassword
        | email == enteredEmail && password == enteredPassword = Just $ "Login successful. Welcome coach: " ++ email ++ "!"
        | otherwise = Nothing

-- Validate credentials and return the appropriate Usertype
validCredentials :: String -> String -> Maybe UserType
validCredentials email password = 
       -- Zip the email and password lists and lookup the email
       case lookup email (zip validUserEmail validUserPassword) of
              Just correctPassword | correctPassword == password ->  Just (User email password)  -- Return User type for user
              _ -> case lookup email (zip validCoachEmail validCoachPassword) of
                     Just correctPassword | correctPassword == password -> Just (Coach email password)  -- Return Coach type for coach 
                     _ -> Nothing

-- Function to perform login
performLogin :: IO()
performLogin = do
       putStrLn "\n***************************************"
       putStrLn "     Welcome to the FitGym System      "
       putStrLn "***************************************"
       putStr "Enter your email: "
       email <- getLine
       putStr "Enter your password: "
       password <- getLine
       -- Apply the validation function and handle the result using a Functor
       case validCredentials email password of
              Just userType -> do
                     let loginMessage = case userType of
                            User _ _ -> login userType email password
                            Coach _ _ -> login userType email password
                     case loginMessage of
                            Just message -> putStrLn message 
                            Nothing -> putStrLn "Invalid credentials. Please try again."
                     userJourney userType
              Nothing -> do
                     putStrLn "Invalid credentials. Please try again."
                     performLogin 


-- Function to ask User experience at Gym after login
askUserExperience :: IO String
askUserExperience =
       putStrLn "\nStep (1/4) " >>
       putStrLn "What is your experience at the gym?" >>
       putStrLn "1. Beginner (Just Starting. No experience)" >>
       putStrLn "2. Intermediate (Been at the gym. Already worked ouut for a few months)" >>
       putStrLn "3. Advanced (Equiments are my freinds)" >>
       putStr "Enter your choice: " >>
       getLine >>= \experience ->
              case experience of 
                     "1" -> return "Beginner"
                     "2" -> return "Intermediate"
                     "3" -> return "Advanced"
                     _ -> putStrLn "Invalid choice. Please try again." >>
                            askUserExperience

-- Function to display user journey based on user type
userJourney :: UserType -> IO ()
userJourney userType = case userType of 
       User _ _ -> do  
              askUserExperience >>= 
                     \experience -> putStrLn $ "You are a " ++ experience ++ " user."
       Coach _ _ -> putStrLn "You are a coach."

prompt :: String -> IO String
prompt message = putStrLn message >> getLine

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
        1 -> performLogin
        2 -> putStrLn "Exiting the system. Goodbye!"
        _ -> do
            putStrLn "Invalid choice. Please try again.\n"
            main -- Recursively call main to allow another attempt
