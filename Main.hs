-- Predefined credentials for user and coach
validUserEmail :: [String]
validUserEmail = ["wzhao@fitgym.com", "zhengtan@fitgym.com"]
validUserPassword :: [String]
validUserPassword = ["wz123", "zt123"]

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


-- Function to perform login
performLogin :: (String -> String -> Maybe String) -> IO()
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
              Just successLogin -> putStrLn successLogin
              Nothing -> do
                     putStrLn "Invalid credentials. Please try again."
                     performLogin validCredentials
       
-- Function to display the Usertype 
showUserType :: UserType -> String
showUserType (User _ _) = "User"
showUserType (Coach _ _) = "Coach"

-- Validate credentials and return the appropriate Usertype
validCredentials :: String -> String -> Maybe String
validCredentials email password = 
       -- Zip the email and password lists and lookup the email
       case lookup email (zip validUserEmail validUserPassword) of
              Just correctPassword | correctPassword == password -> 
                     Just (User email password) >>= \user -> login user email password -- Return User type for user
              _ -> case lookup email (zip validCoachEmail validCoachPassword) of
                     Just correctPassword | correctPassword == password ->
                            Just (Coach email password) >>= \coach -> login coach email password -- Return Coach type for coach 
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
