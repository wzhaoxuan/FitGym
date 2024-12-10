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
        | email == enteredEmail && password == enteredPassword = Just $ "Login successful. Welcome: " ++ email ++ "!"
        | otherwise = Nothing
       login (Coach email password) enteredEmail enteredPassword
        | email == enteredEmail && password == enteredPassword = Just $ "Login successful. Welcome: " ++ email ++ "!"
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
performLogin :: IO ()
performLogin = 
       putStrLn "\n***************************************" >>
       putStrLn "     Welcome to the FitGym System      " >>
       putStrLn "***************************************" >>
       putStr "Enter your email: " >>
       getLine >>= \email -> 
       putStr "Enter your password: " >>
       getLine >>= \password -> 
       -- Apply the validation function and handle the result using a Functor
       case validCredentials email password of
              Just userType -> 
                     let loginMessage = case userType of
                            User _ _ -> login userType email password
                            Coach _ _ -> login userType email password
                     in case loginMessage of
                            Just message -> putStrLn message >> userJourney userType
                            Nothing -> putStrLn "Invalid credentials. Please try again."
              Nothing -> putStrLn "Invalid credentials. Please try again." >> performLogin 


-- Function to ask user experience at Gym after login
askUserExperience :: IO String
askUserExperience =
       putStrLn "\nStep (1/3) " >>
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

-- Function to ask user goal at Gym after login
askUserGoal :: IO String
askUserGoal =
       putStrLn "\nStep (2/3) " >>
       putStrLn "What is your goal at the gym?" >>
       putStrLn "1. Strength (Increase Strength)" >>
       putStrLn "2. Muscle Size (Hypertrophy)" >>
       putStrLn "3. Muscle Endurance" >>
       putStr "Enter your choice: " >>
       getLine >>= \goal ->
              case goal of 
                     "1" -> return "Strength"
                     "2" -> return "Muscle Size (Hypertrophy)"
                     "3" -> return "Muscle Endurance"
                     _ -> putStrLn "Invalid choice. Please try again." >>
                            askUserGoal

-- Function to ask user goal at Gym after login
askUserWorkoutDay :: IO Int
askUserWorkoutDay =
       putStrLn "\nStep (3/3) " >>
       putStrLn "How many days per week do you plan to work out (1-7)?" >>
       putStr "Enter your choice: " >>
       getLine >>= \days ->
              case days of 
                     "1" -> return 1
                     "2" -> return 2
                     "3" -> return 3
                     "4" -> return 4
                     "5" -> return 5
                     "6" -> return 6
                     "7" -> return 7
                     _ -> putStrLn "Invalid choice. Please try again." >>
                            askUserWorkoutDay

-- Function to display user journey based on user type
userJourney :: UserType -> IO ()
userJourney userType = case userType of 
       User _ _ -> 
              askUserExperience >>= \experience ->
              askUserGoal >>= \goal ->
              askUserWorkoutDay >>= \workoutDay ->
              putStrLn ("You are a " ++ experience ++ " user with the goal of " ++ goal ++ ".") >>
              putStrLn ("You plan to work out " ++ show workoutDay ++ " days per week.")
       Coach _ _ -> putStrLn "You are a coach."

prompt :: String -> IO String
prompt message = putStrLn message >> getLine

-- Function to display the login menu and get user choice
getChoice :: IO Int
getChoice =
    putStrLn "***************************************" >>
    putStrLn "     Welcome to the FitGym System      " >>
    putStrLn "***************************************" >>
    putStrLn "1. Login" >>
    putStrLn "2. Exit" >>
    putStr "Enter your choice: " >>
    getLine >>= \choice -> return (read choice :: Int)

-- Main function
main :: IO ()
main = getChoice >>= \choice -> 
       case choice of
              1 -> performLogin
              2 -> putStrLn "Exiting the system. Goodbye!"
              _ -> do
                     putStrLn "Invalid choice. Please try again.\n"
                     main -- Recursively call main to allow another attempt
