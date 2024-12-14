import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (find)

-- Define the Email, Password, and Description types
type Email = String
type Password = String

-- Define the Credentials data type that holds email, password, and description
data Credentials = Credentials
  { email :: Email
  , password :: Password
  } deriving (Show)

-- Define a new data type for Appointment
data Appointment = Appointment
  { userEmail :: Email
  , coachEmail :: Email
  , appointmentDate :: String
  , appointmentTime :: String
  } deriving (Show)

data Availability = Availability
  { emailCoach :: Email
  , day :: [String]
  , time :: [(String, [String])] -- (day, [time])
  } deriving (Show)


-- Define the UserType data type that uses the Credentials type
data UserType = User Credentials | Coach Credentials deriving (Show)
data Action = GymWork | MakeAppointment | GoBackToLogin deriving (Show, Read) -- Read typeclass is used to convert string to
data Experience = Beginner | Intermediate | Advanced deriving (Show, Read) -- Read typeclass is used to convert string to
data Goal = Strength | MuscleSize | MuscleEndurance deriving (Show, Read) -- Read typeclass is used to convert string to
data WorkoutDay = One | Two | Three | Four | Five | Six | Seven deriving (Show, Read) -- Read typeclass is used to convert string


-- Predefined credentials for users and coaches
validUserCredentials :: [Credentials]
validUserCredentials =
  [ Credentials { email = "wzhao@fitgym.com", password = "wz123"},
    Credentials { email = "zhengtan@fitgym.com", password = "zt123"}
  ]

validCoachCredentials :: [Credentials]
validCoachCredentials =
  [ Credentials { email = "Jane@fitgym.com", password = "Jane123"},
    Credentials { email = "Jack@fitgym.com", password = "Jack123"}
  ]

coachAvailability :: [Availability]
coachAvailability = [
       Availability { emailCoach = "Jane@fitgym.com", 
       day = ["Monday", "Wednesday", "Friday"], 
       time = [("Monday", ["10:00", "11:00"]), ("Wednesday", ["10:00", "11:00"]), ("Friday", ["10:00", "11:00"])]
       },

       Availability { emailCoach = "Jack@fitgym.com", 
       day = ["Monday", "Tuesday", "Thursday"], 
       time = [("Monday", ["10:00", "11:00"]), ("Tuesday", ["12:00", "2:00"]), ("Thursday", ["10:00", "11:00"])]
       }]

-- Define a class for loginable types
class Loginable a where
    login :: a -> String -> String -> Maybe String

-- Implement the Loginable instance for UserType
instance Loginable UserType where
    login user enteredEmail enteredPassword =
        case user of
            User (Credentials email password) -> loginUserOrCoach enteredEmail enteredPassword email password
            Coach (Credentials email password) -> loginUserOrCoach enteredEmail enteredPassword email password

-- Helper function to handle login logic for both users and coaches
loginUserOrCoach :: String -> String -> String -> String -> Maybe String
loginUserOrCoach enteredEmail enteredPassword email password
    | email == enteredEmail && password == enteredPassword = Just $ "\nLogin successful. Welcome: " ++ email ++ "!"
    | otherwise = Nothing

-- Define a class for gym questions
class Question a where
       askQuestion :: IO a

instance Question Action where
    askQuestion = askGymQuestion "What would you like to do?" 
                        [("1", GymWork, "GymWork"),
                         ("2", MakeAppointment, "MakeAppointment"),
                         ("3", GoBackToLogin, "GoBackToLogin")
                         ]

instance Question Experience where
    askQuestion = askGymQuestion "What is your experience at the gym?" 
                        [("1", Beginner, "Beginner"),
                         ("2", Intermediate, "Intermediate"),
                         ("3", Advanced, "Advanced")]

instance Question Goal where
    askQuestion = askGymQuestion "What is your goal at the gym?" 
                        [("1", Strength, "Strength: Focus on building muscle strength"),
                         ("2", MuscleSize, "Muscle Size: Focus on hypertrophy to grow muscle mass"),
                         ("3", MuscleEndurance, "Muscle Endurance: Focus on endurance to sustain longer workouts")]

instance Question WorkoutDay where
    askQuestion = askGymQuestion "How many days per week do you plan to work out (1-7)?" 
                        [("1", One, "1 day per week"),
                         ("2", Two, "2 days per week"),
                         ("3", Three, "3 days per week"),
                         ("4", Four, "4 days per week"),
                         ("5", Five, "5 days per week"),
                         ("6", Six, "6 days per week"),
                         ("7", Seven, "7 days per week")]

-- Helper function to display and select an option from a list
selectOption :: String -> [String] -> IO (Maybe String)
selectOption prompt options = do
  putStrLn prompt
  mapM_ (\(i, option) -> putStrLn (show i ++ ". " ++ option)) (zip [1..] options)
  putStr "Enter your choice: "
  choice <- getLine
  return $ case reads choice of
    [(index, "")] | index > 0 && index <= length options -> Just (options !! (index - 1))
    _ -> Nothing

-- Helper function to retry on invalid input
retryOnInvalid :: IO (Maybe a) -> String -> IO a
retryOnInvalid action errorMessage = do
  result <- action
  case result of
    Just value -> return value
    Nothing -> do
      putStrLn errorMessage
      retryOnInvalid action errorMessage

-- Main function to make an appointment
makeAppointment :: String -> [Availability] -> IO Appointment
makeAppointment userEmail coachAvailability = do
  -- Step 1: Select a coach
  let coachEmails = map emailCoach coachAvailability
  selectedCoachEmail <- retryOnInvalid
    (selectOption "Choose a coach:" coachEmails)
    "Invalid coach selection. Please try again."

  -- Step 2: Find the selected coach's availability
  let Just selectedCoachAvailability = find (\coach -> emailCoach coach == selectedCoachEmail) coachAvailability

  -- Step 3: Select an available date
  selectedDate <- retryOnInvalid
    (selectOption "\nAvailable days:" (day selectedCoachAvailability))
    "Invalid date selection. Please try again."

  -- Step 4: Select an available time for the chosen date
  let timesForDate = fromMaybe [] $ lookup selectedDate (time selectedCoachAvailability)
  selectedTime <- retryOnInvalid
    (selectOption "\nAvailable times:" timesForDate)
    "Invalid time selection. Please try again."

  -- Step 5: Create and return the appointment
  let appointment = Appointment
        { userEmail = userEmail
        , coachEmail = selectedCoachEmail
        , appointmentDate = selectedDate
        , appointmentTime = selectedTime
        }
  return appointment

appointments :: [Appointment]
appointments = [] 

-- Helper function to get email of the user or coach
getUserEmail :: UserType -> Email
getUserEmail (User (Credentials email _)) = email
getUserEmail (Coach (Credentials email _)) = email

-- Generalized askQuestion function
askGymQuestion :: String -> [(String, a, String)] -> IO a
askGymQuestion prompt options = do
    putStrLn prompt
    mapM_ (\(label, _, description) -> putStrLn (label ++ ": " ++ description)) options
    putStr "Enter your choice: "
    choice <- getLine
    putStrLn "\n***************************************"
    case lookup choice (map (\(label, _, description) -> (label, description)) options) of
        Just description -> putStrLn ("             " ++ description ++ "            ") >>
                            putStrLn "***************************************"
        Nothing -> putStrLn "Invalid choice. Please try again." 
    case lookup choice (map (\(label, val, _) -> (label, val)) options) of
        Just result -> return result
        Nothing -> putStrLn "Invalid choice. Please try again." >> askGymQuestion prompt options


-- Helper function to display a prompt and get user input
prompt :: String -> IO String
prompt message = putStrLn message >> getLine

 
-- Validate credentials and return the appropriate UserType
validCredentials :: String -> String -> Maybe UserType
validCredentials enteredEmail enteredPassword = 
    -- Check if the email and password match valid user credentials
    case lookup enteredEmail (map (\(Credentials e p) -> (e, p)) validUserCredentials) of
        Just correctPassword | correctPassword == enteredPassword -> Just (User (Credentials enteredEmail enteredPassword))
        _ -> case lookup enteredEmail (map (\(Credentials e p) -> (e, p)) validCoachCredentials) of
               Just correctPassword | correctPassword == enteredPassword -> Just (Coach (Credentials enteredEmail enteredPassword))
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
                     let loginMessage = login userType email password
                     in case loginMessage of
                            Just message -> putStrLn message >> userJourney userType appointments
                            Nothing -> putStrLn "Invalid credentials. Please try again."
              Nothing -> putStrLn "Invalid credentials. Please try again." >> performLogin 

-- Function to display user journey based on user type
userJourney :: UserType -> [Appointment] -> IO ()
userJourney userType appointments = case userType of
    -- For User
    User _ -> do
        putStrLn "\n***************************************"
        putStrLn "                FitGym                  " 
        putStrLn "***************************************"
        action <- askQuestion :: IO Action
        -- Ask the user for their experience, goal, and workout days.
        case action of
            GymWork -> do
                experience <- askQuestion :: IO Experience
                goal <- askQuestion :: IO Goal
                workoutDay <- askQuestion :: IO WorkoutDay
                putStrLn ("You are a " ++ show experience ++ " user with the goal of " ++ show goal ++ ".")
                putStrLn ("You plan to work out " ++ show workoutDay ++ " days per week.")
            MakeAppointment -> do
                let userEmail = getUserEmail userType
                appointment <- makeAppointment userEmail coachAvailability
                -- Update the appointments list
                let updatedAppointments = appointments ++ [appointment]
                putStrLn "\n***************************************"
                putStrLn "         Appointment Scheduled          " 
                putStrLn "***************************************"
                putStrLn ("Coach: " ++ coachEmail appointment ++ 
                    " \nDate: " ++ appointmentDate appointment ++ 
                    " \nTime: " ++ appointmentTime appointment)
                putStrLn "***************************************"
                -- Recurse with the updated appointments list
                userJourney userType updatedAppointments
            GoBackToLogin -> do
                putStrLn "\nReturning to the login screen..."
                performLogin -- Go back to login screen
    -- For Coach
    Coach (Credentials coachEmail _) -> do
      putStrLn "\n***************************************"
      putStrLn "             FitGym (Coach)             " 
      putStrLn "***************************************"
      -- Filter the appointments for this coach
      let coachAppointments = filter (\appointment -> coachEmail == userEmail appointment) appointments
      if null coachAppointments
          then putStrLn "No scheduled appointments."
          else do
              putStrLn "Scheduled Appointments:"
              mapM_ (\appointment -> do
                  putStrLn ("Coach: " ++ coachEmail)
                  putStrLn ("Date: " ++ appointmentDate appointment)
                  putStrLn ("Time: " ++ appointmentTime appointment)
                  putStrLn "***************************************"
                  ) coachAppointments

-- Function to display the login menu and get user choice
getChoice :: IO Int
getChoice =
    putStrLn "***************************************" >>
    putStrLn "     Welcome to the FitGym System      " >>
    putStrLn "***************************************" >>
    putStrLn "1. Login" >>
    putStrLn "2. Exit" >>
    putStr "Enter your choice: " >>
    getLine >>= \choice -> 
       case readMaybe choice :: Maybe Int of
              Just input -> 
                     return input 
              Nothing -> do
                     putStrLn "Invalid choice. Please enter a number.\n"
                     getChoice

-- Main function
main :: IO ()
main = getChoice >>= \choice -> 
       case choice of
              1 -> performLogin
              2 -> putStrLn "Exiting the system. Goodbye!"
              _ -> do
                     putStrLn "Invalid choice. Please try again.\n"
                     main -- Recursively call main to allow another attempt