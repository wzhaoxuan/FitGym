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

-- Define a data type for users with their workout recommendation
data WorkoutPlan = WorkoutPlan
  { experience :: Experience
  , goal :: Goal
  } deriving (Show)

-- Define the UserType data type that uses the Credentials type
data UserType = User Credentials | Coach Credentials deriving (Show)
data Action = GymWork | MakeAppointment | GoBackToLogin deriving (Show, Read) -- Read typeclass is used to convert string to
data Experience = Beginner | Intermediate | Advanced deriving (Show, Read) -- Read typeclass is used to convert string to
data Goal = Strength | MuscleSize | MuscleEndurance deriving (Show, Read) -- Read typeclass is used to convert string to



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


-- Define a class for recommending workouts
class RecommendWorkout a where
    recommend :: a -> String

-- Implement the RecommendWorkout instance for the WorkoutPlan type
instance RecommendWorkout WorkoutPlan where
    recommend (WorkoutPlan Beginner Strength) =
        "Workout days per week: 3-4\n"  ++
        "\n***3-Day Workout Plan (Full Body)***\n" ++
        "Monday\n" ++
        "1. Warm-up (5-10 minutes):\n" ++
        "   Light cardio (e.g., treadmill, cycling, or elliptical)\n" ++
        "   Dynamic stretches (leg swings, arm circles)\n" ++
        "\n2. Workout:\n" ++
        "   Barbell Squats: 3 sets of 5-8 reps\n" ++
        "   Barbell Bench Press: 3 sets of 5-8 reps\n" ++
        "   Barbell Deadlift: 3 sets of 5 reps\n" ++
        "   Overhead Press (Barbell or Dumbbells): 3 sets of 5-8 reps\n" ++
        "   Pull-ups (Assisted if needed) or Lat Pulldown: 3 sets of 6-8 reps\n" ++
        "   Plank: 3 sets of 30-45 seconds\n" ++
        "\n3. Cool-down (5-10 minutes):\n" ++
        "   Stretching: Full body (quads, hamstrings, shoulders, chest)\n" ++
        "\nWednesday:\n" ++
        "1. Warm-up (5-10 minutes):\n" ++
        "   Light cardio (e.g., treadmill, cycling, or elliptical)\n" ++
        "   Dynamic stretches (leg swings, arm circles)\n" ++
        "\n2. Workout:\n" ++
        "   Deadlift (Conventional or Romanian): 3 sets of 5 reps\n" ++
        "   Dumbbell Lunges: 3 sets of 8-10 reps per leg\n" ++
        "   Dumbbell Chest Press: 3 sets of 6-8 reps\n" ++
        "   Seated Row Machine: 3 sets of 8-10 reps\n" ++
        "   Dumbbell Bicep Curls: 3 sets of 8-10 reps\n" ++
        "   Tricep Pushdowns (Cable Machine): 3 sets of 8-10 reps\n" ++
        "\n3. Cool-down (5-10 minutes):\n" ++
        "   Stretching: Full body (quads, hamstrings, shoulders, chest)\n" ++
        "\nFriday:\n" ++
        "1. Warm-up (5-10 minutes):\n" ++
        "   Light cardio (e.g., treadmill, cycling, or elliptical)\n" ++
        "   Dynamic stretches (leg swings, arm circles)\n" ++
        "\n2. Workout:\n" ++
        "   Barbell Squats (if possible, with slightly higher weight than Day 1): 3 sets of 5 reps\n" ++
        "   Incline Dumbbell Press: 3 sets of 6-8 reps\n" ++
        "   Barbell Rows: 3 sets of 5-8 reps\n" ++
        "   Leg Press Machine: 3 sets of 8-10 reps\n" ++
        "   Dumbbell Shoulder Press: 3 sets of 6-8 reps\n" ++
        "   Cable Face Pulls: 3 sets of 10-12 reps\n" ++
        "\n3. Cool-down (5-10 minutes):\n" ++
        "   Stretching: Full body (quads, hamstrings, shoulders, chest)"
    recommend _ = "No specific recommendation available for your profile."

-- Helper function to get the workout recommendation based on user experience and goal
getRecommendation :: Experience -> Goal -> String
getRecommendation experience goal =
    recommend (WorkoutPlan experience goal)

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
makeAppointment :: String -> [Availability] -> [Appointment] -> IO [Appointment]
makeAppointment userEmail coachAvailability appointments = do
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

  -- Step 5: Create the appointment
  let newAppointment = Appointment
        { userEmail = userEmail
        , coachEmail = selectedCoachEmail
        , appointmentDate = selectedDate
        , appointmentTime = selectedTime
        }

  -- Step 6: Add the new appointment to the list and return the updated list
  let updatedAppointments = newAppointment : appointments
  putStrLn "\n***************************************"
  putStrLn "         Appointment Scheduled          " 
  putStrLn "***************************************"
  putStrLn ("Coach: " ++ coachEmail newAppointment ++ 
      " \nDate: " ++ appointmentDate newAppointment ++ 
      " \nTime: " ++ appointmentTime newAppointment)
  putStrLn "***************************************"
  return updatedAppointments

-- Function to display appointments for the logged-in coach
viewCoachAppointments :: Email -> [Appointment] -> IO ()
viewCoachAppointments email appointments = 
    let coachAppointments = filter (\app -> email == coachEmail app) appointments
    in if null coachAppointments
        then putStrLn "No appointments scheduled.\n"
        else do
            putStrLn "\nAppointments scheduled for you:"
            mapM_ printAppointment coachAppointments

-- Helper function to print an appointment
printAppointment :: Appointment -> IO ()
printAppointment app = 
    putStrLn "***************************************" >>
    putStrLn ("User Email: " ++ userEmail app) >> 
    putStrLn ("Date: " ++ appointmentDate app) >>
    putStrLn ("Time: " ++ appointmentTime app) >>
    putStrLn "***************************************"


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
    putStrLn "\n******************************************************************"
    case lookup choice (map (\(label, _, description) -> (label, description)) options) of
        Just description -> putStrLn ("                 " ++ description ++ "            ") >>
                            putStrLn "******************************************************************"
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
                            Just message -> putStrLn message >> userJourney userType []
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
                putStrLn ("For a " ++ show experience ++ " focusing on building " ++ show goal ++ ".")
                -- Provide workout recommendation based on experience and goal
                let recommendation = getRecommendation experience goal
                putStrLn recommendation -- Display the recommendation to the user
            MakeAppointment -> do
                let userEmail = getUserEmail userType
                updatedAppointments <- makeAppointment userEmail coachAvailability appointments
                -- Recurse with the updated appointments list
                userJourney userType updatedAppointments
            GoBackToLogin -> do
                putStrLn "\nReturning to the login screen..."
                performLogin -- Go back to login screen
    -- For Coach
    Coach (Credentials coachEmail _) -> do
      putStrLn "\n***************************************"
      putStrLn "             Coach Dashboard            " 
      putStrLn "***************************************"
      viewCoachAppointments coachEmail appointments
      action <- askQuestion :: IO Action
      case action of
      -- Continue with other actions...
          GoBackToLogin -> do
              putStrLn "\nReturning to the login screen..."
              performLogin -- Go back to login screen
          _ -> userJourney userType appointments -- Recurse to allow more actions

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