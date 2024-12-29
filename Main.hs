{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use void" #-}
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (find)

-- Define the Email, Password, and Description types
type Email = String
type Password = String
type WorkOutName = String
type Description = String

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
data WorkoutRecommend = WorkoutRecommend
  { experience :: Experience
  , goal :: Goal
  } deriving (Show)

-- Data type for log entry
data LogEntry = LogEntry
  { workoutName :: String
  , workoutDate :: String
  , sets :: Int
  , reps :: Int
  } deriving (Show, Read)

-- The first String is the workout name, and the second String is a brief description.
data Workout = Workout WorkOutName Description deriving (Show, Read)

-- Define the UserType data type that uses the Credentials type
data UserType = User Credentials | Coach Credentials deriving (Show)
data Action = RecommendGymWork | GymWork | MakeAppointment | ViewLog | GoBackToLogin deriving (Show, Read) -- Read typeclass is used to convert string to
data Exercise = Abs | Back | Biceps | Calf | Chest | Forearms | Legs | Shoulders | Triceps deriving (Show, Read) -- Read typeclass is used to convert string to
data Experience = Beginner | Intermediate | Advanced deriving (Show, Read) -- Read typeclass is used to convert string to
data Goal = Strength | MuscleSize | MuscleEndurance deriving (Show, Read) -- Read typeclass is used to convert string to
data TipCategory = ProgressionTips Goal| NutritionTips | RestAndRecovery | ExerciseAlternatives


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
                        [("1", RecommendGymWork, "Recommend GymWork"),
                         ("2", GymWork, "GymWork"),
                         ("3", MakeAppointment, "MakeAppointment"),
                         ("4", ViewLog, "ViewLog"),
                         ("5", GoBackToLogin, "GoBackToLogin")
                         ]

instance Question Exercise where
    askQuestion = askGymQuestion "What exercise would you like to do?"
                        [("1", Abs, "Abs"),
                         ("2", Back, "Back"),
                         ("3", Biceps, "Biceps"),
                         ("4", Calf, "Calf"),
                         ("5", Chest, "Chest"),
                         ("6", Forearms, "Forearms"),
                         ("7", Legs, "Legs"),
                         ("8", Shoulders, "Shoulders"),
                         ("9", Triceps, "Triceps")]


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

-- Implement the RecommendWorkout instance for the WorkoutRecommend type
instance RecommendWorkout WorkoutRecommend where
    recommend (WorkoutRecommend Beginner Strength) = unlines [
       "Workout days per week: 3",
        "***3-Day Workout Plan (Full Body)***",
        "Day 1",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 3 sets of 5-8 reps (Increase weight gradually each week)",
        "   Barbell Bench Press: 3 sets of 5-8 reps (If new, use dumbbells or machine for better control)",
        "   Barbell Deadlift: 3 sets of 5 reps (Focus on form, use lighter weights initially)",
        "   Overhead Press (Barbell or Dumbbells): 3 sets of 5-8 reps",
        "   Pull-ups (Assisted if needed) or Lat Pulldown: 3 sets of 6-8 reps",
        "   Chest: 3 sets of 30-45 seconds\n",
        generateCoolDown,
        "Day 2",
        generateWarmUp,
        "2. Workout:\n   Deadlift (Conventional or Romanian): 3 sets of 5 reps",
        "   Dumbbell Lunges: 3 sets of 8-10 reps per leg",
        "   Dumbbell Chest Press: 3 sets of 6-8 reps",
        "   Seated Row Machine: 3 sets of 8-10 reps",
        "   Dumbbell Bicep Curls: 3 sets of 8-10 reps" ,
        "   Tricep Pushdowns (Cable Machine): 3 sets of 8-10 reps\n",
        generateCoolDown,
        "Day 3",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats (if possible, with slightly higher weight than Day 1): 3 sets of 5 reps",
        "   Incline Dumbbell Press: 3 sets of 6-8 reps",
        "   Barbell Rows: 3 sets of 5-8 reps",
        "   Leg Press Machine: 3 sets of 8-10 reps (Alternative for squats if new to barbell)" ,
        "   Dumbbell Shoulder Press: 3 sets of 6-8 reps",
        "   Cable Face Pulls: 3 sets of 10-12 reps\n",
        generateCoolDown,
        generateTips (ProgressionTips Strength),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Beginner MuscleSize) = unlines [
       "Workout days per week: 3",
        "***3-Day Workout Plan (Full Body Split for Hypertrophy)***",
        "Day 1",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 4 sets of 8-10 reps (Focus on depth and full range of motion)",
        "   Barbell Bench Press: 4 sets of 8-10 reps (Ensure a controlled motion and steady pace)",
        "   Dumbbell Row: 4 sets of 8-10 reps (Keep your back neutral, avoid swinging)",
        "   Dumbbell Shoulder Press: 3 sets of 8-10 reps (Start light to master form)",
        "   Leg Press Machine: 3 sets of 10-12 reps (Alternative for squats if new)",
        "   Tricep Pushdowns (Cable): 3 sets of 10-12 reps",
        "   Dumbbell Bicep Curls: 3 sets of 10-12 reps\n",
        generateCoolDown,
        "Day 2",
        generateWarmUp,
        "2. Workout:\n   Deadlift (Conventional or Romanian): 4 sets of 6-8 reps (Focus on hip hinge)",
        "   Incline Dumbbell Press: 4 sets of 8-10 reps (Slow and controlled descent)",
        "   Lat Pulldown or Pull-ups: 3 sets of 8-10 reps (Work full range of motion)",
        "   Dumbbell Lateral Raises: 3 sets of 12-15 reps (Focus on mind-muscle connection)",
        "   Lunges (Dumbbells or Barbell): 3 sets of 10-12 reps per leg",
        "   Leg Curls (Machine): 3 sets of 10-12 reps",
        "   Cable Face Pulls: 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 3",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 4 sets of 8-10 reps (Gradually increase weight from Day 1)",
        "   Dumbbell Chest Press: 4 sets of 8-10 reps",
        "   Barbell Rows: 4 sets of 8-10 reps",
        "   Overhead Press (Barbell or Dumbbells): 3 sets of 8-10 rep",
        "   Dumbbell Bicep Curls: 3 sets of 10-12 reps",
        "   Tricep Dips (Bodyweight or Weighted): 3 sets of 8-10 reps",
        "   Chests: 3 sets of 30-45 seconds\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleSize),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Beginner MuscleEndurance) = unlines [
      "Workout days per week: 3",
        "***3-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1",
        generateWarmUp,
        "2. Workout:\n   Bodyweight Squats: 3 sets of 15-20 reps (Increase weight gradually each week)",
        "   Dumbbell Bench Press: 3 sets of 12-15 reps (If new, use dumbbells or machine for better control)",
        "   Dumbbell Row (each arm): 3 sets of 12-15 reps (Focus on form, use lighter weights initially)",
        "   Standing Overhead Dumbbell Press: 3 sets of 12-15 reps",
        "   Chest with Shoulder Taps: 3 sets of 30 seconds\n",
        generateCoolDown,
        "Day 2",
        generateWarmUp,
        "2. Workout:\n   Lunges (Bodyweight or Dumbbells): 3 sets of 12-15 reps per leg",
        "   Incline Dumbbell Press: 3 sets of 12-15 reps (Slow and controlled descent)",
        "   Lat Pulldown or Pull-ups: 3 sets of 12-15 reps (Work full range of motion)",
        "   Dumbbell Lateral Raises: 3 sets of 12-15 reps (Focus on mind-muscle connection)",
        "   Russian Twists (Bodyweight or Medicine Ball): 3 sets of 20 twists (10 per side)\n",
        generateCoolDown,
        "Day 3",
        generateWarmUp,
        "2. Workout:\n   Biceps (Dumbbells or Barbell): 3 sets of 12-15 reps",
        "   Push-Ups (Knee or Standard): 3 sets of 15-20 reps",
        "   Seated Row Machine: 3 sets of 12-15 reps",
        "   Dumbbell Step-Ups (each leg): 3 sets of 12-15 reps",
        "   Side Chests (each side): 3 sets of 20-30 seconds\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleEndurance),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Intermediate Strength) = unlines [
      "Workout days per week: 4",
        "***4-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1 (Upper Body Push)",
        generateWarmUp,
        "2. Workout:\n   Barbell Bench Press: 4 sets of 5-6 reps.",
        "   Overhead Press (Barbell or Dumbbells): 4 sets of 5-6 reps.",
        "   Incline Dumbbell Press: 3 sets of 8-10 reps.",
        "   Chest Fly (Cable or Dumbbells): 3 sets of 10-12 reps",
        "   Tricep Dips (Bodyweight or Weighted): 3 sets of 6-8 reps",
        "   Lateral Raises: 3 sets of 12-15 reps",
        "   Core (Chest): 3 sets of 45-60 seconds\n",
        generateCoolDown,
        "Day 2 (Lower Body Strength)",
        generateWarmUp,
        "2. Workout:\n   Barbell Back Squats: 4 sets of 5-6 reps",
        "   Romanian Biceps (Barbell or Dumbbells): 4 sets of 6-8 reps",
        "   Leg Press: 3 sets of 8-10 reps",
        "   Bulgarian Split Squats (Dumbbells): 3 sets of 8-10 reps per leg",
        "   Calf Raises (Weighted or Bodyweight): 4 sets of 12-15 reps",
        "   Core (Leg Raises): 3 sets of 15 reps\n",
        generateCoolDown,
        "Day 3 (Upper Body Pull)",
        generateWarmUp,
        "2. Workout:\n   Biceps (Conventional or Sumo): 4 sets of 5 reps",
        "   Pull-ups (Weighted if possible): 4 sets of 5-6 reps",
        "   Barbell Rows (Pendlay or Bent-over): 4 sets of 6-8 reps",
        "   Single-arm Dumbbell Row: 3 sets of 8-10 reps per arm",
        "   Lat Pulldowns (Cable): 3 sets of 8-10 reps",
        "   Face Pulls (Cable): 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 4 (Full Body Strength)",
        generateWarmUp,
        "2. Workout:\n   Front Squats (Barbell): 4 sets of 5 reps",
        "   Barbell Biceps (Heavy): 4 sets of 5 reps",
        "   Dumbbell Shoulder Press: 3 sets of 6-8 reps",
        "   Hammer Curls (Dumbbells): 3 sets of 10-12 reps",
        "   Barbell Shrugs: 3 sets of 10-12 reps",
        "   Cable Tricep Pushdowns: 3 sets of 12-15 reps\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleEndurance),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Intermediate MuscleSize) = unlines [
      "Workout days per week: 4",
        "***4-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1 (Chest, Shoulders, Triceps)",
        generateWarmUp,
        "2. Workout:\n   Barbell Bench Press: 4 sets of 8-10 reps",
        "   Incline Dumbbell Press: 4 sets of 10-12 reps",
        "   Dumbbell Shoulder Press: 3 sets of 10-12 reps",
        "   Lateral Raises: 3 sets of 12-15 reps",
        "   Tricep Dips: 3 sets of 8-10 reps (Add weight if necessary)",
        "   Cable Chest Flys: 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 2 (Legs, Glutes)",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 4 sets of 8-10 reps",
        "   Leg Press Machine: 4 sets of 10-12 reps",
        "   Romanian Biceps (Dumbbells or Barbell): 3 sets of 8-10 reps",
        "   Leg Curls (Machine): 3 sets of 12-15 reps",
        "   Walking Lunges (with Dumbbells): 3 sets of 12 reps per leg",
        "   Standing Calf Raises: 3 sets of 15-20 reps\n",
        generateCoolDown,
        "Day 3 (Back, Biceps)",
        generateWarmUp,
        "2. Workout:\n   Pull-ups (Weighted if possible): 4 sets of 6-8 reps",
        "   Barbell Rows: 4 sets of 8-10 reps",
        "   Seated Cable Rows: 3 sets of 10-12 reps",
        "   Face Pulls (Cable): 3 sets of 12-15 reps",
        "   Dumbbell Bicep Curls: 3 sets of 12-15 reps",
        "   Hammer Curls: 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 4 (Full Body)",
        generateWarmUp,
        "2. Workout:\n   Barbell Biceps: 3 sets of 6-8 reps",
        "   Chest Press Machine: 3 sets of 8-10 reps",
        "   Dumbbell Rows: 3 sets of 10-12 reps",
        "   Overhead Dumbbell Press: 3 sets of 10-12 reps",
        "   Cable Tricep Pushdowns: 3 sets of 12-15 reps",
        "   Preacher Curls (Machine): 3 sets of 10-12 reps",
        "   Barbell Shrugs: 3 sets of 12-15 reps\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleSize),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Intermediate MuscleEndurance) = unlines [
      "Workout days per week: 3",
        "***3-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1 (Full Body Circuit)",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 4 sets of 12-15 reps. (Increase weight gradually each week)",
        "   Dumbbell Bench Press: 4 sets of 12-15 reps.",
        "   Barbell Deadlift: 3 sets of 10-12 reps.",
        "   Dumbbell Shoulder Press: 4 sets of 12-15 reps.",
        "   Lat Pulldown: 4 sets of 12-15 reps.",
        "   Chest: 3 sets of 45-60 seconds.\n",
        generateCoolDown,
        "Day 2 (Upper Body Focus)",
        generateWarmUp,
        "2. Workout:\n   Lunges (Bodyweight or Dumbbells): 4 sets of 10-12 reps per leg.",
        "   Incline Dumbbell Press: 4 sets of 12-15 reps.",
        "   Seated Cable Rows: 4 sets of 12-15 reps.",
        "   Dumbbell Lateral Raises: 3 sets of 15-20 reps.",
        "   Leg Curls (Machine): 4 sets of 12-15 reps.",
        "   Side Chest (each side): 3 sets of 30-45 seconds.\n",
        generateCoolDown,
        "Day 3 (Lower Body Focus)",
        generateWarmUp,
        "2. Workout:\n   Bulgarian Split Squats: 4 sets of 10-12 reps per leg.",
        "   Push-Ups (Weighted if possible): 4 sets of 15-20 reps.",
        "   Dumbbell Rows: 4 sets of 12-15 reps per arm.",
        "   Dumbbell Front Raises: 3 sets of 15-20 reps.",
        "   Calf Raises: 4 sets of 15-20 reps.",
        "   Russian Twists (with or without weight): 3 sets of 20 reps (10 per side).\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleEndurance),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Advanced Strength) = unlines [
        "Workout days per week: 4",
        "***4-Day Workout Plan (Upper Body Strength)***",
        "Day 1 (Push-Pull Focus)",
        generateWarmUp,
        "2. Workout:\n   Barbell Bench Press: 5 sets of 3-5 reps",
        "   Pull-ups (Weighted): 4 sets of 5-6 reps",
        "   Incline Dumbbell Press: 3 sets of 6-8 reps",
        "   Barbell Row (Pendlay Row): 4 sets of 5 reps",
        "   Overhead Dumbbell Tricep Extension: 3 sets of 8-10 reps",
        "   Barbell Curls: 3 sets of 8-10 reps \n",
        generateCoolDown,
        "Day 2 (Lower Body Strength)",
        "2. Workout:\n   Barbell Back Squats: 5 sets of 3-5 reps",
        "   Romanian Biceps: 4 sets of 6-8 reps",
        "   Leg Press (High Weight): 4 sets of 8 reps",
        "   Walking Lunges (Heavy Dumbbells): 3 sets of 12 steps per leg",
        "   Standing Calf Raises (Weighted): 3 sets of 15 reps",
        "   Chest with Weight Plate: 3 sets of 60 seconds\n",
        generateCoolDown,
        "Day 3 (Deadlift & Posterior Chain Focus)",
        generateWarmUp,
        "2. Workout:\n   Biceps (Conventional): 5 sets of 3-5 reps",
        "   Barbell Hip Thrusts: 4 sets of 8 reps",
        "   Barbell Rows: 4 sets of 6-8 reps",
        "   Pull-throughs (Cable or Resistance Band): 3 sets of 12-15 reps",
        "   Hanging Leg Raises: 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 4 (Full-Body Strength)",
        generateWarmUp,
        "2. Workout:\n   Overhead Press (Barbell): 5 sets of 3-5 reps",
        "   Push Press (Barbell): 4 sets of 5 reps",
        "   Dumbbell Arnold Press: 3 sets of 6-8 reps",
        "   Face Pulls (Cable): 4 sets of 12-15 reps",
        "   Russian Twists (Weighted): 3 sets of 40 twists",
        "   Chest to Side Chest (Weighted): 3 sets of 60 seconds\n",
        generateCoolDown,
        generateTips (ProgressionTips Strength),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Advanced MuscleSize) = unlines [
      "Workout days per week: 5",
        "***5-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1 (Push-Pull Focus)",
        generateWarmUp,
        "2. Workout:\n   Barbell Bench Press: 4 sets of 8-10 reps",
        "   Pull-ups (Weighted if possible): 4 sets of 8-10 reps",
        "   Incline Dumbbell Press: 4 sets of 10-12 reps",
        "   Seated Row Machine: 4 sets of 10-12 reps",
        "   Chest Fly (Cable or Dumbbells): 3 sets of 12-15 reps",
        "   Single-arm Dumbbell Row: 3 sets of 12-15 reps per arm\n",
        generateCoolDown,
        "Day 2 (Quad & Hamstring Focus)",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 4 sets of 8-10 reps",
        "   Romanian Biceps: 4 sets of 8-10 reps",
        "   Leg Press Machine: 4 sets of 10-12 reps",
        "   Walking Lunges (Dumbbells): 3 sets of 12 steps per leg",
        "   Leg Curls (Machine): 3 sets of 12-15 reps",
        "   Calf Raises (Seated or Standing): 3 sets of 20-25 reps\n",
        generateCoolDown,
        "Day 3 (Hypertrophy Focus)",
        generateWarmUp,
        "2. Workout:\n   Overhead Barbell Press: 4 sets of 8-10 reps",
        "   Lateral Raises: 4 sets of 12-15 reps",
        "   Rear Delt Fly (Cable or Dumbbells): 3 sets of 12-15 reps",
        "   Barbell Curls: 4 sets of 10-12 reps",
        "   Tricep Dips (Weighted): 3 sets of 10-12 reps",
        "   Hammer Curls: 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 4 (Volume Focus)",
        generateWarmUp,
        "2. Workout:\n   Incline Barbell Bench Press: 4 sets of 8-10 reps",
        "   Dumbbell Fly (Incline): 3 sets of 12-15 reps",
        "   Cable Chest Press: 4 sets of 10-12 reps",
        "   Tricep Pushdowns (Cable): 4 sets of 10-12 reps",
        "   EZ Bar Curls: 4 sets of 10-12 reps",
        "   Dumbbell Concentration Curls: 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 5 (Legs & Core Focus)",
        generateWarmUp,
        "2. Workout:\n   Barbell Biceps: 4 sets of 8-10 reps",
        "   Goblet Squats (Dumbbell): 3 sets of 10-12 reps",
        "   Leg Extensions: 4 sets of 12-15 reps",
        "   Chest with Weighted Plate: 3 sets of 60 seconds",
        "   Russian Twists (Weighted): 3 sets of 40 twists",
        "   Hanging Leg Raises: 3 sets of 15-20 reps\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleSize),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutRecommend Advanced MuscleEndurance) = unlines [
      "Workout days per week: 5",
        "***5-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1 (Full-Body Circuit)",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 3 sets of 12-15 reps. (Increase weight gradually each week)",
        "   Pull-ups (Weighted if possible): 3 sets of 8-10 reps",
        "   Bench Press (Moderate weight): 3 sets of 12-15 reps",
        "   Dumbbell Lunges (Per leg): 3 sets of 10 reps",
        "   Kettlebell Swings: 3 sets of 20 reps",
        "   Chest with Shoulder Taps: 3 sets of 60 seconds\n",
        generateCoolDown,
        "Day 2 (Upper Body Focus)",
        generateWarmUp,
        "2. Workout:\n   Push-ups (Weighted or Deficit): 3 sets of 20 reps",
        "   Seated Row Machine: 3 sets of 15-20 reps",
        "   Arnold Press (Dumbbells): 3 sets of 12-15 reps",
        "   Incline Dumbbell Fly: 3 sets of 15 reps",
        "   Lateral Raises: 3 sets of 15 reps",
        "   Russian Twists (Weighted): 3 sets of 40 twists\n",
        generateCoolDown,
        "Day 3 (Lower Body Focus)",
        generateWarmUp,
        "2. Workout:\n   Biceps (Conventional or Romanian): 3 sets of 8-10 reps",
        "   Step-ups with Dumbbells: 3 sets of 15 reps per leg",
        "   Calf Raises (Bodyweight or Weighted): 3 sets of 25 reps",
        "   Wall Sit (with Weight): 3 sets of 60 seconds",
        generateCoolDown,
        "Day 4 (HIIT and Core)",
        generateWarmUp,
        "2. Workout:\n   Rowing Machine (Intervals): 5 rounds: 40 seconds sprint, 20 seconds rest",
        "   Burpees: 3 sets of 15 reps",
        "   Mountain Climbers: 3 sets of 30 seconds",
        "   Hanging Leg Raises: 3 sets of 12-15 reps",
        "   Flutter Kicks: 3 sets of 30 seconds",
        generateCoolDown,
        "Day 5 (Full-Body Superset Workout)",
        generateWarmUp,
        "2. Workout:\n   Barbell Front Squats + Overhead Press: 3 sets of 10-12 reps each",
        "   Dumbbell Biceps + Pull-ups: 3 sets of 8-10 reps each",
        "   Kettlebell Swings + Push-ups: 3 sets of 20 reps each",
        "   Battle Ropes (30 seconds) + Medicine Ball Slams: 3 sets",
        generateCoolDown,
        generateTips (ProgressionTips MuscleEndurance),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

-- Define a helper function to generate warm-up, workout, and cool-down sections
generateWarmUp :: String
generateWarmUp = unlines [
    "1. Warm-up (5-10 minutes):",
    "   Light cardio (e.g., treadmill, cycling, or elliptical)",
    "   Dynamic stretches (leg swings, arm circles)",
    "   Mobility work (e.g., hip openers, shoulder rotations)"
  ]

generateCoolDown :: String
generateCoolDown = unlines [
    "3. Cool-down (5-10 minutes):",
    "   Stretching: Full body (quads, hamstrings, shoulders, chest)",
    "   Focus on breathing and mobility stretches"
  ]

  -- Helper function to generate tips based on the category
generateTips :: TipCategory -> String
generateTips (ProgressionTips Strength) = unlines [
    "Progression Tips:",
      "   Gradually increase weights or reps each week, ensuring you maintain good form.",
      "   Aim to stick with the program for at least 6-8 weeks",
      "   Aim to progress by 2.5-5 kg (5-10 lbs) per exercise.",
      "   Focus on quality over quantity: prioritize proper form.",
      "   Don't forget to rest! Aim for at least one rest day between workouts."
  ]
generateTips (ProgressionTips MuscleSize) = unlines [
    "Progression Tips:",
      "   Gradually increase weights or reps each week. Try adding 2.5-5 kg (5-10 lbs) to your lifts every 2-3 weeks.",
      "   Focus on proper form before increasing weight.",
      "   Aim for 1-2 more reps per set each week to increase volume.",
      "   Rest for 60-90 seconds between sets to maintain intensity."
  ]
generateTips (ProgressionTips MuscleEndurance) = unlines [
  "Progression Tips:",
    "   Focus on increasing your workout duration and the number of repetitions.",
    "   Gradually reduce rest time between sets for improved cardiovascular endurance."
  ]
generateTips NutritionTips = unlines [
  "Nutrition:",
    "   Ensure you're consuming enough protein (around 1.6-2.2g/kg of body weight)."
  ]
generateTips RestAndRecovery = unlines [
    "Rest & Recovery:",
      "   Aim for 7-9 hours of sleep each night for optimal recovery.",
      "   Rest between 90 to 120 seconds for heavier lifts (like squats, bench press, Biceps).",
      "   Rest around 60 seconds for accessory movements (like curls or shoulder press)",
      "   Hydrate well and fuel your body with balanced meals, especially post-workout."
  ]
generateTips ExerciseAlternatives = unlines [
    "\nExercise Alternatives:",
      "   If you do not have access to free weights or machines, you can substitute exercises with bodyweight alternatives, such as push-ups, bodyweight squats, or resistance bands.",
      "   Use resistance bands or machines if you lack dumbbells or barbells for certain exercises."
  ]

-- Helper function to get the workout recommendation based on user experience and goal
getRecommendation :: Experience -> Goal -> String
getRecommendation experience goal =
    recommend (WorkoutRecommend experience goal)

-- Helper function to display and select an option from a list
selectOption :: String -> [String] -> IO (Maybe String)
selectOption prompt options =
    putStrLn prompt >>
    mapM_ (\(i, option) -> putStrLn (show i ++ ". " ++ option)) (zip [1..] options) >>
    putStr "Enter your choice: " >>
    getLine >>= \choice ->
    return (case reads choice of
        [(index, "")] | index > 0 && index <= length options -> Just (options !! (index - 1))
        _ -> Nothing)


getWorkouts :: Exercise -> [Workout]
getWorkouts exercise = case exercise of
    Abs       -> [Workout "Hanging Leg Raise" "Hang from a pull-up bar, keeping your body straight. Lift your legs straight towards your chest. Slowly lower them back to the starting position.\n",
                  Workout "Russian Twists" "Sit on the floor with your knees bent and feet lifted. Hold a weight or medicine ball with both hands. Twist your torso to the right, then to the left, while balancing on your glutes.\n",
                  Workout "Core Twist" "Stand with your feet shoulder-width apart. Hold a weight or medicine ball with both hands. Twist your torso to the right, then to the left, keeping your core engaged.\n",
                  Workout "Leg Raises" "Lie on your back with your legs straight. Lift your legs towards the ceiling, keeping them straight. Slowly lower them back down without touching the floor.\n"]

    Back      -> [Workout "Seated Row Machine" "Sit at the machine with your chest against the pad. Grasp the handles with an overhand grip. Pull the handles towards your torso, squeezing your shoulder blades.\n",
                  Workout "Lat Pulldowns" "Sit at the machine with your knees secured. Grasp the bar with a wide grip. Pull the bar down to your chest, keeping your back straight.\n",
                  Workout "Single-arm Dumbbell Row" "Place one knee and hand on a bench. Hold a dumbbell in the opposite hand. Pull the dumbbell towards your hip, keeping your back flat.\n",
                  Workout "Pull-throughs" "Attach a rope to a cable machine. Face away from the machine, holding the rope between your legs. Hinge at the hips and pull the rope through.\n",
                  Workout "Barbell Back Squats" "Place a barbell on your upper back. Stand with your feet shoulder-width apart. Squat down, keeping your chest up and back straight. Push through your heels to stand back up.\n",
                  Workout "Face Pulls" "Attach a rope to a cable machine at face height with a rope attachment. Pull the rope towards your face, squeezing your upper back and rear delts.\n",
                  Workout "Barbell Rows" "Hold a barbell with an overhand grip. Bend at the hips and knees, keeping your back flat. Pull the bar towards your torso, squeezing your shoulder blades.\n",
                  Workout "Seated Cable Rows" "Sit at the machine with your knees bent. Grasp the handles with a neutral grip. Pull the handles towards your torso, squeezing your shoulder blades.\n"]

    Biceps    -> [Workout "Dumbbell Bicep Curls" "Stand with a dumbbell in each hand. Curl the weights towards your shoulders, keeping your elbows close to your sides.\n",
                  Workout "Barbell Biceps" "Hold a barbell with an underhand grip. Curl the bar towards your shoulders, keeping your elbows close to your sides.\n",
                  Workout "Hammer Curls" "Hold a dumbbell in each hand with your palms facing each other. Curl the weights towards your shoulders, keeping your elbows close to your sides.\n",
                  Workout "EZ Bar Curls" "Hold an EZ bar with an underhand grip. Curl the bar towards your shoulders, keeping your elbows close to your sides.\n",
                  Workout "Dumbbell Biceps" "Hold a dumbbell in each hand with your palms facing up. Curl the weights towards your shoulders, keeping your elbows close to your sides.\n",
                  Workout "Biceps (Conventional)" "Hold a barbell with an underhand grip. Curl the bar towards your shoulders, keeping your elbows close to your sides.\n",
                  Workout "Barbell Biceps" "Hold a barbell with an underhand grip. Curl the bar towards your shoulders, keeping your elbows close to your sides.\n"]

    Calf      -> [Workout "Standing Calf Raises" "Stand on a block or step with your heels hanging off. Raise your heels as high as possible, then lower them below the step.\n",
                  Workout "Seated Calf Raises" "Sit on a bench with a weight on your knees. Place the balls of your feet on a block. Raise your heels as high as possible, then lower them below the block.\n",
                  Workout "Calf Raises" "Stand on the edge of a step or block. Raise your heels as high as possible, then lower them below the step.\n"]

    Chest     -> [Workout "Barbell Bench Press" "Lie on a bench with a barbell above your chest. Lower the bar to your chest, then press it back up.\n",
                  Workout "Dumbbell Chest Press" "Lie on a bench with a dumbbell in each hand. Lower the weights to your chest, then press them back up.\n",
                  Workout "Chest Fly (Cable or Dumbbells)" "Lie on a bench with a dumbbell in each hand. Open your arms wide, then bring the weights together over your chest.\n",
                  Workout "Incline Dumbbell Press" "Lie on an incline bench with a dumbbell in each hand. Lower the weights to your chest, then press them back up.\n",
                  Workout "Barbell Squats" "Stand with your feet shoulder-width apart. Squat down, keeping your chest up and back straight. Push through your heels to stand back up.\n",
                  Workout "Push-Ups" "Start in a plank position with your hands under your shoulders. Lower your chest to the floor, then push back up.\n",
                  Workout "Chest Press Machine" "Sit at the machine with your back against the pad. Push the handles forward, extending your arms.\n"]

    Forearms  -> [Workout "Barbell Shrugs" "Hold a barbell with an overhand grip. Shrug your shoulders towards your ears, then lower them back down.\n",
                  Workout "Dumbbel Front Raises" "Stand with a dumbbell in each hand. Raise the weights in front of you to shoulder height, then lower them back down.\n",
                  Workout "Reverse Wrist Curls" "Sit on a bench with a dumbbell in each hand. Curl the weights towards your body, then lower them back down.\n"]

    Legs      -> [Workout "Barbell Squats" "Stand with your feet shoulder-width apart. Squat down, keeping your chest up and back straight. Push through your heels to stand back up.\n",
                  Workout "Deadlifts" "Stand with your feet hip-width apart. Hinge at the hips and bend your knees to lower the barbell to the floor. Stand back up, pushing through your heels.\n",
                  Workout "Dumbbel Lunges" "Stand with a dumbbell in each hand. Step forward with one leg and lower your body until both knees are bent at a 90-degree angle. Push back up to the starting position.\n",
                  Workout "Leg Press Machine" "Sit at the machine with your feet shoulder-width apart. Push the platform away from you, extending your legs.\n",
                  Workout "Leg Curls (Machine)" "Lie face down on the machine with your legs straight. Curl your legs towards your glutes, then lower them back down.\n",
                  Workout "Walking Lunges" "Step forward with one leg and lower your body until both knees are bent at a 90-degree angle. Push back up and step forward with the other leg.\n",
                  Workout "Bulgarian Split Squats" "Stand with one foot on a bench behind you. Lower your body until your front thigh is parallel to the floor. Push back up to the starting position.\n",
                  Workout "Leg Extensions" "Sit at the machine with your knees bent. Extend your legs, lifting the weight with your quads.\n",
                  Workout "Goblet Squats" "Hold a dumbbell or kettlebell at chest height. Squat down, keeping your chest up and back straight. Push through your heels to stand back up.\n",
                  Workout "Romanian Biceps" "Stand with a barbell or dumbbells in front of your thighs. Hinge at the hips and lower the weight towards the floor. Stand back up, pushing through your heels.\n"]

    Shoulders -> [Workout "Dumbbell Shoulder Press" "Sit on a bench with a dumbbell in each hand. Press the weights overhead, extending your arms.\n",
                  Workout "Overhead Press (Barbell)" "Stand with a barbell on your upper chest. Press the bar overhead, extending your arms.\n",
                  Workout "Dumbbell Lateral Raises" "Stand with a dumbbell in each hand. Raise the weights out to the sides to shoulder height, then lower them back down.\n",
                  Workout "Arnold Press (Dumbbells)" "Sit on a bench with a dumbbell in each hand. Press the weights overhead while rotating your palms.\n",
                  Workout "Dumbbell Rows" "Hold a dumbbell in each hand with your palms facing each other. Row the weights towards your torso, squeezing your shoulder blades.\n",
                  Workout "Rear Delt Fly (Cable or Dumbbells)" "Stand with a dumbbell in each hand. Raise the weights out to the sides and slightly back, then lower them back down.\n",
                  Workout "Latera Raises" "Stand with a dumbbell in each hand. Raise the weights out to the sides to shoulder height, then lower them back down.\n"]

    Triceps   -> [Workout "Tricep Pushdowns (Cable)" "Attach a rope to a cable machine. Hold the rope with an overhand grip. Push the rope down, extending your arms.\n",
                  Workout "Tricep Dips" "Hold onto parallel bars with your arms straight. Lower your body by bending your elbows, then push back up.\n",
                  Workout "Cable Tricep Pushdowns" "Attach a rope to a cable machine. Hold the rope with an overhand grip. Push the rope down, extending your arms.\n",
                  Workout "Barbell Tricep" "Hold a barbell with an overhand grip. Extend your arms overhead, then lower the bar behind your head.\n"]

-- Function to log a workout
logWorkout :: String -> IO LogEntry
logWorkout workoutName =
  putStrLn "Do you want to add this workout to your log? (yes/no)" >>
  getLine >>= \response ->
  if response == "yes"
    then
      putStr "Enter the date (format: YYYY-MM-DD):" >>
       getLine >>= \date ->
       putStr "Enter the number of sets:" >>
       readLn >>= \numSets ->
       putStr "Enter the number of reps per set:" >>
       readLn >>= \numReps ->
       let logEntry = LogEntry workoutName date numSets numReps
       in putStrLn "Workout logged!" >> return logEntry
    else if response == "no"
      then putStrLn "Workout not logged." >> return (LogEntry workoutName "" 0 0) -- Return an empty log entry if not logged
    else
      putStrLn "Invalid response. Please try again." >> logWorkout workoutName


-- Function to display the log
displayLog :: [LogEntry] -> IO ()
displayLog log =
    (if null log
        then putStrLn "No workouts logged yet."
        else mapM_ displayLogEntry log) >> return () -- Return after displaying

-- Helper function to display each log entry
displayLogEntry :: LogEntry -> IO ()
displayLogEntry (LogEntry name date sets reps) = do
    putStrLn $ "Workout: " ++ name
    putStrLn $ "Date: " ++ date
    putStrLn $ "Sets: " ++ show sets
    putStrLn $ "Reps: " ++ show reps

-- Helper function to retry on invalid input
retryOnInvalid :: IO (Maybe a) -> String -> IO a
retryOnInvalid action errorMessage =
    action >>= \result ->
    case result of
        Just value -> return value
        Nothing ->
            putStrLn errorMessage >>
            retryOnInvalid action errorMessage


-- Main function to make an appointment
makeAppointment :: String -> [Availability] -> [Appointment] -> IO [Appointment]
makeAppointment userEmail coachAvailability appointments =
    let coachEmails = map emailCoach coachAvailability
    in retryOnInvalid
          (selectOption "Choose a coach:" coachEmails)
          "Invalid coach selection. Please try again."
       >>= \selectedCoachEmail ->
       let selectedCoachAvailability = find (\coach -> emailCoach coach == selectedCoachEmail) coachAvailability
       in case selectedCoachAvailability of
           Just availability ->
               retryOnInvalid
                   (selectOption "\nAvailable days:" (day availability))
                   "Invalid date selection. Please try again."
               >>= \selectedDate ->
               let timesForDate = fromMaybe [] $ lookup selectedDate (time availability)
               in retryOnInvalid
                     (selectOption "\nAvailable times:" timesForDate)
                     "Invalid time selection. Please try again."
                  >>= \selectedTime ->
                  let newAppointment = Appointment
                          { userEmail = userEmail
                          , coachEmail = selectedCoachEmail
                          , appointmentDate = selectedDate
                          , appointmentTime = selectedTime
                          }
                      updatedAppointments = newAppointment : appointments
                  in putStrLn "--------Appointment Scheduled--------" >>
                     putStrLn ("Coach: " ++ coachEmail newAppointment ++
                               " \nDate: " ++ appointmentDate newAppointment ++
                               " \nTime: " ++ appointmentTime newAppointment) >>
                     return updatedAppointments
           Nothing -> putStrLn "Selected coach not found. Aborting..." >> return appointments


-- Function to display appointments for the logged-in coach
viewCoachAppointments :: Email -> IO [Appointment] -> IO ()
viewCoachAppointments email appointments =
    appointments >>= \apt ->
    let coachAppointments = filter (\app -> email == coachEmail app) apt
    in if null coachAppointments
        then putStrLn "No appointments scheduled.\n"
        else
          putStrLn "\n--------Appointment--------" >>
            mapM_ printAppointment coachAppointments

-- Helper function to print an appointment
printAppointment :: Appointment -> IO ()
printAppointment app =
    putStrLn ("User Email: " ++ userEmail app) >>
    putStrLn ("Date: " ++ appointmentDate app) >>
    putStrLn ("Time: " ++ appointmentTime app ++ "\n")

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
    case lookup choice (map (\(label, _, description) -> (label, description)) options) of
        Just description -> putStrLn ("\n--------" ++ description ++ "--------")
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
performLogin :: [Appointment] -> [LogEntry] -> IO ()
performLogin apt log =
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
                            Just message -> putStrLn message >> userJourney userType apt log
                            Nothing -> putStrLn "Invalid credentials. Please try again."
              Nothing -> putStrLn "Invalid credentials. Please try again." >> performLogin apt log

-- Function to display user journey based on user type
userJourney :: UserType -> [Appointment] -> [LogEntry] -> IO ()
userJourney userType appointments log = case userType of
    -- For User
    User _ -> do
        putStrLn "\n***************************************"
        putStrLn "                FitGym                  "
        putStrLn "***************************************"
        action <- askQuestion :: IO Action
        -- Ask the user for their experience, goal, and workout days.
        case action of
            RecommendGymWork -> do
                experience <- askQuestion :: IO Experience
                goal <- askQuestion :: IO Goal
                putStrLn ("For a " ++ show experience ++ " focusing on building " ++ show goal ++ ".")
                -- Provide workout recommendation based on experience and goal
                let recommendation = getRecommendation experience goal
                putStrLn recommendation -- Display the recommendation to the user
                userJourney userType appointments log
            GymWork -> do
                exercise <- askQuestion :: IO Exercise
                putStrLn ("You selected: " ++ show exercise)
                let workouts = getWorkouts exercise
                putStrLn "Available workouts:"
                mapM_ (\(Workout name desc) -> putStrLn $ "- " ++ name ++ ": " ++ desc) workouts
                workoutChoice <- askGymQuestion "Which workout would you like to perform?" (zipWith (\i (Workout name _) -> (show i, name, name)) [1..] workouts)
                putStrLn ("You selected workout: " ++ workoutChoice)
                -- Log the workout
                logEntry <- logWorkout workoutChoice
                -- If logEntry is not empty, add it to the log
                let updatedLog = if workoutName logEntry /= "" then logEntry : log else log
                userJourney userType appointments updatedLog
            ViewLog -> do
                -- Display the workout log
                displayLog log
                userJourney userType appointments log
            MakeAppointment -> do
                let userEmail = getUserEmail userType
                updatedAppointments <- makeAppointment userEmail coachAvailability appointments
                -- Recurse with the updated appointments list
                userJourney userType updatedAppointments log

            GoBackToLogin -> do
                putStrLn "Returning to the login screen...\n"
                getChoice >> performLogin appointments log-- Go back to login screen  
    -- For Coach
    Coach (Credentials coachEmail _) -> do
      putStrLn "\n***************************************"
      putStrLn "             Coach Dashboard            "
      putStrLn "***************************************"
      viewCoachAppointments coachEmail (return appointments)
      action <- askQuestion :: IO Action
      case action of
          GoBackToLogin -> do
              putStrLn "\nReturning to the login screen..."
              getChoice >> performLogin appointments log-- Go back to login screen

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
              Nothing ->
                     putStrLn "Invalid choice. Please enter a number.\n" >>
                     getChoice

-- Main function
main :: IO ()
main = getChoice >>= \choice ->
       case choice of
              1 -> performLogin [] []
              2 -> putStrLn "Exiting the system. Goodbye!"
              _ ->
                     putStrLn "Invalid choice. Please try again.\n" >>
                     main -- Recursively call main to allow another attempt