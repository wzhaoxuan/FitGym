import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (find)
import GHC.Event.Windows (updateTimeout)

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
    recommend (WorkoutPlan Beginner Strength) = unlines[
       "Workout days per week: 3",
        "***3-Day Workout Plan (Full Body)***",
        "Day 1",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 3 sets of 5-8 reps (Increase weight gradually each week)",
        "   Barbell Bench Press: 3 sets of 5-8 reps (If new, use dumbbells or machine for better control)",
        "   Barbell Deadlift: 3 sets of 5 reps (Focus on form, use lighter weights initially)",
        "   Overhead Press (Barbell or Dumbbells): 3 sets of 5-8 reps",
        "   Pull-ups (Assisted if needed) or Lat Pulldown: 3 sets of 6-8 reps",
        "   Plank: 3 sets of 30-45 seconds\n",
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

    recommend (WorkoutPlan Beginner MuscleSize) = unlines[
       "Workout days per week: 3",
        "***3-Day Workout Plan (Full Body Split for Hypertrophy)***",
        "Day 1",
        generateWarmUp,
        "2. Workout:\n  Barbell Squats: 4 sets of 8-10 reps (Focus on depth and full range of motion)",
        "   Barbell Bench Press: 4 sets of 8-10 reps (Ensure a controlled motion and steady pace)",
        "   Dumbbell Row: 4 sets of 8-10 reps (Keep your back neutral, avoid swinging)",
        "   Dumbbell Shoulder Press: 3 sets of 8-10 reps (Start light to master form)",
        "   Leg Press Machine: 3 sets of 10-12 reps (Alternative for squats if new)",
        "   Tricep Pushdowns (Cable): 3 sets of 10-12 reps",
        "   Dumbbell Bicep Curls: 3 sets of 10-12 reps\n",
        generateCoolDown,
        "Day 2",
        generateWarmUp,
        "2. Workout:\n  Deadlift (Conventional or Romanian): 4 sets of 6-8 reps (Focus on hip hinge)",
        "   Incline Dumbbell Press: 4 sets of 8-10 reps (Slow and controlled descent)",
        "   Lat Pulldown or Pull-ups: 3 sets of 8-10 reps (Work full range of motion)",
        "   Dumbbell Lateral Raises: 3 sets of 12-15 reps (Focus on mind-muscle connection)",
        "   Lunges (Dumbbells or Barbell): 3 sets of 10-12 reps per leg",
        "   Leg Curls (Machine): 3 sets of 10-12 reps",
        "   Cable Face Pulls: 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 3",
        generateWarmUp,
        "2. Workout:\n    Barbell Squats: 4 sets of 8-10 reps (Gradually increase weight from Day 1)",
        "   Dumbbell Chest Press: 4 sets of 8-10 reps",
        "   Barbell Rows: 4 sets of 8-10 reps",
        "   Overhead Press (Barbell or Dumbbells): 3 sets of 8-10 rep",
        "   Dumbbell Bicep Curls: 3 sets of 10-12 reps",
        "   Tricep Dips (Bodyweight or Weighted): 3 sets of 8-10 reps",
        "   Planks: 3 sets of 30-45 seconds\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleSize),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutPlan Beginner MuscleEndurance) = unlines[
      "Workout days per week: 3",
        "***3-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1",
        generateWarmUp,
        "2. Workout:\n   Bodyweight Squats: 3 sets of 15-20 reps (Increase weight gradually each week)",
        "   Dumbbell Bench Press: 3 sets of 12-15 reps (If new, use dumbbells or machine for better control)",
        "   Dumbbell Row (each arm): 3 sets of 12-15 reps (Focus on form, use lighter weights initially)",
        "   Standing Overhead Dumbbell Press: 3 sets of 12-15 reps",
        "   Plank with Shoulder Taps: 3 sets of 30 seconds\n",
        generateCoolDown,
        "Day 2",
        generateWarmUp,
        "2. Workout:\n  Lunges (Bodyweight or Dumbbells): 3 sets of 12-15 reps per leg",
        "   Incline Dumbbell Press: 3 sets of 12-15 reps (Slow and controlled descent)",
        "   Lat Pulldown or Pull-ups: 3 sets of 12-15 reps (Work full range of motion)",
        "   Dumbbell Lateral Raises: 3 sets of 12-15 reps (Focus on mind-muscle connection)",
        "   Russian Twists (Bodyweight or Medicine Ball): 3 sets of 20 twists (10 per side)\n",
        generateCoolDown,
        "Day 3",
        generateWarmUp,
        "2. Workout:\n    Deadlifts (Dumbbells or Barbell): 3 sets of 12-15 reps",
        "   Push-Ups (Knee or Standard): 3 sets of 15-20 reps",
        "   Seated Row Machine: 3 sets of 12-15 reps",
        "   Dumbbell Step-Ups (each leg): 3 sets of 12-15 reps",
        "   Side Planks (each side): 3 sets of 20-30 seconds\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleEndurance),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]
    
    recommend (WorkoutPlan Intermediate Strength) = unlines[
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
        "   Core (Plank): 3 sets of 45-60 seconds\n",
        generateCoolDown,
        "Day 2 (Lower Body Strength)",
        generateWarmUp,
        "2. Workout:\n  Barbell Back Squats: 4 sets of 5-6 reps",
        "   Romanian Deadlifts (Barbell or Dumbbells): 4 sets of 6-8 reps",
        "   Leg Press: 3 sets of 8-10 reps",
        "   Bulgarian Split Squats (Dumbbells): 3 sets of 8-10 reps per leg",
        "   Calf Raises (Weighted or Bodyweight): 4 sets of 12-15 reps",
        "   Core (Leg Raises): 3 sets of 15 reps\n",
        generateCoolDown,
        "Day 3 (Upper Body Pull)",
        generateWarmUp,
        "2. Workout:\n    Deadlifts (Conventional or Sumo): 4 sets of 5 reps",
        "   Pull-ups (Weighted if possible): 4 sets of 5-6 reps",
        "   Barbell Rows (Pendlay or Bent-over): 4 sets of 6-8 reps",
        "   Single-arm Dumbbell Row: 3 sets of 8-10 reps per arm",
        "   Lat Pulldowns (Cable): 3 sets of 8-10 reps",
        "   Face Pulls (Cable): 3 sets of 12-15 reps\n",
        generateCoolDown,
        "Day 4 (Full Body Strength)",
        generateWarmUp,
        "2. Workout:\n    Front Squats (Barbell): 4 sets of 5 reps",
        "   Barbell Deadlifts (Heavy): 4 sets of 5 reps",
        "   Dumbbell Shoulder Press: 3 sets of 6-8 reps",
        "   Hammer Curls (Dumbbells): 3 sets of 10-12 reps",
        "   Barbell Shrugs: 3 sets of 10-12 reps",
        "   Cable Tricep Pushdowns: 3 sets of 12-15 reps\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleEndurance),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutPlan Intermediate MuscleSize) = unlines[
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
        "   Romanian Deadlifts (Dumbbells or Barbell): 3 sets of 8-10 reps",
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
        "2. Workout:\n   Barbell Deadlifts: 3 sets of 6-8 reps",
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
    
    recommend (WorkoutPlan Intermediate MuscleEndurance) = unlines[
      "Workout days per week: 3",
        "***3-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1 (Full Body Circuit)",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 4 sets of 12-15 reps. (Increase weight gradually each week)",
        "   Dumbbell Bench Press: 4 sets of 12-15 reps.",
        "   Barbell Deadlift: 3 sets of 10-12 reps.",
        "   Dumbbell Shoulder Press: 4 sets of 12-15 reps.",
        "   Lat Pulldown: 4 sets of 12-15 reps.",
        "   Plank: 3 sets of 45-60 seconds.\n",
        generateCoolDown,
        "Day 2 (Upper Body Focus)",
        generateWarmUp,
        "2. Workout:\n   Lunges (Bodyweight or Dumbbells): 4 sets of 10-12 reps per leg.",
        "   Incline Dumbbell Press: 4 sets of 12-15 reps.",
        "   Seated Cable Rows: 4 sets of 12-15 reps.",
        "   Dumbbell Lateral Raises: 3 sets of 15-20 reps.",
        "   Leg Curls (Machine): 4 sets of 12-15 reps.",
        "   Side Plank (each side): 3 sets of 30-45 seconds.\n",
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

    recommend (WorkoutPlan Advanced Strength) = unlines[
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
        "   Romanian Deadlifts: 4 sets of 6-8 reps",
        "   Leg Press (High Weight): 4 sets of 8 reps",
        "   Walking Lunges (Heavy Dumbbells): 3 sets of 12 steps per leg",
        "   Standing Calf Raises (Weighted): 3 sets of 15 reps",
        "   Plank with Weight Plate: 3 sets of 60 seconds\n",
        generateCoolDown,
        "Day 3 (Deadlift & Posterior Chain Focus)",
        generateWarmUp,
        "2. Workout:\n   Deadlifts (Conventional): 5 sets of 3-5 reps",
        "   Barbell Hip Thrusts: 4 sets of 8 reps",
        "   Barbell Rows: 4 sets of 6-8 reps",
        "   Good Mornings (Barbell or Dumbbell): 3 sets of 8-10 reps",
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
        "   Plank to Side Plank (Weighted): 3 sets of 60 seconds\n",
        generateCoolDown,
        generateTips (ProgressionTips Strength),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutPlan Advanced MuscleSize) = unlines[
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
        "   Romanian Deadlifts: 4 sets of 8-10 reps",
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
        "2. Workout:\n   Barbell Deadlifts: 4 sets of 8-10 reps",
        "   Goblet Squats (Dumbbell): 3 sets of 10-12 reps",
        "   Leg Extensions: 4 sets of 12-15 reps",
        "   Plank with Weighted Plate: 3 sets of 60 seconds",
        "   Russian Twists (Weighted): 3 sets of 40 twists",
        "   Hanging Leg Raises: 3 sets of 15-20 reps\n",
        generateCoolDown,
        generateTips (ProgressionTips MuscleSize),
        generateTips NutritionTips,
        generateTips RestAndRecovery,
        generateTips ExerciseAlternatives]

    recommend (WorkoutPlan Advanced MuscleEndurance) = unlines[
      "Workout days per week: 5",
        "***5-Day Workout Plan (Muscle Endurance Focus)***",
        "Day 1 (Full-Body Circuit)",
        generateWarmUp,
        "2. Workout:\n   Barbell Squats: 3 sets of 12-15 reps. (Increase weight gradually each week)",
        "   Pull-ups (Weighted if possible): 3 sets of 8-10 reps",
        "   Bench Press (Moderate weight): 3 sets of 12-15 reps",
        "   Dumbbell Lunges (Per leg): 3 sets of 10 reps",
        "   Kettlebell Swings: 3 sets of 20 reps",
        "   Plank with Shoulder Taps: 3 sets of 60 seconds\n",
        generateCoolDown,
        "Day 2 (Upper Body Focus)",
        generateWarmUp,
        "2. Workout:\n  Push-ups (Weighted or Deficit): 3 sets of 20 reps",
        "   Seated Row Machine: 3 sets of 15-20 reps",
        "   Arnold Press (Dumbbells): 3 sets of 12-15 reps",
        "   Incline Dumbbell Fly: 3 sets of 15 reps",
        "   Lateral Raises: 3 sets of 15 reps",
        "   Russian Twists (Weighted): 3 sets of 40 twists\n",
        generateCoolDown,
        "Day 3 (Lower Body Focus)",
        generateWarmUp,
        "2. Workout:\n  Deadlifts (Conventional or Romanian): 3 sets of 8-10 reps",
        "   Step-ups with Dumbbells: 3 sets of 15 reps per leg",
        "   Calf Raises (Bodyweight or Weighted): 3 sets of 25 reps",
        "   Wall Sit (with Weight): 3 sets of 60 seconds",
        generateCoolDown,
        "Day 4 (HIIT and Core)",
        generateWarmUp,
        "2. Workout:\n  Rowing Machine (Intervals): 5 rounds: 40 seconds sprint, 20 seconds rest",
        "   Burpees: 3 sets of 15 reps",
        "   Mountain Climbers: 3 sets of 30 seconds",
        "   Hanging Leg Raises: 3 sets of 12-15 reps",
        "   Flutter Kicks: 3 sets of 30 seconds",
        generateCoolDown,
        "Day 5 (Full-Body Superset Workout)",
        generateWarmUp,
        "2. Workout:\n  Barbell Front Squats + Overhead Press: 3 sets of 10-12 reps each",
        "   Dumbbell Deadlifts + Pull-ups: 3 sets of 8-10 reps each",
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
      "   Rest between 90 to 120 seconds for heavier lifts (like squats, bench press, deadlifts).",
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
  putStrLn "--------Appointment Scheduled--------" 
  putStrLn ("Coach: " ++ coachEmail newAppointment ++ 
      " \nDate: " ++ appointmentDate newAppointment ++ 
      " \nTime: " ++ appointmentTime newAppointment)
  return updatedAppointments

-- Function to display appointments for the logged-in coach
viewCoachAppointments :: Email -> IO [Appointment] -> IO ()
viewCoachAppointments email appointments = 
    appointments >>= \apt ->
    let coachAppointments = filter (\app -> email == coachEmail app) apt
    in if null coachAppointments
        then putStrLn "No appointments scheduled.\n"
        else do
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
performLogin :: [Appointment] -> IO ()
performLogin apt = 
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
                            Just message -> putStrLn message >> userJourney userType apt
                            Nothing -> putStrLn "Invalid credentials. Please try again."
              Nothing -> putStrLn "Invalid credentials. Please try again." >> performLogin apt

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
                performLogin appointments -- Go back to login screen
    -- For Coach
    Coach (Credentials coachEmail _) -> do
      putStrLn "\n***************************************"
      putStrLn "             Coach Dashboard            " 
      putStrLn "***************************************"
      viewCoachAppointments coachEmail (return appointments)
      -- action <- askQuestion :: IO Action
      -- case action of
      --     GoBackToLogin -> do
      --         putStrLn "\nReturning to the login screen..."
      --         performLogin appointments-- Go back to login screen

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
              1 -> performLogin []
              2 -> putStrLn "Exiting the system. Goodbye!"
              _ -> do
                     putStrLn "Invalid choice. Please try again.\n"
                     main -- Recursively call main to allow another attempt