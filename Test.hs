-- Assuming Appointment data type is something like this
data Appointment = Appointment {
    userEmail :: String,
    coachEmail :: String,
    appointmentDate :: String,
    appointmentTime :: String
} deriving (Show)

-- Function to display appointments for the logged-in coach
viewCoachAppointments :: String -> [Appointment] -> IO ()
viewCoachAppointments email appointments = do
    let coachAppointments = filter (\app -> coachEmail app == email) appointments
    putStrLn $ "Filtered appointments for coach " ++ email ++ ":"
    print coachAppointments  -- Debugging: Print the filtered list
    if null coachAppointments
        then putStrLn "No appointments scheduled.\n"
        else do
            putStrLn "\nAppointments scheduled for you:"
            mapM_ printAppointment coachAppointments

-- Helper function to print an appointment
printAppointment :: Appointment -> IO ()
printAppointment app = do
    putStrLn "***************************************"
    putStrLn ("User Email: " ++ userEmail app)
    putStrLn ("Date: " ++ appointmentDate app)
    putStrLn ("Time: " ++ appointmentTime app)
    putStrLn "***************************************"

-- Sample data to test
appointments :: [Appointment]
appointments = [
    Appointment "user1@example.com" "coach@example.com" "2024-12-15" "10:00 AM",
    Appointment "user2@example.com" "coach@example.com" "2024-12-16" "11:00 AM"
    ]

-- Testing the function with a coach email
main :: IO ()
main = do
    viewCoachAppointments "coach@example.com" appointments
