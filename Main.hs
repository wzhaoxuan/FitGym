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

main :: IO ()
main = do
       choice <- getChoice
       case choice of
            1 -> putStrLn "Login as User"
            2 -> putStrLn "Login as Coach"
            3 -> putStrLn "Exiting"
            _ -> putStrLn "Invalid choice"
                