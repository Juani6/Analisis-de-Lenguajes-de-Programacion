module Main where
import Control.Monad.IO.Class
import Data.Char

helloWorld :: IO ()
helloWorld = do putStrLn "Hello world"

getChars2 :: Int -> IO String
getChars2 n = if n == 0 then return []
                        else do c <- getChar
                                cs <- getChars2 (n-1)
                                return (c:cs)
adivinar :: IO () 
adivinar = do ns <- adivinar'
              putStr "El numero era: "
              putStrLn ns      

adivinar' :: IO String
adivinar' = do n <- getLine --string
               if read n > 67 then do putStrLn "Muy grande"; adivinar'
                              else if read n < 67 then do putStrLn "Muy chico"; adivinar'
                                                  else do putStrLn "Genio!"; return n 
              
type Tablero = [String] -- Cada string es una fila

printTablero :: Tablero -> IO ()
printTablero []     = return ()
printTablero (x:xs) = do putStrLn x
                         printTablero xs

initTablero :: Tablero
initTablero = ["*****","****","***","**","*"]

tomarDeTablero :: Int -> Int -> Tablero -> Tablero
tomarDeTablero _ _ [] = []
tomarDeTablero n k xs = let ys = xs !! n in if k >= length ys 
                                            then eliminarDeTablero n xs     
                                            else insertarEnTablero n (replicate (length ys-k) '*') xs 

eliminarDeTablero :: Int -> Tablero -> Tablero
eliminarDeTablero _ []     = []
eliminarDeTablero 0 (x:xs) = xs
eliminarDeTablero n (x:xs) = x : eliminarDeTablero (n-1) xs

insertarEnTablero :: Int -> String -> Tablero -> Tablero
insertarEnTablero 0 s xs     = s : (eliminarDeTablero 0 xs)
insertarEnTablero n s (x:xs) = x : insertarEnTablero (n-1) s xs 

nim :: IO ()
nim = nim' initTablero

turnoJugador :: Int -> Tablero -> IO Tablero
turnoJugador m xs = do printTablero xs
                       putStrLn $ "Turno Jugador " ++ show m ++ ": Elegi fila"
                       n <- getLine
                       putStrLn $ "Turno Jugador " ++ show m ++ ": Cuantos tomas"
                       k <- getLine
                       return $ tomarDeTablero (read n) (read k) xs

nim' :: Tablero -> IO ()
nim' xs = do xs1 <- turnoJugador 1 xs
             if xs1 == [] then do putStrLn "Gano jugador 1"
                          else do xs2 <- turnoJugador 2 xs1; if xs2 == [] 
                                                          then do putStrLn "Gano jugador2"; return ()
                                                          else do nim' xs2

archivoToUpper' :: FilePath -> FilePath -> IO ()
archivoToUpper' f1 f2 = do xs <- readFile f1
                           writeFile f2 (map toUpper xs)
                           return ()

archivoToUpper :: IO ()
archivoToUpper = do putStrLn "Ingresar archivo de entrada: "
                    f1 <- getLine
                    putStrLn "Ingresar archivo de salida: "
                    f2 <- getLine
                    archivoToUpper' f1 f2

main :: IO ()
main = archivoToUpper