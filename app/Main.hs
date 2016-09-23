
module Main where
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)

-- TODO: bench judy, vector, judy-tight, bloom-filter

import Data.Judy
import qualified Data.List as L
import qualified Data.IntMap.Strict as I
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed.Mutable as UV

import System.Random
import System.Environment (getArgs)

main :: IO ()
main = do
  [m,a',b] <- getArgs
  let a = read a'
      g = mkStdGen 4711
  case m of
    "j" ->   printmap_J =<< freqCount_J   (take a $ randomRs (0, read b::Key) g)
    "jw" ->  printmap_J =<< freqCount_Jiw (take a $ randomRs (0, read b::Key) g)
    "i" ->   printmap_I =<< freqCount_I   (take a $ randomRs (0, read b::Int) g)
    "v" ->   printmap_V =<< freqCount_V (read b) (take a $ randomRs (0, read b::Int) g)
    "vu" ->   printmap_U =<< freqCount_U (read b) (take a $ randomRs (0, read b::Int) g)
    "vm" ->   printmap_UV =<< freqCount_UV (read b) (take a $ randomRs (0, read b::Int) g)
    "vs" ->   printmap_S =<< freqCount_S (read b) (take a $ randomRs (0, read b::Int) g)
    "l" ->   printmap_L =<< freqCount_L   (take a $ randomRs (0, read b::Int) g)
    _  ->   return ()

-- --------------------------------------------------
-- IntMap - for reference

freqCount_I :: [Int] -> IO (I.IntMap Int)
freqCount_I = return . I.fromListWith (+) . map (\x -> (x,1::Int))

printmap_I :: I.IntMap Int -> IO ()
printmap_I m = mapM_ (putStrLn . show . fromMaybe 0) $ map (flip I.lookup m) [0..30::Int]

-- --------------------------------------------------
-- Vector

freqCount_V :: Int -> [Int] -> IO (V.Vector Int)
freqCount_V v = return . V.accum (+) v0 . map (\x -> (x,1))
  where v0 = V.replicate (v+1) 0

printmap_V :: V.Vector Int -> IO ()
printmap_V v = mapM_ (putStrLn . show) $ map ((V.!) v) [0..30::Int]

-- --------------------------------------------------
-- Vector

freqCount_U :: Int -> [Int] -> IO (U.Vector Int)
freqCount_U v = return . U.accum (+) v0 . map (\x -> (x,1))
  where v0 = U.replicate (v+1) 0

printmap_U :: U.Vector Int -> IO ()
printmap_U v = mapM_ (putStrLn . show) $ map ((U.!) v) [0..30::Int]

-- --------------------------------------------------
-- Storable Vector

freqCount_S :: Int -> [Int] -> IO (S.Vector Int)
freqCount_S v = return . S.accum (+) v0 . map (\x -> (x,1))
  where v0 = S.replicate (v+1) 0

printmap_S :: S.Vector Int -> IO ()
printmap_S v = mapM_ (putStrLn . show) $ map ((S.!) v) [0..30::Int]

-- --------------------------------------------------
-- Unsafe/Unboxed/Mutable Vector

freqCount_UV :: Int -> [Int] -> IO (UV.IOVector Int)
freqCount_UV v xs = do
  v0 <- UV.replicate (v+1) 0
  mapM_ (insV v0) xs
  return v0

insV :: UV.IOVector Int -> Int -> IO ()
insV v x = do
  val <- UV.unsafeRead v x  -- NSFT!
  UV.unsafeWrite v x (val+1)

printmap_UV :: UV.IOVector Int -> IO ()
printmap_UV v = mapM_ (putStrLn . show) =<< mapM (UV.unsafeRead v) [0..30::Int]

-- --------------------------------------------------
-- Judy trees

-- freqCount :: (JE a, Integral a) => [Key] -> IO (JudyL a)
freqCount_J :: [Key] -> IO (JudyL Int)
freqCount_J xs = do
  m <- new
  mapM_ (ins1 m) xs
  return m

-- ins1 :: (JE a, Integral a) => JudyL a -> Key -> IO ()
-- is this really atomic?  don't think so.  NSFT!
ins1 :: JudyL Int -> Key -> IO ()
ins1 m k = do
  mv <- lookup k m
  insert k (1+fromMaybe 0 mv) m

printmap_J :: JudyL Int -> IO ()
printmap_J m = mapM_ (putStrLn . show . fromMaybe 0) =<< mapM (flip lookup m) [0..30::Key]

-- --------------------------------------------------
-- Judy trees

-- freqCount :: (JE a, Integral a) => [Key] -> IO (JudyL a)
freqCount_Jiw :: [Key] -> IO (JudyL Int)
freqCount_Jiw xs = do
  m <- new
  mapM_ (\x -> insertWith (+) x 1 m) xs
  return m

-- --------------------------------------------------
-- List - for reference

freqCount_L :: [Int] -> IO [(Int,Int)]
freqCount_L = return . map (\x -> (x,1::Int))

printmap_L :: [(Int,Int)] -> IO ()
printmap_L [] = return ()
printmap_L m = mapM_ print (map (flip L.lookup m) [0..30::Int])
