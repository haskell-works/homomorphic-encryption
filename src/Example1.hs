module Example1 where

import Data.Monoid

require :: Bool -> String -> IO ()
require True _ = return ()
require False message = error ("Require: " <> message)

newtype Crypt = Crypt { getCrypt :: Integer }

instance Show Crypt where
  show (Crypt v) = show v

instance Num Crypt where
  (Crypt a) + (Crypt b) = Crypt (a + b)
  (Crypt a) * (Crypt b) = Crypt (a * b)

λ = 5 :: Integer
theN = λ
theP = λ ^ 2
theQ = λ ^ 5

p = 23994432 :: Integer
q = 52328278781835085283520572288706460827587665448136471518820885888616888786538787568456115520655575006388487633467321503783224008528863465412684208662785551771055478304114154108312868438286847165868610106536855480053508658741758868581282030546053333238177002578611618026120385136872384508888671606552514888558461378435486613712352565322877746684768802781388512815343301066316425278086787645535116304646473086107336514780344304511884316142818825211774628621262704646141545576183585530678828324118820432700003604673185207241264772017502285762617185634848421228112540013406827868783087656368075878547733754828574840727360715413048811284563518126487050440477882471771474734874654803076231423443367281682536250388817340434164184580204876038823776561275326307567114486460184508815888307340388232818213886753583100430573176753134100040764880520524368711758763868603838211166007450358787283206138876542501777610455326513450434370685277350467175186432 :: Integer

encrypt noise message = Crypt ((noise * 2 + message) + (p * q))
decrypt (Crypt cipher) = (cipher `mod` p) `mod` 2
noise (Crypt cipher) = cipher `mod` p

r0 = 7 :: Integer
r1 = 9 :: Integer

m0 = 0 :: Integer
m1 = 1 :: Integer

c0 = encrypt r0 m0
c1 = encrypt r1 m1

d0 = decrypt c0
d1 = decrypt c1

run :: IO ()
run = do
  require (p < 2 ^ theP) "p < 2 ^ theP"

  putStrLn $ "m0 = " <> show m0
  putStrLn $ "m1 = " <> show m1

  putStrLn $ "c0 = " <> show c0
  putStrLn $ "c1 = " <> show c1

  putStrLn $ "noise c0 = " <> show (noise c0)
  putStrLn $ "noise c1 = " <> show (noise c1)

  putStrLn $ "decrypt c0 = " <> show (decrypt c0)
  putStrLn $ "decrypt c1 = " <> show (decrypt c1)

  putStrLn $ "decrypt (c0 + c0) = " <> show (decrypt (c0 + c0))
  putStrLn $ "decrypt (c0 + c1) = " <> show (decrypt (c0 + c1))
  putStrLn $ "decrypt (c1 + c0) = " <> show (decrypt (c1 + c0))
  putStrLn $ "decrypt (c1 + c1) = " <> show (decrypt (c1 + c1))

  putStrLn $ "decrypt (c0 + c0 + c0) = " <> show (decrypt (c0 + c0 + c0))
  putStrLn $ "decrypt (c0 + c0 + c1) = " <> show (decrypt (c0 + c0 + c1))
  putStrLn $ "decrypt (c0 + c1 + c0) = " <> show (decrypt (c0 + c1 + c0))
  putStrLn $ "decrypt (c0 + c1 + c1) = " <> show (decrypt (c0 + c1 + c1))
  putStrLn $ "decrypt (c1 + c0 + c0) = " <> show (decrypt (c1 + c0 + c0))
  putStrLn $ "decrypt (c1 + c0 + c1) = " <> show (decrypt (c1 + c0 + c1))
  putStrLn $ "decrypt (c1 + c1 + c0) = " <> show (decrypt (c1 + c1 + c0))
  putStrLn $ "decrypt (c1 + c1 + c1) = " <> show (decrypt (c1 + c1 + c1))

  putStrLn $ "decrypt (c0 * c0) = " <> show (decrypt (c0 * c0))
  putStrLn $ "decrypt (c0 * c1) = " <> show (decrypt (c0 * c1))
  putStrLn $ "decrypt (c1 * c0) = " <> show (decrypt (c1 * c0))
  putStrLn $ "decrypt (c1 * c1) = " <> show (decrypt (c1 * c1))

  putStrLn $ "decrypt (c0 * c0 * c0) = " <> show (decrypt (c0 * c0 * c0))
  putStrLn $ "decrypt (c0 * c0 * c1) = " <> show (decrypt (c0 * c0 * c1))
  putStrLn $ "decrypt (c0 * c1 * c0) = " <> show (decrypt (c0 * c1 * c0))
  putStrLn $ "decrypt (c0 * c1 * c1) = " <> show (decrypt (c0 * c1 * c1))
  putStrLn $ "decrypt (c1 * c0 * c0) = " <> show (decrypt (c1 * c0 * c0))
  putStrLn $ "decrypt (c1 * c0 * c1) = " <> show (decrypt (c1 * c0 * c1))
  putStrLn $ "decrypt (c1 * c1 * c0) = " <> show (decrypt (c1 * c1 * c0))
  putStrLn $ "decrypt (c1 * c1 * c1) = " <> show (decrypt (c1 * c1 * c1))

  print (2 ^ theQ)

  return ()
