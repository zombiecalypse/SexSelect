{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Random
import Control.Applicative

import Control.Concurrent.ParallelIO

import Data.List (zipWith, sortBy, replicate, intercalate)
import Data.IORef

import Data.List.Split (splitOn)

import qualified Data.ByteString.Lazy as T

import qualified Data.Aeson as A

import Data.Aeson ((.=))

import Options

import System.IO

newtype MutationRate = MutationRate Double
  deriving (Show, Eq, Ord)
newtype Goal = Goal [Bool] -- ^ This is the goal vector, but it might change!
  deriving (Show, Eq, Ord)
newtype Offspring = Offspring Int
  deriving (Show, Eq, Ord)
newtype Survival = Survival Int
  deriving (Show, Eq, Ord)

data Genome = Genome
  { sexual :: Bool
  , fitness :: [Bool]
  } deriving (Show, Eq, Ord)

data World = World
  { worldGoal :: Goal
  , worldGenome :: [Genome]
  } deriving (Show)

instance A.ToJSON Goal where
  toJSON (Goal g) = A.toJSON g

instance A.ToJSON Genome where
  toJSON (Genome s f) = A.object
    [ "sexual" .= s
    -- , "vec" .= f
    ]

instance A.ToJSON World where
  toJSON (World { worldGoal = g, worldGenome = gen }) =
    let serialize genome = A.object 
          [ "sexual" .= sexual genome
          , "fitness" .= fitnessEval g genome
          ]
    in A.toJSON $ map serialize gen

getChance :: (RandomGen g) => Rand g Double
getChance = getRandomR (0, 1)

withProb :: (RandomGen g) => Double -> a -> a -> Rand g a
withProb p x y = do
  r <- getChance
  return $ if r < p then x else y

choice :: (RandomGen g) => [a] -> Rand g (Maybe a)
choice [] = return Nothing
choice (x:xs) = Just <$> choice' 0 x xs
  where
    choice' :: (RandomGen g) => Double -> a -> [a] -> Rand g a
    choice' i d [] = return d
    choice' i d (x:xs) = do
      newD <- withProb (1/i) x d
      choice' (i+1) newD xs

mutateGene :: (RandomGen g) => MutationRate -> Bool -> Rand g Bool
mutateGene (MutationRate mt) b = withProb mt (not b) b

mutate :: (RandomGen g) => MutationRate -> Genome -> Rand g Genome
mutate m g =
  Genome <$> mutateGene m (sexual g) <*> mapM (mutateGene m) (fitness g)

crossover :: (RandomGen g) => Genome -> Genome -> Rand g Genome
crossover g1 g2 = do
  when (not (sexual g1 && sexual g2)) $ fail "Nonsexual sex?!"
  let f1 = fitness g1
      f2 = fitness g2
      c  = zip f1 f2
      choose (x,y) = withProb 0.5 x y
  Genome True <$> mapM choose c

fitnessEval :: Goal -> Genome -> Double
fitnessEval (Goal bs) (Genome { fitness = f }) =
  sum $ zipWith (\x y -> if x == y then 1 else 0) bs f

bestPopulation :: Survival -> Goal -> [Genome] -> [Genome]
bestPopulation (Survival n) goal population =
  let compare' x y = fitnessEval goal x `compare` fitnessEval goal y
  in take n $ sortBy compare' population

reproduce :: (RandomGen g) => MutationRate -> Offspring -> [Genome] ->
                                Genome -> Rand g [Genome]
reproduce mr (Offspring n) _ g@(Genome { sexual = False }) =
  mapM (mutate mr) $ replicate n g
reproduce mr (Offspring n) population g@(Genome { sexual = True }) = do
  mate' <- choice $ filter sexual population
  case mate' of
    Nothing -> return []
    Just mate -> do
      crossed <- mapM (\_ -> crossover mate g) [1..n `div` 2]
      mapM (mutate mr) crossed

updatePopulation :: (RandomGen g) =>
                      MutationRate -> Survival ->
                        Offspring -> Goal -> [Genome] -> Rand g [Genome]
updatePopulation mr s os goal population = do
  pop' <- mapM (reproduce mr os population) population
  let pop = concat pop'
  return $ bestPopulation s goal pop

updateWorld :: (RandomGen g) =>
                  MutationRate -> MutationRate -> Survival -> 
                    Offspring -> Goal -> [Genome] -> Rand g (Goal, [Genome])
updateWorld goalMutation mr s os goal@(Goal goalvec) population =
  (,) <$> (Goal <$> mapM (mutateGene goalMutation) goalvec)
      <*> updatePopulation mr s os goal population

dumpWorld :: Goal -> [Genome] -> T.ByteString
dumpWorld goal genome =
  A.encode $ World goal genome

instance Random Genome where
  randomR ((Genome { fitness = f1 }), _) g =
    let len = length f1
        (sex, g') = random g
        addRand (l, gen) _ = case random gen of
          (el, gen') -> (el:l, gen')
        (f', g'') = foldl addRand ([], g') [1..len]
    in (Genome sex f', g'')
  random g =
    let gene = Genome True $ replicate 100 True
    in randomR (gene, gene) g

data CmdOptions = CmdOptions
  { cmdGoalChange :: [Double]
  , cmdMutationRate :: [Double]
  , cmdOffspring :: [Int]
  , cmdPopsize :: [Int]
  , cmdGenomesize :: [Int]
  , cmdGenerations :: Int
  }

listOption :: SimpleOptionType a => String -> String -> DefineOptions [a]
listOption name doc = defineOption t mod
  where t = optionType_list ',' simpleOptionType
        mod o =  o { optionLongFlags = [name]
                   , optionDefault = []
                   , optionDescription = doc
                   }

instance Options CmdOptions where
  defineOptions = CmdOptions
    <$> listOption "goal-change" "Mutation rate of the goal"
    <*> listOption "mutation-rate" "Mutation rate of the population"
    <*> listOption "offspring" "Number of children each member has"
    <*> listOption "population" "Population size"
    <*> listOption "genome" "Genome size"
    <*> simpleOption "generations" 100 "Number of generations to run"

runSingle :: String -> MutationRate -> MutationRate ->
               Offspring -> Survival -> Int -> Int -> IO ()
runSingle prefix goalMr@(MutationRate gmr') mr@(MutationRate mr')
          offspring@(Offspring offspring') survival@(Survival survival')
          genome generations = do
  let filename = prefix ++ "-" ++ intercalate "-" [ show gmr'
                                                  , show mr'
                                                  , show offspring'
                                                  , show survival'
                                                  , show genome
                                                  ] ++ ".json"
  file <- openFile filename WriteMode
  let goal = Goal $ replicate genome True
      template = Genome True $ replicate genome True
  population' <- getRandomRs (template, template)
  let population = take survival' population'
  goalRef <- newIORef goal
  geneRef <- newIORef population
  forM_ [1..generations] $ \_ -> do
    g <- readIORef goalRef
    pop <- readIORef geneRef
    (g', genes) <- evalRandIO $ updateWorld goalMr mr 
                                          survival offspring g pop
    writeIORef goalRef g'
    writeIORef geneRef genes
    T.hPutStr file $ dumpWorld g' genes
    T.hPutStr file "\n"
  hClose file

mainCmd :: CmdOptions -> [String] -> IO ()
mainCmd opts [] = mainCmd opts ["exp"]
mainCmd opts (prefix:o) = do
  let all = do
              goal <- cmdGoalChange opts
              mr <- cmdMutationRate opts
              offspring <- cmdOffspring opts
              pop <- cmdPopsize opts
              genome <- cmdGenomesize opts
              return (MutationRate goal, MutationRate mr, Offspring offspring
                     , Survival pop, genome)
  let run (goal, mr, offspring, pop, genome) =
        runSingle prefix goal mr offspring pop genome (cmdGenerations opts)
      actions = map run all
  void $ parallelInterleaved actions

main = runCommand mainCmd
