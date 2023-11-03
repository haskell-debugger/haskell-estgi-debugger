{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE LambdaCase         #-}
module Region
  ( reportRegions
  , customCommandRegions
  , customCommandRegionInstances
  , getRegionHeap
  ) where

import Text.Printf
import Control.Monad.IO.Class
import Control.Monad.State

import           Data.String.Conversions               (cs)
import qualified Data.IntMap.Strict                    as IntMap
import qualified Data.Map.Strict                       as Map
import           Data.Map.Strict                       ( Map )

import           Stg.Interpreter.Base                  hiding (lookupEnv, getCurrentThreadState, Breakpoint, Region)

import DAP
import DapBase
import CustomDapTypes
import CustomCommands

customCommandRegions :: Adaptor ESTG ()
customCommandRegions = do
  {-
    region instance map
      - calc region name
      - calc region heap size
  -}
  StgState{..} <- getStgState
  regions <- forM (Map.toList ssRegionInstances) $ \(r, instances) -> do
    let name = case r of
          IRRegion{}      -> cs $ show r
          EventRegion{..} -> cs regionName
    pure Region
      { regionName          = name
      , regionInstanceCount = length instances
      }
  sendSuccesfulResponse . setBody $ RegionsResponse
    { regionsResponseRegions = regions
    }

customCommandRegionInstances :: Adaptor ESTG ()
customCommandRegionInstances = do
  RegionInstancesArguments {..} <- getArguments
  StgState{..} <- getStgState
  let region = EventRegion $ cs regionInstancesArgumentsRegionName
  regionInstances <- case Map.lookup region ssRegionInstances of
    Nothing        -> pure []
    Just instances -> forM (IntMap.toList instances) $ \(idx, (start, end)) -> do
      let heap = getRegionHeap (asNextHeapAddr start) (asNextHeapAddr end) ssHeap
      varsRef <- getVariablesRef $ VariablesRef_RegionInstance region idx
      pure RegionInstance
        { regionInstanceInstanceId          = idx
        , regionInstanceObjectCount         = IntMap.size heap
        , regionInstanceVariablesReference  = varsRef
        }
  sendSuccesfulResponse . setBody $ RegionInstancesResponse
    { regionInstancesResponseRegionInstances = regionInstances
    }

---------------------
reportRegions :: Adaptor ESTG ()
reportRegions = do
  stgState@StgState{..} <- getStgState
  evalStateT (mapM_ parseRegion $ reverse ssTraceMarkers) $ RegionState Map.empty stgState Map.empty

{-
  estgi.debug.region.start

  logInfo "Running garbage collection...done"
  , ssTraceMarkers        :: ![(String, Int, AddressState)]

getRegionHeap :: Int -> Int -> M Heap
getRegionHeap start end = do
  heap <- gets ssHeap
  let ltEnd   = fst $ IntMap.split end heap
      geStart = snd $ IntMap.split (start-1) ltEnd
  pure geStart

showRegion :: Bool -> String -> String -> M ()
showRegion doHeapDump start end = do
  regions <- gets ssRegions
  let r = Region (BS8.pack start) (BS8.pack end)
      printDelimiter = when doHeapDump $ liftIO $ putStrLn "\n==============================================================================\n"
  case Map.lookup r regions of
    Nothing       -> pure ()
    Just (cur, _curCallGraph, l) -> do
      liftIO $ putStrLn $ "region data count: " ++ show (length l)
      liftIO $ putStrLn $ "order:  OLD -> NEW"
      forM_ (reverse l) $ \(s, e) -> do
        printDelimiter
        let sAddr = asNextHeapAddr s
            eAddr = asNextHeapAddr e
        rHeap <- getRegionHeap sAddr eAddr
        liftIO $ printf "heap start: %-10d  end: %-10d  object count: %d\n" sAddr eAddr (IntMap.size rHeap)
        when doHeapDump $ do
          liftIO $ putStrLn ""
          dumpHeapM rHeap
          liftIO $ putStrLn ""
      printDelimiter
-}

data RegionInstanceData
  = RegionInstanceData
  { ridStart  :: AddressState
  , ridEnd    :: AddressState
  , ridHeap   :: Heap
  }

type R = StateT RegionState (Adaptor ESTG)

data RegionState
  = RegionState
  { rsRegionStack :: Map (Int, String) [AddressState]
  , rsStgState    :: StgState
  , rsInstances   :: Map String [RegionInstanceData]
  }

  {-
    TODO:
      parse region stack
      report on the fly
  -}
parseRegion :: (String, Int, AddressState) -> R ()
parseRegion (msg, tid, addrState) = do
  case words msg of
    ["estgi.debug.region.start", regionName] -> do
      pushRegion tid regionName addrState

    ["estgi.debug.region.end", regionName] -> do
      popRegion tid regionName >>= \case
        Nothing -> lift .logError . cs $ "missing region start for: " ++ show (tid, regionName)
        Just startAddrState -> do
          reportRegion regionName startAddrState addrState

    _ -> pure ()

pushRegion :: Int -> String -> AddressState -> R ()
pushRegion tid regionName addrState = modify' $ \s@RegionState{..} -> s {rsRegionStack = Map.insertWith (++) (tid, regionName) [addrState] rsRegionStack}

popRegion :: Int -> String -> R (Maybe AddressState)
popRegion tid regionName = do
  Map.lookup (tid, regionName) <$> gets rsRegionStack >>= \case
    Nothing -> pure Nothing
    Just (x : xs) -> do
      modify' $ \s@RegionState{..} -> s {rsRegionStack = Map.insert (tid, regionName) xs rsRegionStack}
      pure $ Just x

reportRegion :: String -> AddressState -> AddressState -> R ()
reportRegion regionName start end = do
  let sAddr = asNextHeapAddr start
      eAddr = asNextHeapAddr end
  rHeap <- getRegionHeapM sAddr eAddr
  let instanceData = RegionInstanceData
        { ridStart  = start
        , ridEnd    = end
        , ridHeap   = rHeap
        }
  modify' $ \s@RegionState{..} -> s {rsInstances = Map.insertWith (++) regionName [instanceData] rsInstances}
  let str :: String
      str = printf "region: %-10s   heap start: %-10d  end: %-10d  object count: %d\n" regionName sAddr eAddr (IntMap.size rHeap)
  lift . logInfo $ cs str

getRegionHeap :: Int -> Int -> Heap -> Heap
getRegionHeap start end heap = geStart
  where
    ltEnd   = fst $ IntMap.split end heap
    geStart = snd $ IntMap.split (start-1) ltEnd

getRegionHeapM :: Int -> Int -> R Heap
getRegionHeapM start end = gets $ getRegionHeap start end . ssHeap . rsStgState
