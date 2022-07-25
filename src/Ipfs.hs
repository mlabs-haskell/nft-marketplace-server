module Ipfs
  ( encodeBase32InBase36,
    ipfsAdd,
    CID (..),
  )
where

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Float (float2Int, int2Float)
import Network.IPFS.API (ApiV0Add)
import Servant (Proxy (..))
import Servant.Client (ClientEnv, client, runClientM)

ipfsAddApi :: Proxy ApiV0Add
ipfsAddApi = Proxy

newtype CID = CID {unCID :: Text}

data Base32 = Base32 {prefix32 :: String, name32 :: String, alphabet32 :: String, bitsPerChar32 :: Int}
  deriving stock (Show)

data Base36 = Base36 {prefix36 :: String, name36 :: String, alphabet36 :: String}
  deriving stock (Show)

data Out = Out {buffer :: Int, bits :: Int, written :: Int, out :: [Int]}
  deriving stock (Show)

ipfsAdd :: ClientEnv -> ByteString -> App (Either String CID)
ipfsAdd envIpfsClientEnv fileContents = do
  result <- liftIO $ runClientM query envIpfsClientEnv
  case result of
    Left e -> do
      let msg = "Error making an ipfs client request: " <> show e
      liftIO $ putStrLn msg
      pure $ Left msg
    Right (Object obj)
      | Just (String hash) <- KeyMap.lookup "Hash" obj ->
        pure . Right $ CID hash
    Right json -> do
      let msg = "Error making an ipfs client request: wrong response format: " <> show json
      liftIO $ putStrLn msg
      pure $ Left msg
  where
    ipfsClientAdd = client ipfsAddApi

    query =
      ipfsClientAdd
        (BSL.fromStrict fileContents)
        Nothing -- recursive
        Nothing -- quiet
        Nothing -- quieter
        Nothing -- silent
        Nothing -- progress
        Nothing -- trickle
        Nothing -- only-hash
        Nothing -- wrap-with-directory
        Nothing -- hidden
        Nothing -- chunker
        (Just True) -- pin
        Nothing -- raw-leaves
        Nothing -- nocopy
        Nothing -- fscache
        (Just 1) -- cid-version
        Nothing -- hash

base32 :: Base32
base32 = Base32 {prefix32 = "b", name32 = "base32", alphabet32 = "abcdefghijklmnopqrstuvwxyz234567", bitsPerChar32 = 5}

base36 :: Base36
base36 = Base36 {prefix36 = "k", name36 = "base36", alphabet36 = "0123456789abcdefghijklmnopqrstuvwxyz"}

decodeBase32 :: String -> String -> Int -> [Int]
decodeBase32 string alphabet bitsPerChar =
  out $ foldl writeOut (Out {buffer = 0, bits = 0, written = 0, out = []}) end
  where
    insertInCodes :: [(Int, Char)] -> Map.Map Char Int -> Map.Map Char Int
    insertInCodes [(i, a)] codesMap = Map.insert a i codesMap
    insertInCodes ((i, a) : xs) codesMap = insertInCodes xs (Map.insert a i codesMap)
    insertInCodes _ codesMap = codesMap

    codes :: Map.Map Char Int
    codes = insertInCodes (zip [0 ..] alphabet) Map.empty

    end :: String
    end = filter (/= '=') string

    writeOut :: Out -> Char -> Out
    writeOut acc c =
      let value = fromJust $ Map.lookup c codes
          newBuffer = (buffer acc `shiftL` bitsPerChar) .|. value
          newBits = bits acc + bitsPerChar
       in if newBits >= 8
            then
              Out
                { buffer = newBuffer,
                  bits = newBits - 8,
                  written = written acc + 1,
                  out = out acc ++ [0xff .&. (newBuffer `shiftR` (newBits - 8))]
                }
            else
              Out
                { buffer = newBuffer,
                  bits = newBits,
                  written = written acc,
                  out = out acc
                }

encodeBase36 :: String -> [Int] -> String
encodeBase36 alphabet source =
  let (pbegin, zeroes) = getZeroesAndPBegin 0 0
      newB58 = reverse $ map snd $ Map.toList $ processBytes (size pbegin) pbegin (b58 [0 .. (size pbegin - 1)] Map.empty)
      it2 = getIt2 (size pbegin) (size pbegin - (size pbegin - 1)) newB58
      str = replicate zeroes leader
   in getStr str it2 (size pbegin) newB58
  where
    base :: Int
    base = length alphabet

    leader :: Char
    leader = head alphabet

    iFactor :: Float
    iFactor = logBase (int2Float base) 256

    pend :: Int
    pend = length source

    getZeroesAndPBegin :: Int -> Int -> (Int, Int)
    getZeroesAndPBegin pbegin zeroes =
      if pbegin /= pend && (source !! pbegin) == 0
        then getZeroesAndPBegin (pbegin + 1) (zeroes + 1)
        else (pbegin, zeroes)

    size :: Int -> Int
    size pbegin = float2Int (int2Float (pend - pbegin) * iFactor + 1) `shiftR` 0

    b58 :: [Int] -> Map.Map Int Int -> Map.Map Int Int
    b58 [x] b58Context = Map.insert x 0 b58Context
    b58 (x : xs) b58Context = b58 xs (Map.insert x 0 b58Context)
    b58 [] _ = Map.empty

    processBytes :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
    processBytes size' pbegin b58Context =
      if pbegin /= pend
        then
          let carry = source !! pbegin
           in processBytes size' (pbegin + 1) (snd $ applyBytes (size' - 1) carry b58Context)
        else b58Context

    applyBytes :: Int -> Int -> Map.Map Int Int -> (Int, Map.Map Int Int)
    applyBytes size' carry b58Context = Seq.foldlWithIndex f (carry, b58Context) (Seq.fromList [0 .. (size' - 1)])
      where
        f :: (Int, Map.Map Int Int) -> Int -> Int -> (Int, Map.Map Int Int)
        f (carry', b58Context') idx _ =
          let bit = Map.toList b58Context' !! idx
              newCarry = carry' + ((256 * snd bit) `shiftR` 0)
           in ((newCarry `div` base) `shiftR` 0, Map.update (\_ -> Just (mod newCarry base `shiftR` 0)) (fst bit) b58Context')

    getIt2 :: Int -> Int -> [Int] -> Int
    getIt2 size' it2 newB58 =
      if it2 /= size' && (newB58 !! it2) == 0
        then getIt2 size' (it2 + 1) newB58
        else it2

    getStr :: String -> Int -> Int -> [Int] -> String
    getStr str it2 size' newB58 =
      if it2 < size'
        then str ++ getStr [alphabet !! (newB58 !! it2)] (it2 + 1) size' newB58
        else str

encodeBase32InBase36 :: String -> Maybe String
encodeBase32InBase36 (_ : xs) = Just $ (['k'] ++) $ encodeBase36 (alphabet36 base36) $ decodeBase32 xs (alphabet32 base32) (bitsPerChar32 base32)
encodeBase32InBase36 _ = Nothing
