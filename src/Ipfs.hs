module Ipfs (
    encodeBase32InBase36,
    ipfsAdd,
    CID (..),
) where

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (foldl')
import Data.List.Extra ((!?))
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Float (float2Int, int2Float)
import Network.IPFS.API (ApiV0Add)
import Servant (Proxy (..))
import Servant.Client (ClientEnv, client, runClientM)

ipfsAddApi :: Proxy ApiV0Add
ipfsAddApi = Proxy

newtype CID = CID {unCID :: Text}

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

base32Alphabet :: String
base32Alphabet = "abcdefghijklmnopqrstuvwxyz234567"

base32BitsPerChar :: Int
base32BitsPerChar = 5

base36Alphabet :: String
base36Alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"

decodeBase32 :: String -> String -> Int -> Either String [Int]
decodeBase32 string alphabet bitsPerChar =
    case foldl' writeOut (Right $ Out{buffer = 0, bits = 0, written = 0, out = []}) end of
        Right x -> pure $ out x
        Left err -> Left err
  where
    codes :: Map.Map Char Int
    codes = Map.fromList (zip alphabet [0 ..])

    end :: String
    end = filter (/= '=') string

    writeOut :: Either String Out -> Char -> Either String Out
    writeOut acc c = do
        acc' <- acc
        case Map.lookup c codes of
            Just value ->
                let newBuffer = (buffer acc' `shiftL` bitsPerChar) .|. value
                    newBits = bits acc' + bitsPerChar
                 in if newBits >= 8
                        then
                            pure $
                                Out
                                    { buffer = newBuffer
                                    , bits = newBits - 8
                                    , written = written acc' + 1
                                    , out = out acc' ++ [0xff .&. (newBuffer `shiftR` (newBits - 8))]
                                    }
                        else
                            pure $
                                Out
                                    { buffer = newBuffer
                                    , bits = newBits
                                    , written = written acc'
                                    , out = out acc'
                                    }
            Nothing -> Left "Not found char in base32 alphabet"

encodeBase36 :: String -> [Int] -> Either String String
encodeBase36 alphabet source = do
    (pbegin, zeroes) <- getZeroesAndPBegin 0 0

    b58Context <- processBytes (size pbegin) pbegin (Map.fromList [(i, 0) | i <- [0 .. (size pbegin - 1)]])

    let b58List = reverse $ map snd $ Map.toList b58Context

    it2 <- getIt2 (size pbegin) (size pbegin - (size pbegin - 1)) b58List

    getStr (replicate zeroes leader) it2 (size pbegin) b58List
  where
    base :: Int
    base = length alphabet

    leader :: Char
    leader = head alphabet

    iFactor :: Float
    iFactor = logBase (int2Float base) 256

    pend :: Int
    pend = length source

    getZeroesAndPBegin :: Int -> Int -> Either String (Int, Int)
    getZeroesAndPBegin pbegin zeroes = case source !? pbegin of
        Just i ->
            if pbegin /= pend && i == 0
                then getZeroesAndPBegin (pbegin + 1) (zeroes + 1)
                else pure (pbegin, zeroes)
        Nothing -> Left $ "Cannot access index " ++ show pbegin ++ " in " ++ show source

    size :: Int -> Int
    size pbegin = float2Int (int2Float (pend - pbegin) * iFactor + 1)

    processBytes :: Int -> Int -> Map.Map Int Int -> Either String (Map.Map Int Int)
    processBytes size' pbegin b58Context =
        if pbegin /= pend
            then case source !? pbegin of
                Just carry -> do
                    b58WithAppliedBytes <- applyBytes (size' - 1) carry b58Context
                    processBytes size' (pbegin + 1) (snd b58WithAppliedBytes)
                Nothing -> Left $ "Cannot access index " ++ show pbegin ++ " in " ++ show source
            else pure b58Context

    applyBytes :: Int -> Int -> Map.Map Int Int -> Either String (Int, Map.Map Int Int)
    applyBytes size' carry b58Context = Seq.foldlWithIndex f (Right (carry, b58Context)) (Seq.fromList [0 .. (size' - 1)])
      where
        f :: Either String (Int, Map.Map Int Int) -> Int -> Int -> Either String (Int, Map.Map Int Int)
        f acc idx _ = do
            (carry', b58Context') <- acc
            case Map.toList b58Context' !? idx of
                Just bit ->
                    let newCarry = carry' + (256 * snd bit)
                     in pure (newCarry `div` base, Map.update (\_ -> Just (mod newCarry base)) (fst bit) b58Context')
                Nothing -> Left $ "Cannot access index " ++ show idx ++ " in " ++ show (Map.toList b58Context')

    getIt2 :: Int -> Int -> [Int] -> Either String Int
    getIt2 size' it2 b58List = case b58List !? it2 of
        Just i ->
            if it2 /= size' && i == 0
                then getIt2 size' (it2 + 1) b58List
                else pure it2
        Nothing -> Left $ "Cannot access index " ++ show it2 ++ " in " ++ show b58List

    getStr :: String -> Int -> Int -> [Int] -> Either String String
    getStr str it2 size' b58List =
        if it2 < size'
            then case (b58List !? it2) >>= (alphabet !?) of
                Just i -> do
                    c <- getStr [i] (it2 + 1) size' b58List
                    pure $ str ++ c
                Nothing -> Left $ "Cannot access index " ++ show it2 ++ " in " ++ show b58List
            else pure str

encodeBase32InBase36 :: String -> Either String String
encodeBase32InBase36 (_ : xs) = do
    decodedSource <- decodeBase32 xs base32Alphabet base32BitsPerChar
    out <- encodeBase36 base36Alphabet decodedSource
    pure $ 'k' : out
encodeBase32InBase36 _ = Left "String is too short"
