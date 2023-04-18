{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Turing1936 as T36

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Text as Text

import Data.Aeson.Types
import Control.Monad
import System.Environment

instance Aeson.FromJSON T36.SymbolPredicate where
  parseJSON (Aeson.Bool True) = return T36.Any
  parseJSON (Aeson.Bool False) = return T36.None
  parseJSON (Aeson.Null) = return T36.All
  parseJSON (Aeson.String s) = if Text.length s == 1 then
                                   return (T36.Is (Text.head s))
                                 else
                                   return (T36.AnyOf $ Text.unpack s)


instance Aeson.ToJSON T36.SymbolPredicate where
  toJSON (T36.Any)  = Aeson.Bool True
  toJSON (T36.None) = Aeson.String (Text.pack " ")
  toJSON (T36.All)  = Aeson.Null
  toJSON (T36.Is s) = (Aeson.String (Text.pack [s]))
  toJSON (T36.AnyOf s) = Aeson.String (Text.pack s)


instance Aeson.FromJSON T36.Operation where
  parseJSON (Aeson.String "L") = return T36.L
  parseJSON (Aeson.String "R") = return T36.R
  parseJSON (Aeson.String "P1") = return T36.P1
  parseJSON (Aeson.String "P0") = return T36.P0
  parseJSON (Aeson.String "Pә") = return T36.Pә
  parseJSON (Aeson.String "Px") = return T36.Px
  parseJSON (Aeson.String "E") = return T36.E
  parseJSON (Aeson.String s) = do
    guard (Text.length s == 2 && Text.head s == 'P')
    return (T36.P $ Text.last s)
  parseJSON _ = fail "Invalid operation"


instance Aeson.ToJSON T36.Operation where
  toJSON (T36.L) = Aeson.String "L"
  toJSON (T36.R) = Aeson.String "R"
  toJSON (T36.P1) = Aeson.String "P1"
  toJSON (T36.P0) = Aeson.String "P0"
  toJSON (T36.Pә) = Aeson.String "Pә"
  toJSON (T36.Px) = Aeson.String "Px"
  toJSON (T36.E) = Aeson.String "E"
  toJSON (T36.P x) = Aeson.String (Text.pack("P" ++ [x]))


instance Aeson.FromJSON T36.TuringMachine where
  parseJSON (Aeson.Object v) = do
    position <- v .: "position"
    tape <- v .: "tape"
    m_config <- v .: "m-config"
    table <- v .: "table" :: Data.Aeson.Types.Parser T36.TuringMachineTable
    comments <- v .: "comments"
    return (T36.TM position tape m_config table comments)

instance Aeson.ToJSON T36.TuringMachine where
  toJSON (T36.TM pos tape mc table comment) =
    object ["position" .= pos, "tape" .= tape, "m-config" .= mc,
            "table" .= table, "comments" .= comment]


n1 = 10
n2 = 80

{-| Execute a few steps of Turing's first two example machines. -}
default_action = do
  putStrLn ("\n\nExecuting " ++ show(n1) ++ " moves of Turing's first example")
  T36.printSteps n1 T36.tm1

  putStrLn ("\n\nExecuting " ++ show(n2) ++ " moves of Turing's second example")
  T36.printSteps n2 T36.tm2


decode bytes = do
  tm <- Aeson.decode bytes :: Maybe T36.TuringMachine
  return $ show tm


decode_file f = do
  j <- Bytes.readFile f
  case Aeson.eitherDecode j of
    Left err -> putStrLn $ "Failed decode: " ++ err
    Right tm -> putStrLn tm


encode_write = do
  putStrLn "=================  Encode & Write ================="
  putStrLn $ "Showing T36.tm1:\n" ++ show T36.tm1
  putStrLn $ "Encoding and writing T36.tm1 to file"
  let enc = Aeson.encode T36.tm1
  Bytes.writeFile "./test_output.json" enc
  putStrLn $ "File written. Contents: \n: " ++ show enc ++ "\n"
  case (Aeson.eitherDecode enc :: Either String T36.TuringMachine)  of
    Left err -> putStrLn $ "Decode error: " ++ err
    Right tm -> putStrLn $ "Success:" ++ (show tm)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      putStrLn "================= Parse & Run Turing Machine ================="
      j <- Bytes.readFile file
      do
        putStrLn $ "File conents: \n" ++ (UTF8.toString j)
        case (Aeson.decode j :: Maybe T36.TuringMachine) of
          Just tm -> do
            putStr $ "Successfully parsed Turing Machine. Running "
              ++ show n1 ++ " iterations\n\n"
            T36.printSteps n1 tm

          Nothing -> putStr $ "Failed to parse provided file"
    _ -> do default_action
