{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.List (intercalate)
import GHC.Generics
import Network.HTTP.Req
import System.Environment (getArgs)
import Data.Char (toLower, isAlphaNum)
-- I LOVE HASKELL
-- :HEART: :HEART: :HEART:

-- you've heard of everything is an expression
-- get ready for everything is a function
mangleChar :: Char -> Char
mangleChar c
    | not (isAlphaNum c) = '-'
    | otherwise = toLower c;
mangleName :: String -> String
mangleName = map mangleChar

data ModInfo = ModInfo { 
    name :: String, 
    author :: String, 
    version :: String, 
    dependencies :: Maybe [String], 
    download_url :: String, 
    gitPath :: Maybe String, 
    group :: String, 
    installLocation :: Maybe String, 
    beta :: Maybe Bool
} deriving Generic

instance FromJSON ModInfo
instance ToJSON ModInfo

instance Show ModInfo where 
    showsPrec d m = showString (name m) . showString " " . showString (version m)
    show m = shows m ""
    showList ml = showString (intercalate "\n" (map show ml))
main :: IO()
main = runReq defaultHttpConfig $ do 
    
    response <- req GET (https "raw.githubusercontent.com" /: "DeadlyKitten/MonkeModInfo/master/modinfo.json") NoReqBody jsonResponse mempty
    -- how'd i get this to work
    liftIO $ do
        args <- getArgs
        if null args then 
            putStrLn "usage: cligmm [arg]"
        else do
            let cmd = head args in 
                case cmd of 
                    "list" -> 
                        print (responseBody response :: [ModInfo])
                    "install" -> 
                        putStrLn "todo"
                    cmd -> 
                        putStrLn "unknown command"
    
  
    
    

