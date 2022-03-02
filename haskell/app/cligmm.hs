{-# LANGUAGE DeriveGeneric, OverloadedStrings, ImportQualifiedPost    #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.List (intercalate, find)
import GHC.Generics
import Network.HTTP.Req
import System.Environment (getArgs)
import Data.Char (toLower, isAlphaNum)
import Codec.Archive.Zip
import System.IO.Error
import Text.URI
import Data.ByteString qualified as B
import System.IO (openBinaryFile, openBinaryTempFile, hClose)
import System.Directory (removeFile, doesPathExist)
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



installModStr :: [ModInfo] -> String -> Bool -> String -> IO Bool 
installModStr infos path doDeps mname =
    case find (\x -> name x == mname) infos of 
        Just mod -> 
            installMod infos path doDeps mod 
        Nothing -> 
            return False
-- put common stuff at front for partial application magic
installMod :: [ModInfo] -> String -> Bool -> ModInfo -> IO Bool 
installMod infos path doDeps info  = do 
    uri <- mkURI $ pack $ download_url info
    let url' = useHttpsURI uri
        magicInstall = installModStr infos path doDeps
    if doDeps then 
        case (dependencies info) of 
            Just v -> 
                do
                    mapM_ magicInstall v
                    return True
            Nothing -> 
                return True
    else 
        return True
    case url' of 
        Just (url, _) -> do
            runReq defaultHttpConfig $ do 
                response <- req GET url NoReqBody bsResponse mempty
                
                liftIO $ do 
                    (zpath, handle) <- openBinaryTempFile path "temp.zip"
                    B.hPut handle (responseBody response :: B.ByteString)
                    hClose handle 
                    withArchive zpath $ do 
                        let rootDir = case install_location info of 
                                    Just v -> 
                                        path ++ v 
                                    Nothing -> 
                                        path 
                        unpackInto rootDir
                    removeFile zpath
            return True
        Nothing ->  
            return False


data ModInfo = ModInfo { 
    name :: String, 
    author :: String, 
    version :: String, 
    dependencies :: Maybe [String], 
    download_url :: String, 
    gitPath :: Maybe String, 
    group :: String, 
    install_location :: Maybe String, 
    beta :: Maybe Bool
} deriving Generic

instance FromJSON ModInfo
instance ToJSON ModInfo

instance Show ModInfo where 
    showsPrec d m = showString (mangleName $ name m) . showString " " . showString (version m)
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
                        if length args < 2 then 
                            putStrLn "usage: cligmm install [mod-name]"
                        else do
                            
                            existsThingie <- doesPathExist "gtagpath.txt"
                            if existsThingie then do
                                gtagPath <- readFile "gtagpath.txt"
                                let modName = args !! 1
                                    modlist = responseBody response :: [ModInfo]
                                    daMod = find (\x -> mangleName (name x) == modName) modlist
                                case daMod of 
                                    Just mod -> do 
                                        let doDeps = "--ignore-deps" `notElem` args
                                        installMod modlist gtagPath doDeps mod 
                                        if doDeps then 
                                            putStrLn "mod has been installed, along with dependencies"
                                        else 
                                            putStrLn "mod has been installed, with no dependencies"
                                    Nothing -> 
                                        putStrLn "no such mod"
                            else 
                                putStrLn "unknown path... run cligmm setup [path]"

                    "setup" -> do 
                        if length args < 2 then 
                            putStrLn "usage: cligmm setup [path]"
                        else 
                            writeFile "gtagpath.txt" (args !! 1)
                    cmd -> 
                        putStrLn "unknown command"