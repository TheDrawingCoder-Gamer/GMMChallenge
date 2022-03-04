{-# LANGUAGE DeriveGeneric, OverloadedStrings, ImportQualifiedPost, DuplicateRecordFields    #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Monad ()
import Data.Aeson ( ToJSON, FromJSON )
import Data.Text (pack)
import Data.List (intercalate, find)
import GHC.Generics ( Generic )
import Network.HTTP.Req
    ( (/:),
      bsResponse,
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      useHttpsURI,
      GET(GET),
      NoReqBody(NoReqBody) )
import System.Environment (getArgs)
import Data.Char (toLower, isAlphaNum)
import Codec.Archive.Zip ( unpackInto, withArchive )
import Text.URI ( mkURI )
import Data.ByteString qualified as B
import System.IO (openBinaryTempFile, hClose)
import System.Directory (removeFile, doesPathExist)
import Data.Functor ((<&>))

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
    showsPrec _ m = showString (mangleName $ name m) . showString " " . showString (version m)
    show m = shows m ""
    showList ml = showString (intercalate "\n" (map show ml))
-- CMD INFO & USAGE STUFF
data CmdInfo = CmdInfo {
    cmdname :: String, 
    desc :: String,
    usage :: String
}

cmdinfos :: [CmdInfo]
cmdinfos = [
    CmdInfo {
        cmdname = "list",
        desc = "lists all available mods",
        usage = "cligmm list"
    },
    CmdInfo {
        cmdname = "setup",
        desc = "saves the path to gtag",
        usage = "cligmm setup [path]"
    },
    CmdInfo {
        cmdname = "install",
        desc = "install selected mod",
        usage = "cligmm install [mod]"
    }
    ]
cmdfromname :: String -> Maybe CmdInfo
cmdfromname dacmdname = find ((== dacmdname) . cmdname) cmdinfos
cmdUsage :: String -> Maybe String
cmdUsage dacmdname = cmdfromname dacmdname <&> usage
-- MISC STUFF
mangleChar :: Char -> Char
mangleChar c
    | not (isAlphaNum c) = '-'
    | otherwise = toLower c;
mangleName :: String -> String
mangleName = map mangleChar


atLeastLong :: Int -> [a] -> Bool
atLeastLong 0 _ = True
atLeastLong _ [] = False 
atLeastLong n (_:ys) = atLeastLong (n-1) ys

atMostLong :: Int -> [a] -> Bool 
-- is there a better way to do this?
atMostLong x = not . atLeastLong x

-- TODO: There is probably a nicer, more monadic way to do this
installPath :: String -> ModInfo -> String 
installPath path info = case install_location info of
                            Just v -> path ++ v
                            Nothing -> path

installModStr :: [ModInfo] -> String -> Bool -> String -> IO Bool 
installModStr infos path doDeps mname =
    case find (\x -> name x == mname) infos of 
        Just info -> 
            installMod infos path doDeps info 
        Nothing -> 
            pure False
-- put common stuff at front for partial application magic
installMod :: [ModInfo] -> String -> Bool -> ModInfo -> IO Bool 
installMod infos path doDeps info  = do 
    uri <- mkURI $ pack $ download_url info
    let url' = useHttpsURI uri
        magicInstall = installModStr infos path doDeps
    if doDeps then 
        case dependencies info of 
            Just v -> 
                do
                    mapM_ magicInstall v
            Nothing -> 
                pure ()
    else 
        pure ()
    case url' of 
        Just (url, _) -> do
            putStrLn ("Installing " ++ name info)
            response <- runReq defaultHttpConfig $ req GET url NoReqBody bsResponse mempty
            (zpath, handle) <- openBinaryTempFile path "temp.zip"
            B.hPut handle (responseBody response :: B.ByteString)
            hClose handle 
            withArchive zpath $ do 
                unpackInto $ installPath path info
            removeFile zpath
            pure True
        Nothing ->  
            pure False


maybePrint :: Maybe String -> IO ()
maybePrint = maybe (pure ()) putStrLn
-- MAIN STUFF : )
main :: IO()
main = do 
    
    response <- runReq defaultHttpConfig $ req GET (https "raw.githubusercontent.com" /: "DeadlyKitten/MonkeModInfo/master/modinfo.json") NoReqBody jsonResponse mempty

    args <- getArgs
    if null args then 
        putStrLn "usage: cligmm [arg]"
    else do
        let cmd = head args in 
            case cmd of 
                "list" -> 
                    print (responseBody response :: [ModInfo])
                "install" -> 
                    if atMostLong 2 args then 
                        maybePrint $ cmdUsage "install"
                    else do
                        
                        existsThingie <- doesPathExist "gtagpath.txt"
                        if existsThingie then do
                            gtagPath <- readFile "gtagpath.txt"
                            let modName = args !! 1
                                modlist = responseBody response :: [ModInfo]
                                daMod = find (\x -> mangleName (name x) == modName) modlist
                            case daMod of 
                                Just info -> do 
                                    let doDeps = "--ignore-deps" `notElem` args
                                    success <- installMod modlist gtagPath doDeps info
                                    if success then 
                                        if doDeps then 
                                            putStrLn "mod has been installed, along with dependencies"
                                        else 
                                            putStrLn "mod has been installed, with no dependencies"
                                    else 
                                        putStrLn "failed to install mod"
                                Nothing -> 
                                    putStrLn "no such mod"
                        else do
                            putStrLn "unknown path... run cligmm setup [path]"

                "setup" -> do 
                    if  atMostLong 2 args then 
                        maybePrint $ cmdUsage "setup"
                    else 
                        writeFile "gtagpath.txt" (args !! 1)
                _ -> 
                    putStrLn "unknown command"
        