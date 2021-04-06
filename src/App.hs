module App where

import System.Directory

data FromLibrary = FromLibrary { libName :: Maybe String, impFileName :: String }


copyLibraryFile t s = do
        source <- makeAbsolute s
        target <- makeAbsolute t
        putStrLn $ "BasicEC Target: " ++ target
            ++ "\nBasicEC Source: " ++ source
        copyFile target source