module PythonSite where

import System.Directory (doesFileExist, getHomeDirectory)
import System.Directory.Glob (glob, globDefaults)
import System.FilePath ((</>), getSearchPath)
import System.Info (os)

import Util


type OS = String

pySiteDirs :: String -> IO [FilePath]
pySiteDirs pkg = do
    let concatMapM f = fmap concat . mapM f
    home <- getHomeDirectory
    let dirs = (</> pkg) <$> possibleDirs os home
    concatMapM (glob globDefaults) dirs

-- Finds an executable with the given filename
which :: FilePath -> IO (Maybe FilePath)
which exe = go =<< getSearchPath where
    go (p:ps) = ifM (doesFileExist (p </> exe))
                    (return2 p)
                    (go ps)
    go [] = return Nothing


{- Returns a list of possible Python site-packages directories, using wildcards for versions.
   This could be in any number of places, but invoking Python to do it the right way would be too expensive (60 ms).
   I'm only supporting Unixes for now, but Windows support can be added if required.

   + Debian, etc. have dist-packages in addition to site-packages

   + user vs system site packages:
     * user packages:
        - Linux + OSX: ~/.local/lib/pythonX.Y/site-packages
        - OSX:         ~/Library/Python/X.Y/lib/python/site-packages
        - Windows:     %APPDATA%\Python\PythonXY\site-packages

     * site-packages:
        - Linux:       /usr/lib/python3.4/site-packages
        - OSX:         /Library/Python/2.7/site-packages
                       /Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages
        - Both:        /usr/lib or /usr/local/lib
 -}
possibleDirs :: OS -> FilePath -> [FilePath]
possibleDirs "darwin" home = [
        home </> ".local/lib/python?.?/site-packages",
        home </> "Library/Python/?.?/lib/python/site-packages",
        "/Library/Python/?.?/site-packages",
        "/Library/Frameworks/Python.framework/Versions/?.?/lib/python?.?/site-packages",
        "/usr/local/lib/python?.?/site-packages",
        "/usr/lib/python?.?/site-packages"
    ]
-- Assume all other OSes are some kind of Unix
possibleDirs _ home = let homeSite = home </> ".local/lib/python?.?/site-packages"
                      in  homeSite : do
    libPrefix <- ["/usr/lib/python?.?", "/usr/local/lib/python?.?"]
    sitePkgs  <- ["site-packages", "dist-packages"]
    return $ libPrefix </> sitePkgs


