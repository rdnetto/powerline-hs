
module CommandArgs where

import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as Map
import Options.Applicative
import Options.Applicative.Types
import Safe (lastMay)
import Text.ParserCombinators.ReadP (char, munch1, sepBy1, readP_to_S)


data CommandArgs = CommandArgs {
                    rendererModule :: String,
                    rendererArgs   :: RendererArgs,
                    promptWidth    :: Int,
                    lastExitCode   :: Int,
                    lastPipeStatus :: [Int],
                    jobNum         :: Int,
                    extension      :: String,
                    renderSide     :: RenderSide
                }

data RenderSide = RSLeft | RSRight | RSAbove | RSAboveLeft

type RendererArgs = Map.Map String String


argParser :: Parser CommandArgs
argParser = CommandArgs
            <$> strOption (  long "renderer-module"
                          <> short 'r'
                          <> metavar "MODULE"
                          <> help "Renderer module. e.g. .zsh"
                          )
            <*> rendererArgsOption (  long "renderer-arg"
                          <> metavar "ARG=VALUE"
                          <> help "Additional information for the renderer."
                          )
            <*> intOption (  long "width"
                          <> short 'w'
                          <> metavar "WIDTH"
                          <> help "Maximum prompt width - controls truncation"
                          )
            <*> intOption (  long "last-exit-code"
                          <> metavar "INT"
                          <> help "Last exit code."
                          )
            <*> option splintReader
                          (  long "last-pipe-status"
                          <> metavar "LIST"
                          <> help "Space-seperated array of exit codes, corresponding to the commands in one pipe."
                          )
            <*> intOption (  long "jobnum"
                          <> metavar "INT"
                          <> help "Number of jobs."
                          )
            <*> argument str (metavar "EXT")
            <*> argument sideReader (metavar "SIDE")


parseArgs :: IO CommandArgs
parseArgs = execParser opts where
    opts = info (helper <*> argParser)
                (fullDesc
                <> progDesc "Powerline clone - generates shell prompts and statuslines."
                )

intOption :: Mod OptionFields Int -> Parser Int
intOption = option auto

-- Parser for a key-value pair option that may be specified multiple times
rendererArgsOption :: Mod OptionFields (String, String) -> Parser RendererArgs
rendererArgsOption desc = Map.fromList . concat <$> some single where
    single = return <$> option keyValuePairReader desc

-- Parses a 'key=value' pair
keyValuePairReader :: ReadM (String, String)
keyValuePairReader = do
    arg <- readerAsk
    let parser = sepBy1 (munch1 $ (/=) '=') (char '=')

    case lastMay $ readP_to_S parser arg of
        Just ([k, v], "") -> return (k, v)
        _                 -> fail $ '\'' : arg ++ "' does not have format 'key=value'."

-- Space-seperated list of ints
-- Based on http://therning.org/magnus/posts/2014-10-13-000-optparse-applicative.html
-- Note that ReadP is a much simpler, easier to use parser combinator library than parsec, and does not require additional dependencies.
splintReader :: ReadM [Int]
splintReader = do
    arg <- readerAsk
    let parseSplint = sepBy1 (munch1 isDigit) (munch1 isSpace)

    case lastMay $ readP_to_S parseSplint arg of
        Just (x, "") -> return $ read <$> x
        _            -> fail $ '\'' : arg ++ "' is not a space-separated list of ints."

sideReader :: ReadM RenderSide
sideReader = do
    arg <- readerAsk
    let sides = [
                ("left", RSLeft),
                ("right", RSRight),
                ("above", RSAbove),
                ("aboveleft", RSAboveLeft)
            ]
    let validSides = fst <$> sides

    case lookup arg sides of
        Just x  -> return x
        Nothing -> fail $ "'" ++ arg ++ "' is not one of: " ++ show validSides

