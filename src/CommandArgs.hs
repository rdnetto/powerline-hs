
module CommandArgs where

import Data.Char (isDigit, isSpace)
import Options.Applicative
import Options.Applicative.Types
import Safe (lastMay)
import Text.ParserCombinators.ReadP (munch1, sepBy1, readP_to_S)


data CommandArgs = CommandArgs {
                    rendererModule :: String,
                    rendererArgs   :: [String],
                    promptWidth    :: Int,
                    lastExitCode   :: Int,
                    lastPipeStatus :: [Int],
                    jobNum         :: Int,
                    extension      :: String
                }

argParser :: Parser CommandArgs
argParser = CommandArgs
            <$> strOption (  long "renderer-module"
                          <> short 'r'
                          <> metavar "MODULE"
                          <> help "Renderer module. e.g. .zsh"
                          )
            <*> multiStrOption (  long "renderer-arg"
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
                          <> help "Number of jobs."
                          )
            <*> argument str (metavar "EXT")


parseArgs :: IO CommandArgs
parseArgs = execParser opts where
    opts = info (helper <*> argParser)
                (fullDesc
                <> progDesc "Powerline clone - generates shell prompts and statuslines."
                )

intOption :: Mod OptionFields Int -> Parser Int
intOption = option auto

-- Parser for a String option that may be specified multiple times
multiStrOption :: Mod OptionFields [String] -> Parser [String]
multiStrOption desc = concat <$> some single where
    single = option (return <$> str) desc


-- TODO WIP: the values are being parsed correctly in splintReader, but don't seem to be stored in the record correctly

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

