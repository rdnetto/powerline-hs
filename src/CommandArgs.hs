
module CommandArgs where

import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as Map
import Options.Applicative
import Options.Applicative.Types
import Safe (lastMay)
import Text.ParserCombinators.ReadP (char, munch, munch1, sepBy1, readP_to_S)


data CommandArgs = CommandArgs {
                    rendererModule :: RendererModule,
                    rendererArgs   :: RendererArgs,
                    promptWidth    :: Maybe Int,        -- If not present, don't need to drop segments.
                    lastExitCode   :: Int,
                    lastPipeStatus :: [Int],
                    jobNum         :: Int,
                    debugSegment   :: Maybe String,
                    extension      :: String,
                    renderSide     :: RenderSide
                } deriving (Show)

data RendererModule = RMRaw | RMZsh | RMBash
    deriving (Eq, Show)

type RendererArgs = Map.Map String String

data RenderSide = RSLeft        -- Continuation (PS2, PS3)
                | RSRight       -- Right prompt (RPS1)
                | RSAboveLeft   -- Left prompt (PS1)
                | RSAbove       -- Unknown
                deriving (Eq, Show)


argParser :: Parser CommandArgs
argParser = CommandArgs
            <$> defOption RMRaw rmReader
                          (  long "renderer-module"
                          <> short 'r'
                          <> metavar "MODULE"
                          <> help "Renderer module. e.g. .zsh"
                          )
            <*> rendererArgsOption (  long "renderer-arg"
                          <> metavar "ARG=VALUE"
                          <> help "Additional information for the renderer."
                          )
            <*> optional (option auto
                          $  long "width"
                          <> short 'w'
                          <> metavar "WIDTH"
                          <> help "Maximum prompt width - controls truncation"
                          )
            <*> defOption 0 auto
                          (  long "last-exit-code"
                          <> metavar "INT"
                          <> help "Last exit code."
                          )
            <*> defOption [] splintReader
                          (  long "last-pipe-status"
                          <> metavar "LIST"
                          <> help "Space-seperated array of exit codes, corresponding to the commands in one pipe."
                          )
            <*> defOption 0 auto (  long "jobnum"
                          <> metavar "INT"
                          <> help "Number of jobs."
                          )
            <*> optional (strOption
                          $  long "debug"
                          <> short 'd'
                          <> metavar "SEGMENT"
                          <> help "Display only the specified segment. Used for debugging. Must be a fully qualified name. e.g powerline.segments.shell.cwd"
                          )
            <*> argument str (metavar "EXT")
            <*> argument sideReader (metavar "SIDE")


parseArgs :: IO CommandArgs
parseArgs = execParser opts where
    opts = info (helper <*> argParser)
                (fullDesc
                <> progDesc "Powerline clone - generates shell prompts and statuslines."
                )

-- Convenience function for options which have a default value
defOption :: a -> ReadM a -> Mod OptionFields a -> Parser a
defOption def p m = option p m <|> pure def

-- Parser for a key-value pair option that may be specified multiple times
rendererArgsOption :: Mod OptionFields (String, String) -> Parser RendererArgs
rendererArgsOption desc = Map.fromList . concat <$> many single where
    single = return <$> option keyValuePairReader desc

-- Parses a 'key=value' pair. Note that value (but not key) can be an empty String.
keyValuePairReader :: ReadM (String, String)
keyValuePairReader = do
    arg <- readerAsk
    let k = munch1 $ (/=) '='
    let v = munch  $ const True
    let parser = (,) <$> k
                     <*  char '='
                     <*> v

    case lastMay $ readP_to_S parser arg of
        Just (kv, "") -> return kv
        _             -> fail $ '\'' : arg ++ "' does not have format 'key=value'."

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

rmReader ::  ReadM RendererModule
rmReader = do
    arg <- readerAsk

    return $ case arg of
                  ".zsh"  -> RMZsh
                  ".bash" -> RMBash
                  unknown -> error $ "Unknown renderer module: " ++ unknown

