{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides top-level definitions for the CLI program.
module Lhp.Cli where

import qualified Autodocodec.Schema as ADC.Schema
import Control.Applicative ((<**>))
import Control.Monad (join)
import Control.Monad.Except (runExceptT)
import qualified Control.Monad.Parallel as MP
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Lhp.Meta as Meta
import Lhp.Remote (compileReport)
import Lhp.Types (Report)
import qualified Lhp.Types as Types
import Options.Applicative ((<|>))
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)


-- * Entrypoint


-- | CLI program entrypoint.
cli :: IO ExitCode
cli =
  join (OA.execParser (OA.info opts desc))
  where
    opts = optProgram <**> infoOptVersion <**> OA.helper
    desc =
      OA.fullDesc
        <> OA.progDesc "Top Level Commands"
        <> infoModHeader
        <> infoModFooter


-- * Program


-- | Option parser for top-level commands.
optProgram :: OA.Parser (IO ExitCode)
optProgram =
  commandCompile
    <|> commandSchema


-- * Commands


-- ** compile


-- | Definition for @compile@ CLI command.
commandCompile :: OA.Parser (IO ExitCode)
commandCompile = OA.hsubparser (OA.command "compile" (OA.info parser infomod) <> OA.metavar "compile")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Compile remote host information." <> OA.footer "This command fetches and compiles remote host information."
    parser =
      doCompile
        <$> OA.many (OA.strOption (OA.short 'h' <> OA.long "host" <> OA.help "Remote host (in SSH destination format)."))
        <*> OA.switch (OA.short 's' <> OA.long "stream" <> OA.help "Streaming results.")


-- | @compile@ CLI command program.
doCompile :: [T.Text] -> Bool -> IO ExitCode
doCompile dests stream = do
  let hosts = fmap (\d -> Types.Host {Types._hostName = d, Types._hostUrl = Nothing, Types._hostTags = []}) dests
  case stream of
    False -> do
      res <- runExceptT (MP.mapM compileReport hosts)
      case res of
        Left err -> BLC.hPutStrLn stderr (Aeson.encode err) >> pure (ExitFailure 1)
        Right sr -> BLC.putStrLn (Aeson.encode sr) >> pure ExitSuccess
    True -> do
      mapM_ go hosts
      pure ExitSuccess
      where
        go h@Types.Host {..} = do
          hPutStrLn stderr ("Patrolling " <> T.unpack _hostName)
          res <- runExceptT (compileReport h)
          case res of
            Left err -> BLC.hPutStrLn stderr (Aeson.encode err)
            Right sr -> BLC.putStrLn (Aeson.encode sr)


-- ** schema


-- | Definition for @schema@ CLI command.
commandSchema :: OA.Parser (IO ExitCode)
commandSchema = OA.hsubparser (OA.command "schema" (OA.info parser infomod) <> OA.metavar "schema")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Produce JSON schema for report." <> OA.footer "This command produces JSON schema for report data type."
    parser = pure (BLC.putStrLn (Aeson.encode (ADC.Schema.jsonSchemaViaCodec @Report)) >> pure ExitSuccess)


-- * Helpers


-- | Version option parser.
infoOptVersion :: OA.Parser (a -> a)
infoOptVersion =
  OA.infoOption Meta.versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- | Header 'OA.InfoMod'.
infoModHeader :: OA.InfoMod a
infoModHeader =
  OA.header (T.unpack (Meta.name <> " - " <> Meta.title <> " v" <> Meta.versionText))


-- | Footer 'OA.InfoMod'.
infoModFooter :: OA.InfoMod a
infoModFooter =
  OA.footer "See <https://github.com/vst/lhp> for help and feedback."


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser = OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as = case runParserTest p as of
  OA.Success _ -> pure (Right ())
  OA.Failure f -> pure (Left (show f))
  OA.CompletionInvoked _ -> pure (Right ())
