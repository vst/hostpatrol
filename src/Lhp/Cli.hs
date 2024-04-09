{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides top-level definitions for the CLI program.
module Lhp.Cli where

import qualified Autodocodec.Schema as ADC.Schema
import Control.Applicative ((<**>))
import Control.Monad (join)
import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Lhp.Config as Config
import qualified Lhp.Meta as Meta
import Lhp.Remote (compileReport)
import Lhp.Types (Report)
import Options.Applicative ((<|>))
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))
import System.IO (stderr)


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
    <|> commandVersion


-- * Commands


-- ** compile


-- | Definition for @compile@ CLI command.
commandCompile :: OA.Parser (IO ExitCode)
commandCompile = OA.hsubparser (OA.command "compile" (OA.info parser infomod) <> OA.metavar "compile")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Compile remote host information." <> OA.footer "This command fetches and compiles remote host information."
    parser =
      doCompile
        <$> OA.optional (OA.strOption (OA.short 'c' <> OA.long "config" <> OA.action "file" <> OA.help "Path to the configuration file."))
        <*> OA.many (OA.strOption (OA.short 'h' <> OA.long "host" <> OA.help "Remote host (in SSH destination format)."))
        <*> OA.switch (OA.short 'p' <> OA.long "parallel" <> OA.help "Hit remote hosts in parallel.")


-- | @compile@ CLI command program.
doCompile :: Maybe FilePath -> [T.Text] -> Bool -> IO ExitCode
doCompile cpath dests par = do
  baseConfig <- maybe (pure (Config.Config [] [])) Config.readConfigFile cpath
  let config =
        baseConfig
          { Config._configHosts = Config._configHosts baseConfig <> fmap _mkHost dests
          }
  res <- runExceptT (compileReport par config)
  case res of
    Left err -> BLC.hPutStrLn stderr (Aeson.encode err) >> pure (ExitFailure 1)
    Right sr -> BLC.putStrLn (Aeson.encode sr) >> pure ExitSuccess
  where
    _mkHost d =
      Config.HostSpec
        { Config._hostSpecName = d
        , Config._hostSpecSsh = Nothing
        , Config._hostSpecId = Nothing
        , Config._hostSpecUrl = Nothing
        , Config._hostSpecTags = []
        , Config._hostSpecData = Aeson.Null
        , Config._hostSpecKnownSshKeys = []
        }


-- ** schema


-- | Definition for @schema@ CLI command.
commandSchema :: OA.Parser (IO ExitCode)
commandSchema = OA.hsubparser (OA.command "schema" (OA.info parser infomod) <> OA.metavar "schema")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Produce JSON schema for report." <> OA.footer "This command produces JSON schema for report data type."
    parser = pure (BLC.putStrLn (Aeson.encode (ADC.Schema.jsonSchemaViaCodec @Report)) >> pure ExitSuccess)


-- ** version


-- | Definition for @version@ CLI command.
commandVersion :: OA.Parser (IO ExitCode)
commandVersion = OA.hsubparser (OA.command "version" (OA.info parser infomod) <> OA.metavar "version")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Show version and build information." <> OA.footer "This command shows version and build information."
    parser =
      doVersion
        <$> OA.switch (OA.short 'j' <> OA.long "json" <> OA.help "Format output in JSON.")
    doVersion json = do
      if json
        then BLC.putStrLn (Aeson.encode Meta.buildInfo)
        else TIO.putStrLn (Meta.prettyBuildInfo Meta.buildInfo)
      pure ExitSuccess


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
