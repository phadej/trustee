module Trustee.Options (
    Cmd (..),
    GlobalOpts (..),
    parseOpts,
    ) where

import Control.Applicative  ((<**>), (<|>), many)
import Distribution.Text    (simpleParse)
import Distribution.Version (VersionRange, anyVersion)

import qualified Options.Applicative as O

newtype GlobalOpts = GlobalOpts
    { goGhcVersions :: VersionRange
    }
  deriving Show

data Cmd
    = CmdNewBuild [String]
    | CmdMatrix [FilePath]
    | CmdLowerBounds
  deriving Show

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

parseOpts :: IO (GlobalOpts, Cmd)
parseOpts = O.execParser $ O.info ((,) <$> globalOpts <*> cmd <**> O.helper) $ mconcat
    [ O.fullDesc
    , O.header "trustee - automate Hackage Trustee tasks"
    ]

globalOpts :: O.Parser GlobalOpts
globalOpts = GlobalOpts
    <$> ghcs
  where
    option x ms = O.option x (mconcat ms)

    ghcs = option (O.maybeReader simpleParse)
        [ O.short 'g'
        , O.long "ghcs"
        , O.metavar ":version-range"
        , O.help "GHC versions to build with"
        ] <|> pure anyVersion

cmd :: O.Parser Cmd
cmd = O.subparser $ mconcat
    [ O.command "new-build" $ O.info cmdNewBuild $ O.progDesc "Execute cabal new-build."
    , O.command "matrix" $ O.info cmdMatrix $ O.progDesc "Build matrix"
    ]

cmdNewBuild :: O.Parser Cmd
cmdNewBuild = CmdNewBuild <$> many (O.strArgument $ mconcat
    [ O.metavar "arg"
    , O.help "arguments to cabal new-build"
    ])

cmdMatrix :: O.Parser Cmd
cmdMatrix = CmdMatrix <$> many (O.strArgument $ mconcat
    [ O.metavar "pkg-dir"
    , O.help "package directories to include in matrix"
    ])
