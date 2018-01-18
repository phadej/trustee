module Trustee.Options (
    Cmd (..),
    GlobalOpts (..),
    Limit (..),
    parseOpts,
    ) where

import Control.Applicative           (many, optional, (<**>), (<|>))
import Data.Map                      (Map)
import Data.Time                     (UTCTime, defaultTimeLocale, parseTimeM)
import Distribution.Package          (PackageName)
import Distribution.Text             (simpleParse)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Version          (VersionRange, anyVersion)

import qualified Data.Map            as Map
import qualified Options.Applicative as O

data GlobalOpts = GlobalOpts
    { goGhcVersions :: VersionRange
    , goConstraints :: Map PackageName VersionRange
    , goIndexState  :: Maybe UTCTime
    }
  deriving Show

data Cmd
    = CmdNewBuild [String]
    | CmdMatrix Bool [FilePath]
    | CmdGet PackageName VersionRange
    | CmdBounds Bool (Maybe Limit)
  deriving Show

data Limit = LimitUpper | LimitLower
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
    <*> fmap mkConstraintMap (many constraints)
    <*> optional indexState
  where
    option x ms = O.option x (mconcat ms)

    ghcs = option (O.maybeReader simpleParse)
        [ O.short 'g'
        , O.long "ghcs"
        , O.metavar ":version-range"
        , O.help "GHC versions to build with"
        ] <|> pure anyVersion

    constraints = option (O.maybeReader simpleParse)
        [ O.short 'c'
        , O.long "constraint"
        , O.metavar ":constraint"
        , O.help "Additional constraints"
        ]

    indexState = option (O.maybeReader $ \s -> iso s <|> stamp s)
        [ O.long "index-state"
        , O.metavar ":timestamp"
        , O.help "Index state"
        ]
      where
        iso   = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
        stamp = parseTimeM True defaultTimeLocale "%s"

cmd :: O.Parser Cmd
cmd = O.subparser $ mconcat
    [ O.command "new-build" $ O.info cmdNewBuild $ O.progDesc "Execute cabal new-build."
    , O.command "matrix" $ O.info cmdMatrix $ O.progDesc "Build matrix"
    , O.command "get" $ O.info cmdGet $ O.progDesc "Fetch package sources"
    , O.command "bounds" $ O.info cmdBounds $ O.progDesc "Find and check bounds"
    ]

cmdBounds :: O.Parser Cmd
cmdBounds = CmdBounds
    <$> switch
        [ O.help "verify plans"
        , O.long "verify"
        ]
    <*> limit
    <**> O.helper
  where
    limit = lower <|> upper <|> sweep

    lower = O.flag' (Just LimitLower) $ mconcat
        [ O.long "lower"
        , O.help "Lower bounds"
        ]

    upper = O.flag' (Just LimitUpper) $ mconcat
        [ O.long "upper"
        , O.help "Upper bounds"
        ]

    sweep = O.flag' Nothing $ mconcat
        [ O.long "sweep"
        , O.help "Sweep bounds (expensive)"
        ]

cmdNewBuild :: O.Parser Cmd
cmdNewBuild = CmdNewBuild
    <$> many (O.strArgument $ mconcat
        [ O.metavar "arg"
        , O.help "arguments to cabal new-build"
        ])
    <**> O.helper

cmdMatrix :: O.Parser Cmd
cmdMatrix = CmdMatrix
    <$> switch
        [ O.help "Run tests also"
        , O.long "test"
        ]
    <*> many pkgs
    <**> O.helper
  where
    pkgs = O.strArgument $ mconcat
        [ O.metavar "pkg-dir"
        , O.help "package directories to include in matrix"
        ]


cmdGet :: O.Parser Cmd
cmdGet = CmdGet
    <$> name
    <*> (version <|> pure anyVersion)
    <**> O.helper
  where
    name = O.argument (O.maybeReader simpleParse) $ mconcat
        [ O.metavar ":pkgname"
        , O.help "Package name"
        ]

    version = O.argument (O.maybeReader simpleParse) $ mconcat
        [ O.metavar ":versions"
        , O.help "Version range"
        ]

switch :: [O.Mod O.FlagFields Bool] -> O.Parser Bool
switch = O.switch . mconcat

mkConstraintMap :: [Dependency] -> Map PackageName VersionRange
mkConstraintMap = Map.fromList . map toPair where
    toPair (Dependency pname vr) = (pname, vr)
