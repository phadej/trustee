module Trustee.Options (
    Cmd (..),
    GlobalOpts (..),
    IncludeDeprecated (..),
    goIndexState,
    PlanParams (..),
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
import System.Path                   (FsPath, fromFilePath)

import qualified Data.Map            as Map
import qualified Options.Applicative as O

data IncludeDeprecated = IncludeDeprecated | OmitDeprecated
  deriving Show

data GlobalOpts = GlobalOpts
    { goGhcVersions       :: VersionRange
    , goPlanParams        :: PlanParams
    , goIncludeDeprecated :: IncludeDeprecated
    }
  deriving Show

goIndexState :: GlobalOpts -> Maybe UTCTime
goIndexState = ppIndexState . goPlanParams

data PlanParams = PlanParams
    { ppConstraints :: Map PackageName VersionRange -- TODO: support installed
    , ppAllowNewer  :: [String]                     -- TODO: proper type
    , ppIndexState  :: Maybe UTCTime
    , ppBackjumps   :: Maybe Int
    }
  deriving Show

data Cmd
    = CmdNewBuild [String]
    | CmdMatrix Bool [FsPath]
    | CmdMatrix2 Bool [FsPath]
    | CmdGet PackageName VersionRange
    | CmdBounds Bool (Maybe Limit)

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
globalOpts = mkGlobalOpts
    <$> ghcs
    <*> fmap mkConstraintMap (many constraints)
    <*> many allowNewer
    <*> optional indexState
    <*> optional backjumps
    <*> includeDeprecated
  where
    mkGlobalOpts x0 x1 x2 x3 x4 = GlobalOpts x0 (PlanParams x1 x2 x3 x4)

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

    allowNewer = O.strOption $ mconcat
        [ O.long "allow-newer"
        , O.metavar "pkg:dep"
        , O.help "Allow newer versions of packags"
        ]

    indexState = option (O.maybeReader $ \s -> iso s <|> stamp s)
        [ O.long "index-state"
        , O.metavar ":timestamp"
        , O.help "Index state"
        ]
      where
        iso   = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
        stamp = parseTimeM True defaultTimeLocale "%s"

    backjumps = option O.auto
        [ O.long "max-backjumps"
        , O.metavar ":count"
        , O.help "Maximum number of backjumps allowed"
        ]

    includeDeprecated = a <|> b <|> pure IncludeDeprecated where
        a = O.flag' IncludeDeprecated $ mconcat
            [ O.long "include-deprecated"
            , O.help "Include deprecated packages (default)"
            ]

        b = O.flag' OmitDeprecated $ mconcat
            [ O.long "omit-deprecated"
            , O.help "Omit deprecated packages"
            ]


cmd :: O.Parser Cmd
cmd = O.subparser $ mconcat
    [ O.command "new-build" $ O.info cmdNewBuild $ O.progDesc "Execute cabal new-build."
    , O.command "matrix" $ O.info cmdMatrix $ O.progDesc "Build matrix"
    , O.command "matrix2" $ O.info cmdMatrix2 $ O.progDesc "Build matrix"
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
    pkgs = O.argument (O.maybeReader $ Just . fromFilePath) $ mconcat
        [ O.metavar "pkg-dir"
        , O.help "package directories to include in matrix"
        ]

cmdMatrix2 :: O.Parser Cmd
cmdMatrix2 = CmdMatrix2
    <$> switch
        [ O.help "Run tests also"
        , O.long "test"
        ]
    <*> many pkgs
    <**> O.helper
  where
    pkgs = O.argument (O.maybeReader $ Just . fromFilePath) $ mconcat
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
    toPair (Dependency pname vr _) = (pname, vr)
