module Trustee.Options (
    Cmd (..),
    GlobalOpts (..),
    IncludeDeprecated (..),
    goIndexState,
    PlanParams (..),
    PackageConstraint (..),
    Limit (..),
    Verify (..),
    parseOpts,
    ) where

import Control.Applicative     ((<**>))
import Data.Time               (defaultTimeLocale, parseTimeM)
import Distribution.Parsec     (Parsec (..), ParsecParser, eitherParsec, explicitEitherParsec)
import Distribution.Types.Flag (FlagAssignment, parsecFlagAssignment)
import Distribution.Version    (anyVersion, intersectVersionRanges)
import System.Path             (fromAbsoluteFilePath)

import qualified Data.Map                        as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Version            as C
import qualified Options.Applicative             as O

import Peura

data IncludeDeprecated = IncludeDeprecated | OmitDeprecated
  deriving Show

data GlobalOpts = GlobalOpts
    { goGhcVersions       :: VersionRange
    , goPlanParams        :: PlanParams
    , goIncludeDeprecated :: IncludeDeprecated
    , goTracer            :: TracerOptions Void -> TracerOptions Void
    }

goIndexState :: GlobalOpts -> Maybe UTCTime
goIndexState = ppIndexState . goPlanParams

-- TODO: support installed
data PackageConstraint
    = PackageConstraint VersionRange FlagAssignment
  deriving (Show, Generic)

instance Binary PackageConstraint

-- this will work better with Cabal-3.4
instance Parsec PackageConstraint where
    parsec = mkVr <$> parsec <|> mkFa <$> parsecFlagAssignment where
        mkVr vr = PackageConstraint vr mempty
        mkFa fa = PackageConstraint anyVersion fa

instance Semigroup PackageConstraint where
    PackageConstraint vrA faA <> PackageConstraint vrB faB =
        PackageConstraint (intersectVersionRanges vrA vrB) (faA <> faB)

namedConstraint :: ParsecParser (PackageName, PackageConstraint)
namedConstraint = do
    pn <- parsec
    P.spaces
    c <- parsec
    return (pn, c)

data PlanParams = PlanParams
    { ppConstraints :: Map PackageName PackageConstraint
    , ppAllowNewer  :: [String]                           -- TODO: proper type
    , ppIndexState  :: Maybe UTCTime
    , ppBackjumps   :: Maybe Int
    , ppLocalRepos  :: [Path Absolute]
    }
  deriving Show

data Cmd
    = CmdMatrix Verify [FsPath]
    -- | CmdNewBuild [String]
    | CmdGet PackageName (Maybe VersionRange)
    | CmdBounds Verify Limit
    | CmdSweep Verify

data Verify = Verify | SolveOnly
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
globalOpts = mkGlobalOpts
    <$> ghcs
    <*> fmap Map.fromList (many constraints)
    <*> many allowNewer
    <*> optional indexState
    <*> optional backjumps
    <*> many localrepo
    <*> includeDeprecated
    <*> tracerOptionsParser
  where
    mkGlobalOpts x0 x1 x2 x3 x4 x5 = GlobalOpts x0 (PlanParams x1 x2 x3 x4 x5)

    option x ms = O.option x (mconcat ms)
    flag' x ms = O.flag' x (mconcat ms)

    ghcs =
        option (O.eitherReader eitherParsec)
        [ O.short 'g'
        , O.long "ghcs"
        , O.metavar ":version-range"
        , O.help "GHC versions to build with"
        ] <|>
        flag' (C.orLaterVersion $ C.mkVersion [8])
        [ O.short '8'
        , O.help "GHC>=8"
        ] <|>
        pure anyVersion

    constraints = option (O.eitherReader $ explicitEitherParsec namedConstraint)
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

    localrepo :: O.Parser (Path Absolute)
    -- TODO: this will error on non-absolute paths
    localrepo = O.option (O.maybeReader $ return . fromAbsoluteFilePath) $ mconcat
        [ O.long "local-repo"
        , O.metavar ":path"
        , O.help "Local file+noindex repository"
        ]

    includeDeprecated = a <|> b <|> pure IncludeDeprecated where
        a = flag' IncludeDeprecated
            [ O.long "include-deprecated"
            , O.help "Include deprecated packages (default)"
            ]

        b = flag' OmitDeprecated
            [ O.long "omit-deprecated"
            , O.help "Omit deprecated packages"
            ]


cmd :: O.Parser Cmd
cmd = O.subparser $ mconcat
    [ O.command "matrix" $ O.info cmdMatrix $ O.progDesc "Build matrix"
    , O.command "bounds" $ O.info cmdBounds $ O.progDesc "Find and check bounds"
    , O.command "sweep" $ O.info cmdSweep $ O.progDesc "Sweep dependencies"
    , O.command "get" $ O.info cmdGet $ O.progDesc "Fetch package sources"
    ]

cmdBounds :: O.Parser Cmd
cmdBounds = CmdBounds
    <$> verify
    <*> limit
    <**> O.helper
  where
    limit = lower <|> upper <|> pure LimitLower

    lower = O.flag' LimitLower $ mconcat
        [ O.long "lower"
        , O.help "Lower bounds"
        ]

    upper = O.flag' LimitUpper $ mconcat
        [ O.long "upper"
        , O.help "Upper bounds"
        ]

cmdSweep :: O.Parser Cmd
cmdSweep = CmdSweep
    <$> verify
    <**> O.helper

cmdMatrix :: O.Parser Cmd
cmdMatrix = CmdMatrix
    <$> verify
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
    <*> (Just <$> versionRange <|> latest <|> pure (Just anyVersion))
    <**> O.helper
  where
    name = O.argument (O.eitherReader eitherParsec) $ mconcat
        [ O.metavar ":pkgname"
        , O.help "Package name"
        ]

    versionRange :: O.Parser VersionRange
    versionRange = O.argument (O.eitherReader $ explicitEitherParsec p) $ mconcat
        [ O.metavar ":versions"
        , O.help "Version range"
        ]
      where
        p :: ParsecParser VersionRange
        p = parsec <|> C.thisVersion <$> parsec

    latest :: O.Parser (Maybe a)
    latest = O.flag' Nothing $ mconcat
        [ O.long "latest"
        , O.help "Latest version"
        ]

verify :: O.Parser Verify
verify = verify' <|> solveOnly <|> pure Verify
  where
    verify' = O.flag' Verify $ mconcat
        [ O.long "verify"
        , O.help "Try to build too"
        ]

    solveOnly = O.flag' SolveOnly $ mconcat
        [ O.long "solve-only"
        , O.help "Only solve"
        ]
