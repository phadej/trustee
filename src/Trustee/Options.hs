module Trustee.Options (
    Cmd (..),
    GlobalOpts (..),
    Limit (..),
    parseOpts,
    ) where

import Control.Applicative  ((<**>), (<|>), many)
import Distribution.Text    (simpleParse)
import Distribution.Package (PackageName)
import Distribution.Version (VersionRange, anyVersion)

import qualified Options.Applicative as O

newtype GlobalOpts = GlobalOpts
    { goGhcVersions :: VersionRange
    }
  deriving Show

data Cmd
    = CmdNewBuild [String]
    | CmdMatrix [FilePath] [String]
    | CmdGet PackageName VersionRange
    | CmdBounds Bool Limit
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
    , O.command "get" $ O.info cmdGet $ O.progDesc "Fetch package sources"
    , O.command "bounds" $ O.info cmdBounds $ O.progDesc "Find and check bounds"
    ]

cmdBounds :: O.Parser Cmd
cmdBounds = CmdBounds
    <$> O.switch (mconcat
        [ O.help "verify plans"
        , O.long "verify"
        ])
    <*> limit
  where
    limit = lower <|> upper

    lower = O.flag' LimitLower $ mconcat
        [ O.long "lower"
        , O.help "Lower bounds"
        ]

    upper = O.flag' LimitUpper $ mconcat
        [ O.long "upper"
        , O.help "Upper bounds"
        ]
        

cmdNewBuild :: O.Parser Cmd
cmdNewBuild = CmdNewBuild <$> many (O.strArgument $ mconcat
    [ O.metavar "arg"
    , O.help "arguments to cabal new-build"
    ])

cmdMatrix :: O.Parser Cmd
cmdMatrix = CmdMatrix <$> many pkgs <*> many constraints where
    pkgs = O.strArgument $ mconcat
        [ O.metavar "pkg-dir"
        , O.help "package directories to include in matrix"
        ]

    constraints = O.strOption $ mconcat
        [ O.short 'c'
        , O.long "constraint"
        , O.metavar ":constraint"
        , O.help "Additional constraints"
        ]

cmdGet :: O.Parser Cmd
cmdGet = CmdGet <$> name <*> (version <|> pure anyVersion) where
    name = O.argument (O.maybeReader simpleParse) $ mconcat
        [ O.metavar ":pkgname"
        , O.help "Package name"
        ]

    version = O.argument (O.maybeReader simpleParse) $ mconcat
        [ O.metavar ":versions"
        , O.help "Version range"
        ]
