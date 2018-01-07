module Trustee.LowerBounds where

import Path (Path, Abs, Dir)

import Trustee.Monad
import Trustee.Options

cmdLowerBounds :: GlobalOpts -> Path Abs Dir -> M ()
cmdLowerBounds _ _ = pure ()


{-


lowerboundsCmd  :: [String] -> IO a
#ifndef LOWERBOUNDS_ENABLED
lowerboundsCmd _ = dieCli [Error "Compiled without +lowerbounds"]
#else
lowerboundsCmd argv = do
    let path = "cabal.project"
    Just proj <- runYamlWriter Nothing $ readProject path
    index <- loadIndex

    cols <- fmap M.unions $ forConcurrently ghcVersions $ \ghcVersion -> do
        cells <- lowerBoundsForGhc (null argv) index proj ghcVersion
        return $ M.mapKeysMonotonic ((,) ghcVersion) cells

    printTable cols

    -- TODO: exit code based on table result!
    exitSuccess
  where
    ghcVersions =
        -- [ mkVersion [7,0,4]
        -- , mkVersion [7,2,2]
        -- , mkVersion [7,4,2]
        -- , mkVersion [7,6,3]
        [ mkVersion [7,8,4]
        , mkVersion [7,10,3]
        , mkVersion [8,0,2]
        , mkVersion [8,2,2]
        ]

data Cell
    = CellOk Version
    | CellHigh Version Version
    | CellNoVersion
    | CellBuildError Version String String
  deriving (Eq, Ord, Show)

cellText :: Cell -> Txt
cellText (CellOk v)             = Txt ColorGreen   (length s) s where s = display v
cellText CellNoVersion          = Txt ColorBlue    (length s) s where s = "no-ver"
cellText (CellHigh v _)         = Txt ColorCyan    (length s) s where s = display v
cellText (CellBuildError v _ _) = Txt ColorRed     (length s) s where s = display v

thrPutStrLnQSem :: QSem
thrPutStrLnQSem = unsafePerformIO (newQSem 1)
{-# NOINLINE thrPutStrLnQSem #-}

thrPutStrLn :: String -> IO ()
thrPutStrLn s = bracket (waitQSem thrPutStrLnQSem) (\_ -> signalQSem thrPutStrLnQSem) $ \() -> do
    putStrLn s
    hFlush stdout

emptyTxt :: Txt
emptyTxt = Txt ColorNormal 0 ""

textLength :: Txt -> Int
textLength (Txt _ n _) = n

printTable :: M.Map (Version, PackageName) Cell -> IO ()
printTable m = do
    putStrLn header
    forM_ packageNames $ \pn -> do
        putStrLn $ packageRow pn
  where
    m' = fmap cellText m

    ghcVersions  = sortNub $ [ v | (v, _) <- M.keys m ]
    packageNames = sortNub $ [ n | (_, n) <- M.keys m ]

    packageNamesW = maximum (0 : map (length . display) packageNames) + 2
    ghcVersionsW =
        [ maximum (6 : [ textLength rc | ((v', _), rc) <- M.toList m', v == v' ]) + 2
        | v <- ghcVersions
        ]

    header :: String
    header = leftPad "" packageNamesW ++
        concat [ leftPad (display v) w | (v, w) <- zip ghcVersions ghcVersionsW ]

    packageRow :: PackageName -> String
    packageRow pn = leftPad (display pn) packageNamesW ++ concat
        [ leftPadTxt (fromMaybe emptyTxt $ M.lookup (v, pn) m') w
        | (v, w) <- zip ghcVersions ghcVersionsW
        ]


    leftPadTxt :: Txt -> Int -> String
    leftPadTxt (Txt ColorNormal n' str) n = str ++ replicate (n - n') ' '
    leftPadTxt (Txt c           n' str) n = colorCode c ++ str ++ colorReset ++ replicate (n - n') ' '

leftPad :: String -> Int -> String
leftPad str n = str ++ replicate (n - length str) ' '

sortNub :: Ord a => [a] -> [a]
sortNub = S.toList . S.fromList

cabalQSem :: QSem
cabalQSem = unsafePerformIO $ do
    n <- getNumCapabilities
    newQSem n
{-# NOINLINE cabalQSem #-}

lowerBoundsForGhc
    :: Bool             -- build
    -> Index            -- Index
    -> Project Package  -- packages
    -> Version          -- GHC version
    -> IO (M.Map PackageName Cell)
lowerBoundsForGhc build index proj ghcVersion = do
    let deps  = allBuildDepends ghcVersion proj
    let deps' = M.intersectionWith withinRange' index deps

    -- non-dry sem
    qsem <- newQSem 1

    fmap M.fromList $ forConcurrently (M.toList deps') $ \(pkgName, vs) -> do
        x <- findLowest qsem pkgName Nothing vs
        return (pkgName, x)
  where
    withinRange' vs vr = filter (`withinRange` vr) $ S.toList vs

    pfx pkgName v = "GHC " ++ leftPad (display ghcVersion) 6  ++ " " ++ leftPad pkgName 20 ++ " " ++ leftPad v' 10 ++ " "
      where
        v' | v == version0 = ""
           | otherwise = display v

    version0 = mkVersion []

    findLowest :: QSem -> PackageName -> Maybe Version -> [Version] -> IO Cell
    findLowest _ (display -> pkgName) _ [] = do
        thrPutStrLn $ colorCode ColorBlue ++ pfx pkgName version0 ++ "No version found" ++ colorReset
        return CellNoVersion
    findLowest qsem pn@(display -> pkgName) u (v : vs) = do
        (ec, o, e) <- bracket (waitQSem cabalQSem) (\_ -> signalQSem cabalQSem) $ \() -> readProcessWithExitCode
            "cabal"
            [ "new-build"
            , "--builddir=.dist-newstyle-" ++ display ghcVersion ++ "-" ++ pkgName ++ "-" ++ display v
            , "-w", "ghc-" ++ display ghcVersion
            , "--disable-tests", "--disable-benchmarks"
            , "--constraint=" ++ pkgName ++ " ==" ++ display v
            , "--ghc-options=" ++ ghcOptions
            , "--dry-run"
            , "all"
            ]
            ""
        _ <- evaluate (force o)
        _ <- evaluate (force e)

        case ec of
            ExitSuccess -> do
                thrPutStrLn $ colorCode ColorMagenta ++ pfx pkgName v ++ "Install plan found" ++ colorReset
                hFlush stdout

                verify qsem pn u v

            ExitFailure _ -> do
                thrPutStrLn $ pfx pkgName v ++ "Install-plan not found, trying next version..."
                hFlush stdout
                findLowest qsem pn (u <|> Just v) vs

    cellDone Nothing v  = CellOk v
    cellDone (Just u) v
        | take 2 (versionNumbers u) == take 2 (versionNumbers v) = CellOk v
        | otherwise = CellHigh v u

    verify :: QSem -> PackageName -> Maybe Version -> Version -> IO Cell
    verify qsem (display -> pkgName) u v
        | not build = return $ cellDone u v
        | otherwise = bracket (waitQSem qsem) (\_ -> signalQSem qsem) $ \() -> do
            thrPutStrLn $ colorCode ColorCyan ++ pfx pkgName v ++ "Trying to build" ++ colorReset

            (ec, o', e') <- readProcessWithExitCode
                "cabal"
                [ "new-build"
                , "-w", "ghc-" ++ display ghcVersion
                , "--disable-tests", "--disable-benchmarks"
                , "--constraint=" ++ pkgName ++ " ==" ++ display v
                , "--ghc-options=" ++ ghcOptions
                , "all"
                ]
                ""
            o <- evaluate (force o')
            e <- evaluate (force e')
            case ec of
                ExitSuccess   -> do
                    thrPutStrLn $ colorCode ColorGreen ++ pfx pkgName v ++ "Build OK" ++ colorReset
                    return $ cellDone u v
                ExitFailure _ -> do
                    thrPutStrLn $ colorCode ColorRed ++ pfx pkgName v ++ "Build failed" ++ colorReset
                    thrPutStrLn o
                    thrPutStrLn e
                    thrPutStrLn $ colorCode ColorRed ++ pfx pkgName v ++ "Build failed" ++ colorReset
                    return $ CellBuildError v o e

    ghcOptions
        | ghcVersion >= mkVersion [7,8] = "-j2 +RTS -A64m -I0 -qg -RTS"
        | otherwise                     = "    +RTS -A64m -I0 -qg -RTS"

allBuildDepends :: F.Foldable f => Version ->  f Package -> M.Map PackageName VersionRange
allBuildDepends ghcVersion
    = M.map simplifyVersionRange
    . M.unionsWith intersectVersionRanges
    . map extractBuildDepends
    . F.toList
  where
    extractBuildDepends :: Package -> M.Map PackageName VersionRange
    extractBuildDepends Pkg{pkgGpd} =
        maybe M.empty libBD (condLibrary pkgGpd)

    libBD :: PD.CondTree PD.ConfVar [Dependency] PD.Library
          -> M.Map PackageName VersionRange
    libBD
        = M.fromListWith intersectVersionRanges
        . map dependencyToPair
        . fst
        . simplifyCondTree (Right . evalConfVar)

    dependencyToPair (Dependency p vr) = (p, vr)

    evalConfVar :: PD.ConfVar -> Bool
    evalConfVar (PD.OS Linux)    = True
    evalConfVar (PD.OS _)        = False
    evalConfVar (PD.Impl GHC vr) = ghcVersion `withinRange` vr
    evalConfVar (PD.Impl _ _)    = False
    evalConfVar _                = True


#endif


-}
