module Trustee.NewBuild (cmdNewBuild) where

import Control.Applicative        ((<|>))
import Control.Monad              (when)
import Distribution.Text          (display)
import System.Exit                (ExitCode (..))

import Trustee.GHC
import Trustee.Monad
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Util

data Result
    = ResultOk
    | ResultDryFail ExitCode String String
    | ResultDepFail ExitCode String String
    | ResultFail ExitCode String String
  deriving Show

isResultFail :: Result -> Maybe (String, String)
isResultFail (ResultFail _ o e) = Just (o, e)
isResultFail _                  = Nothing

isResultDepFail :: Result -> Maybe (String, String)
isResultDepFail (ResultDepFail _ o e) = Just (o, e)
isResultDepFail _                     = Nothing

cmdNewBuild :: GlobalOpts -> [String] -> M ()
cmdNewBuild opts args = do
    (_jGHC, jCabal) <- jobs
    let ghcs = ghcsInRange (goGhcVersions opts)

    xs <- forConcurrently ghcs $ \ghcVersion -> runEarlyExit $ do
        (ec0, o0, e0) <- lift $ runWithGHC True ghcVersion "cabal" $
            [ "new-build"
            , "-w", "ghc-" ++ display (toVersion ghcVersion)
            , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion)
            -- , "--ghc-options=" ++ ghcOptions jGHC ghcVersion
            , "-j" ++ show jCabal
            , "--dry-run"
            ] ++ args

        when (isFailure ec0) $ exit $ ResultDryFail ec0 o0 e0

        (ec1, o1, e1) <- lift $ runWithGHC False ghcVersion "cabal" $
            [ "new-build"
            , "-w", "ghc-" ++ display (toVersion ghcVersion)
            , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion)
            -- , "--ghc-options=" ++ ghcOptions jGHC ghcVersion
            , "--dependencies"
            , "-j" ++ show jCabal
            ] ++ args

        when (isFailure ec1) $ exit $ ResultDepFail ec1 o1 e1

        (ec2, o2, e2) <- lift $ runWithGHC False ghcVersion "cabal" $
            [ "new-build"
            , "-w", "ghc-" ++ display (toVersion ghcVersion)
            , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion)
            -- , "--ghc-options=" ++ ghcOptions jGHC ghcVersion
            , "--dependencies"
            , "-j" ++ show jCabal
            ] ++ args

        when (isFailure ec2) $ exit $ ResultDepFail ec2 o2 e2

        return ResultOk

    case findMaybe isResultFail xs of
        Just (o, e) -> putStrs
            [ colored Cyan $ "==== STDOUT " ++ replicate 60 '='
            , o
            , colored Red  $ "==== STDERR " ++ replicate 60 '='
            , e
            , colored Cyan $ replicate 72 '='
            ]
        Nothing -> case findMaybe isResultDepFail xs of
            Just (o, e) -> putStrs
                [ ""
                , colored Cyan $ "==== STDOUT " ++ replicate 60 '='
                , ""
                , o
                , ""
                , colored Red  $ "==== STDERR " ++ replicate 60 '='
                , ""
                , e
                , ""
                , colored Cyan $ replicate 72 '='
                , ""
                ]
            Nothing -> pure ()

    putStrs [ renderTable $ zipWith mkRow ghcs xs ]
  where
    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

    mkRow :: GHCVer -> Result -> [Txt]
    mkRow v r =
        [ mkTxt Black (display (toVersion v))
        , mkCell r
        ]

    mkCell :: Result -> Txt
    mkCell ResultOk         = mkTxt Green   "success"
    mkCell ResultDryFail {} = mkTxt Blue    "--dry-run failed"
    mkCell ResultDepFail {} = mkTxt Magenta "--dependencies failed"
    mkCell ResultFail {}    = mkTxt Red     "failure"

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe _ [] = Nothing
findMaybe f (x : xs) = f x <|> findMaybe f xs
