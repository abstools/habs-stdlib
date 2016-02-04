module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html
import System.Environment (withArgs)
import System.IO (hSetBuffering, BufferMode( LineBuffering ), stderr)

main :: IO ()
main = withArgs ["-j1"] $ -- don't run tests in parallel because it messes output
       hSetBuffering stderr LineBuffering >>
       defaultMainWithIngredients (htmlRunner:defaultIngredients)(
         localOption (mkTimeout 1000000) $ -- timeouts any test at 1s
         testGroup "group1"
                     [ testCase "case_stub" case_stub

                     ])

case_stub :: IO ()
case_stub = return ()
