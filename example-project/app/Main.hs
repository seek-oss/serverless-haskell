{-# LANGUAGE TemplateHaskell #-}
module Main where

import AWSLambda
import qualified ApiGW
import qualified IntArray
import qualified S3

generateLambdaDispatcher
