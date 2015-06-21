{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cypher.Query
import Network.Wreq

main = post "http://httpbin.org/post" ["num" := 3, "str" := "wat"]
