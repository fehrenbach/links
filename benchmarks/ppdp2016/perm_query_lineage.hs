#!/usr/bin/env stack
-- stack --resolver lts-8.13 script --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Turtle.Format

parser :: Parser (Int, Int)
parser = (,) <$> optInt "elements" 'n' "Size of the relation"
             <*> optInt "relations" 'm' "Number of relations"

main = do
    (n, m) <- options "query n x m identity relations" parser
    sh $ do
      printf ("\\COPY (")
    sh $ do
      printf ("SELECT PROVENANCE t_1.a, t_1.b FROM ")
      j <- select [1..m-1]
      printf ("r_"%d%"_"%d%" AS t_"%d%", ") n j j
    sh $ do
      printf ("r_"%d%"_"%d%" AS t_"%d%" ") n m m
      printf ("WHERE ")
      j <- select [1..m-2]
      printf ("t_"%d%".a = t_"%d%".a AND ") 1 (j+1)
    sh $ do
      printf ("t_"%d%".a = t_"%d%".a") 1 m
    sh $ do
      printf ") TO STDOUT WITH CSV\n"
