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
      printf ("SELECT PROVENANCE ON CONTRIBUTION (WHERE) ")
    sh $ do
      j <- select [1..m-1]
      printf ("t_"%d%".cardinal, ") j
    sh $ do
      printf ("t_"%d%".cardinal FROM ") m
      j <- select [1..m-1]
      printf ("i_s_c_o_"%d%"_"%d%" AS t_"%d%", ") n j j
    sh $ do
      printf ("i_s_c_o_"%d%"_"%d%" AS t_"%d%" ") n m m
      printf ("WHERE ")
      j <- select [1..m-2]
      printf ("t_"%d%".i = t_"%d%".i AND ") 1 (j+1)
    sh $ do
      printf ("t_"%d%".i = t_"%d%".i") 1 m
    sh $ do
      printf ") TO STDOUT WITH CSV\n"
