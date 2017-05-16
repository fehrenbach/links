#!/usr/bin/env stack
-- stack --resolver lts-8.13 script --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Turtle.Format

parser :: Parser (Int, Int)
parser = (,) <$> optInt "elements" 'n' "Size of the relation"
             <*> optInt "relations" 'm' "Number of relations"

main = do
    (n, m) <- options "n x m identity relations" parser
    sh $ do
      j <- select [1..m]
      printf ("DROP TABLE IF EXISTS r_"%d%"_"%d%";\n") n j
      printf ("CREATE TABLE r_"%d%"_"%d%" (a INTEGER PRIMARY KEY, b INTEGER) WITH OIDS;\n") n j
      printf ("INSERT INTO r_"%d%"_"%d%" (a, b) VALUES \n") n j
      sh $ do
        i <- select [1..n-1]
        printf ("("%d%","%d%"),\n") i i
      printf ("("%d%","%d%");\n") n n
