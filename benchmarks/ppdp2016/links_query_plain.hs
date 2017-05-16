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
      printf ("var db = database \"links\";\n")
      j <- select [1..m]
      printf ("var r_"%d%"_"%d%" =\n") n j
      printf ("table \"r_"%d%"_"%d%"\" with (oid: Int, a: Int, b: Int)\n") n j
      printf ("where oid readonly tablekeys [[\"oid\"], [\"a\"], [\"b\"]] from db;\n")
    sh $ do
      printf ("query { \n")
      j <- select [1..m]
      printf ("for (t_"%d%" <-- r_"%d%"_"%d%")\n") j n j
    sh $ do
      printf ("where (")
      j <- select [1..m-2]
      printf ("t_"%d%".a == t_"%d%".a && ") 1 (j+1)
    sh $ do
      printf ("t_"%d%".a == t_"%d%".a)\n") 1 m
      printf ("[(a = t_1.a, b = t_1.b)] }\n")
