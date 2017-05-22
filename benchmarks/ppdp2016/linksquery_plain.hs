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
      printf ("var i_s_c_o_"%d%"_"%d%" =\n") n j
      printf ("table \"i_s_c_o_"%d%"_"%d%"\" with (oid: Int, i: Int, s: String, cardinal: String, ordinal: String)\n") n j
      printf ("where oid readonly tablekeys [[\"oid\"], [\"i\"]] from db;\n")
    sh $ do
      printf ("query { \n")
      j <- select [1..m]
      printf ("for (t_"%d%" <-- i_s_c_o_"%d%"_"%d%")\n") j n j
    sh $ do
      printf ("where (mod(t_1.i, 10) == 9 && ")
      j <- select [1..m-2]
      printf ("t_"%d%".i == t_"%d%".i && ") 1 (j+1)
    sh $ do
      printf ("t_"%d%".i == t_"%d%".i)\n") 1 m
      printf ("[(i = t_1.i, c = t_1.cardinal)] }\n")
