module Ventti where

import System.Random

--Data ja tyypit
data osallistuja = Pelaaja | Jakaja deriving (Show, Enum)
data maa = Pata | Risti | Ruutu | Hertta deriving (Show, Enum)
data kortinNo = [1 .. 13] deriving (Show, Enum)
p1Win = "Pelaaja voitti pelin!"
pcWin = "Tietokone voitti pelin!"
instance Show Value where
    show 1 = "Ässä"
    show 2 = "Kaksi"
    show 3 = "Kolme"
    show 4 = "Neljä"
    show 5 = "Viisi"
    show 6 = "Kuusi"
    show 7 = "Seitsemän"
    show 8 = "Kahdeksan"
    show 9 = "Yhdeksän"
    show 10 = "Kymmenen"
    show 11 = "Jätkä"
    show 12 = "Kuningatar"
    show 13 = "Kuningas"
type kortti = (maa, kortinNo)
type pakka = [kortti]

mahdPisteet :: [kortti] -> [Int]
mahdPisteet = go [0] where
        go ns (1:rest) = go (map ((+) 1) ns ++ map ((+) 11) ns) rest
        go ns (2:rest) = go (map ((+) 2) ns) rest
        go ns (3:rest) = go (map ((+) 3) ns) rest
        go ns (4:rest) = go (map ((+) 4) ns) rest
        go ns (5:rest) = go (map ((+) 5) ns) rest
        go ns (6:rest) = go (map ((+) 6) ns) rest
        go ns (7:rest) = go (map ((+) 7) ns) rest
        go ns (8:rest) = go (map ((+) 8) ns) rest
        go ns (9:rest) = go (map ((+) 9) ns) rest
        go ns (10:rest) = go (map ((+) 10) ns) rest
        go ns (11:rest) = go (map ((+) 10) ns) rest
        go ns (12:rest) = go (map ((+) 10) ns) rest
        go ns (13:rest) = go (map ((+) 10) ns) rest

pelaajaNo = (1, 2)

teePakka :: pakka
teePakka = [(maa, kortinNo) | maa <- [Pata .. Hertta], kortinNo <- [1 .. 13]]

type pakanState s = State pakka s

data Pelaaja = Pelaaja {
    kortit[pelaajaNo, kortit]
} deriving (Eq, Show)

head osallistuja = Pelaaja {
    pelaajaNo = 1
} deriving (Eq, Show)

tail osallistuja = Pelaaja {
    pelaajaNo = 2
} deriving (Eq, Show)

-- main
main :: IO ()
main = do
	putStrLn "Ohjelma aloitettu "
    luoPeli


luoPeli :: IO Game
luoPeli = do
    putStrLn "Peli aloitettu "
    teePakka

    jaa2korttiaP1 -- yhdeksi metodiksi?
    jaa2korttiaPC -- yhdeksi metodiksi?

	-- Käyttäjän syöte
    syote <- getLine

--satunnainen 52:stä kortista voi myös käydä
jaaKortti :: osallistuja
jaaKortti do
  curr <- get
  let n = length $ cards curr
      (i, gen') = randomR (0, n) $ gen curr
  card <- takeCardAt i
  put curr { gen = gen' }
  return card

jaa2korttiaP1 :: 

jaa2korttiaPC :: 

--Satunnaisen kortin jakamiseen
satunnainenMaa x = randomRIO (1, 4)
satunnainenNo x = randomRIO (1, 13)
satunnainenKortti

--Jaetun kortin poistaminen pakasta
poistaNoPakasta x = x
poistaMaaPakasta x = x
poistaKortti

--Ässän pisteiden määrän valintaan
valitseÄssänArvo :: Bool -> pisteArvot
valitseÄssänArvo b = if b == True then 1
    else if b == False then 11-

--Kuvakortin tunnistamiseen
onkoKuvakortti :: kortinNo -> Bool
onkoKuvakortti b = if b > 10 then True
    else False

--Ässän tunnistamiseen
onkoAssa :: kortinNo -> Bool
onkoAssa b = if b == 1 then True
    else False

skippaa

vuoronVaihto

getPisteet ::
getPisteet 

--Pelin voittajan päättämiseen
lopullisetPisteet :: pisteArvot -> pisteArvot -> Int
lopullisetPisteet x y = if x > y && x <= 21 then p1Win
    else if y > x && y <=21 then pcWin
        else if x == y then pcWin



--Ohjeistus:
--Jakaja jakaa aluksi kaksi korttia sekä pelaajalle että itselleen.
--Kuvakortit (kuningas, kuningatar ja jätkä) ovat numeroarvoltaan 10, ässä pelaajan valinnan mukaan joko 1 tai 11 ja muiden korttien numeroarvo on normaali.
--Ensimmäisen jaon jälkeen pelaaja voi vuorollaan joko pyytää lisäkortin tai tyytyä nykyisiin.
--Jakajan on pakko ottaa uusi kortti, mikäli hänellä on kasassa pisteitä 16 tai vähemmän, ja hänen on pakko pysähtyä, mikäli hänellä on pisteitä 17 tai enemmän.
--Mikäli pelaaja saa ventin eli tasan 21 pistettä, hän voittaa. Mikäli hän ylittää ventin, hän häviää.
--Mikäli pelaaja pääsi lähemmäs venttiä (sitä kuitenkaan ylittämättä), hän voittaa.
--Mikäli pelaaja ja jakaja päätyivät tasapeliin, jakaja voittaa.
--Toteuta järjestelmä, jossa käyttäjä pääsee pelaamaan venttiä tietokonejakajaa vastaan.