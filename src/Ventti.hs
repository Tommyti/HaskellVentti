module Ventti where

import Pakka

--Data ja tyypit
data osallistuja = Pelaaja | Jakaja deriving (Show, Enum)

p1Win = "Pelaaja voitti pelin!"
pcWin = "Tietokone voitti pelin!"

pelaajaNo = (1, 2)

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



jaa2korttiaP1 ::

jaa2korttiaPC ::



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
