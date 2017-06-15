module Ventti where

import Pakka
import Control.Monad.State

--Data ja tyypit
data Osallistuja = Pelaaja | Jakaja deriving (Show, Eq)
data Liike = Ota kortti | Jää (Show, Eq)

--Pelin ja pelaajien tiedot
data Peli = Peli {
    pakka :: Pakka,
    pelaajaKasi :: [Kortti],
    jakajaKasi :: [Kortti],
    pelaajaLiike :: Liike,
    jakajaLiike :: Liike
}

type PeliTila a = StateT Peli a

p1Win = "Pelaaja voitti pelin!"
pcWin = "Tietokone voitti pelin!"

-- main
main :: IO ()
main = do
	putStrLn "Ohjelma aloitettu "
    luoPeli

--Pelin luominen
luoPeli :: IO Peli
luoPeli = do
    putStrLn "Peli aloitettu "
    teePakka
    --sekoitaPakka

    
    jaa2korttiaP1 -- yhdeksi metodiksi?
    jaa2korttiaPC -- yhdeksi metodiksi?

	-- Käyttäjän syöte
    syote <- getLine


peliLooppi :: IO PeliTila
peliLooppi = do
    curr <- get

--Jää
jaa :: 

--Kortin ottamis metodi
otaKortti :: 


--Vuoron vaihto metodi
vaihdaVuoroa :: 


--Vuoron get metodi
getVuoro :: Bool


--Pisteiden get metodi
getPisteet :: Int
getPisteet


--Pelaajan syötteen tarkistus
syoteTarkistus ::


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
