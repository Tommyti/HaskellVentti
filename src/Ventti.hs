module Ventti where

import Pakka
import Control.Monad.State

--Data ja tyypit
data Osallistuja = Pelaaja | Jakaja deriving (Show, Eq)
data Liike = Otakortti | Jää | Tulosta (Show, Eq) --Tilanteen tulostaminen testaamiseen

--Pelin ja pelaajien tiedot
data Peli = Peli {
    pakka :: Pakka,
    pelaajaKasi :: [Kortti],
    jakajaKasi :: [Kortti],
    pelaajaLiike :: Liike,
    jakajaLiike :: Liike,
    pelaajaPisteet :: Int,
    jakajaPisteet :: Int,
    vuoro :: Osallistuja
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
luoPeli :: IO PeliTila
luoPeli = do
    putStrLn "Peli aloitettu "
    teePakka
    sekoitaPakka
    setVuoro 1
    otaKortti
    otaKortti
    vaihdaVuoroa
    otaKortti
    otaKortti
    vaihdaVuoroa
    peliLooppi
    

--Pyörittää peliä
peliLooppi :: IO PeliTila
peliLooppi = do
    curr <- get

    getTilanne
    -- Käyttäjän syöte
    syote <- getLine

    --otaKortti
    --getPisteet

--Ota kortti omaan käteen
otaKortti :: [Kortti]
otaKortti = otaXKortti 0

--Jää
jaa :: Osallistuja
jaa = vaihdaVuoroa

--Vuoron asetus funktio, 1=pelaaja 2=jakaja
setVuoro :: Int -> Osallistuja
setVuoro x = x

--Vuoron vaihto funktio
vaihdaVuoroa :: Osallistuja -> Osallistuja
vaihdaVuoroa = do y = getVuoro
    if y == Pelaaja return Jakaja
    else return Pelaaja

--Vuoron get funktio
getVuoro :: Osallistuja
getVuoro = do x=vuoro
    return x

--Pisteiden get funktio
getPisteet :: Int
getPisteet = do

getJakajanPisteet :: Int

--Jakajan ja pelaajan korttien tulostamiseen
getTilanne :: 


--Pelaajan syötteen tarkistus
syoteTarkistus :: a -> Int
syoteTarkistus x = case x of
    1 -> 1
    2 -> 2
    3 -> 3
    _ -> 0 --Käyttäjän väärälle syötteelle

--Jakajan liikeen päättämiseen
jakajanLiike :: jakajaKasi -> Liike
jakajanLiike kasi
  | pisteet < 17 = Otakortti
  | pisteet == 17 = if softKasi kasi then Otakortti else Jää
  | otherwise = Jää
  where pisteet = getJakajanPisteet kasi

pelaajanLiike :: Int -> Liike
pelaajanLiike numero
  | numero == 1 = Otakortti
  | numero == 2 = Jää
  | numero > 2 = Tulosta


--"Pehmeä käsi"
softKasi :: [Kortti] -> Bool
softKasi kasi = elem kasi 1

--Pelin voittajan päättämiseen
--lopullisetPisteet :: pisteArvot -> pisteArvot -> Int
--lopullisetPisteet x y = if x > y && x <= 21 then p1Win
--    else if y > x && y <=21 then pcWin
--        else if x == y then pcWin



--Ohjeistus:
--Jakaja jakaa aluksi kaksi korttia sekä pelaajalle että itselleen.
--Kuvakortit (kuningas, kuningatar ja jätkä) ovat numeroarvoltaan 10, ässä pelaajan valinnan mukaan joko 1 tai 11 ja muiden korttien numeroarvo on normaali.
--Ensimmäisen jaon jälkeen pelaaja voi vuorollaan joko pyytää lisäkortin tai tyytyä nykyisiin.
--Jakajan on pakko ottaa uusi kortti, mikäli hänellä on kasassa pisteitä 16 tai vähemmän, ja hänen on pakko pysähtyä, mikäli hänellä on pisteitä 17 tai enemmän.
--Mikäli pelaaja saa ventin eli tasan 21 pistettä, hän voittaa. Mikäli hän ylittää ventin, hän häviää.
--Mikäli pelaaja pääsi lähemmäs venttiä (sitä kuitenkaan ylittämättä), hän voittaa.
--Mikäli pelaaja ja jakaja päätyivät tasapeliin, jakaja voittaa.
--Toteuta järjestelmä, jossa käyttäjä pääsee pelaamaan venttiä tietokonejakajaa vastaan.
