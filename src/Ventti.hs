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

--Main funktio
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
    --Korttien jakaminen pelin alussa
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
vaihdaVuoroa = do 
    let y = getVuoro
    if y == Pelaaja return Jakaja
    if y == Jakaja return Pelaaja

--Vuoron get funktio
getVuoro :: Osallistuja
getVuoro = do x=vuoro
    return x

--Pisteiden get funktio
getPisteet :: Int
getPisteet = do

--Tarkistaa pelaajan ja jakajan pisteet
tarkistaPisteet ::

getJakajanPisteet :: Int

--Jakajan ja pelaajan korttien tulostamiseen
tulostaTilanne :: 


--Jakajan liikeen päättämiseen
jakajanLiike :: jakajaKasi -> Liike
jakajanLiike kasi
  | pisteet < 17 = Otakortti
  | pisteet == 17 = if softKasi kasi then Otakortti else Jää
  | otherwise = Jää
  where pisteet = getJakajanPisteet kasi

--"Pehmeä käsi", jakaja ottaa vielä yhden kortin lisää jos tosi ja pisteet=17
softKasi :: [Kortti] -> Bool
softKasi kasi = elem kasi 1 --ässä

--Pelaajan syötteen tarkistus
syoteTarkistus :: a -> Int
syoteTarkistus x = case x of
    1 -> 1
    2 -> 2
    _ -> 3 --Muut syötteet

--Pelaajan liike
pelaajanLiike :: Int -> Liike
pelaajanLiike numero
  | numero == 1 = Otakortti
  | numero == 2 = Jää
  | numero == 3 = Tulosta

--Pelin voittajan päättämiseen pelin lopussa, x=pelaajan pisteet ja y= jakajan pisteet
lopullisetPisteet :: pisteArvot -> pisteArvot -> Osallistuja
lopullisetPisteet x y = if x > y && x <= 21 then p1Win
    else if y > x && y <=21 then pcWin
        else if x == y then pcWin