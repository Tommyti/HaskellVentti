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
    vuoro :: Osallistuja,
    jakajaStrategia :: Strategia
}

type PeliTila a = StateT Peli a
type Strategia = [Kortti] -> PeliTila IO (Liike)

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
    setVuoro 1
    otaKortti
    otaKortti
    vaihdaVuoroa
    otaKortti
    otaKortti
    vaihdaVuoroa
    peliLooppi
    

--Pyörittää peliä
peliLooppi :: PeliTila IO ()
peliLooppi = do
    curr <- get
    when ((pelaajaLiike curr) == Hit) hoidaPelaaja
    when ((jakajaLiike curr) == Hit) hoidaJakaja
    peliLoppu <- onkoPeliLoppu
    when peliLoppu hoidaLoppunutPeli
    when (not peliLoppu) peliLooppi

onkoPeliLoppu :: PeliTila IO Bool
onkoPeliLoppu = do
    curr <- get
    let pelaaja      = pelaajaLiike curr
        jakaja       = jakajaLiike curr
        pelaajaK     = pelaajaKasi curr
        jakajaK      = jakajaKasi curr
        pysyvat     = (pelaaja == Pysyy && jakaja == Pysyy)
        pelaajaYli   = yli pelaajaK
        jakajaYli    = yli jakajaK
        peliLoppu    = pysyvat || pelaajaYli || jakajaYli

    when pelaajaYli $ liftIO . putStrLn $ "Pelaaja meni yli 21!"
    when jakajaYli $ liftIO . putStrLn $ "Jakaja meni yli 21!"

    return peliLoppu

hoidaLoppunutPeli :: PeliTila IO ()
hoidaLoppunutPeli = do
    curr <- get
    let pelaajaK = pelaajaKasi curr
        jakajaK  = jakajaKasi curr
        voittaja = voitti pelaajaK jakajaK
    liftIO . putStrLn $ "Kätesi: " ++ (show pelaajaK)
    liftIo . putStrLn $ "Jakajan käsi: " ++ (show jakajaK)
    when voittaja $ liftIO .  putStrLn $ "Voitit!"
    when (not voittaja) $liftIO . putStrLn $ "Hävisit!"

hoidaPelaaja :: Pelitila IO ()
hoidaPelaaja = do
    curr <- get
    input <- liftIO $ do
        let pelaajaKasi = pelaajaKasi curr
        putStrLn $ "Korttisi: " ++ (show pelaajaKasi)
        putStrLn $ "Jakajan kortti: " ++ (naytaJakaja $ jakajaKasi curr)
        putStrLn $ "Haluatko jatkaa? (Otakortti/Jää)"
        input <- getLine
        return input

    let vastaus = read input :: Liike
    when (vastaus == Otakortti) $ do
        let (kortti, pakka') = runState draw $ pakka curr
        put curr { pakka = pakka'
                 , pelaajaKasi = kortti : pelaajaKasi curr }
        uusi <- get
        let pelaajaK = uusi
        liftIO . putStrLn $ "Korttisi: " ++ (naytaJakaja pelaajaK)

    when (vastaus == Jää) $ do
        put curr { pelaajaLiike = Jää }

hoidaJakaja :: PeliTila IO ()
hoidaJakaja = do
    curr <- get
    liike <- jakajaStrategia curr $ jakajaKasi curr
    when (liike == Ota) $ do
        let (kortti, pakka') = runState jaa $ pakka curr
        put curr { pakka = pakka'
                 , jakajaKasi = kortti : jakajaKasi curr }
        uusi <- get
        let jakajaK = jakajaKasi uusi
        liftIO . putStrLn $ "Jakaja otti kortin"
        liftIO . putStrLn $ "Jakajan käsi: " ++ (naytaJakajanKasi jakajaK)
    when (liike == Jää) $ do
        put curr { jakajaLiike = Jää }
        liftIO . putStrLn $ "Jakaja ei ottanut korttia"


naytaJakaja :: [Kortti] -> String
naytaJakaja kasi = "[" ++ (show $ head kasi) ++ "," ++ (intersperse ',' hidden) ++ "]"
    where x = length $ tail kasi
          hidden = replicate x '?'


voitti :: [Kortti] -> [Kortti] -> Bool
voitti pelaajaK jakajaK = pelaajanPisteet > jakajanPisteet
    where pelaajanPisteet = score pelaajaKasi
          jakajanPisteet  = score jakajanKasi

pisteet :: [Kortti] -> Int
pisteet x
    | yli x    = 0
    | muuten   = paras x

yli :: [Kortti] -> Bool
yli = and . map ((<) 21) . mahdPisteet

kakskytYks :: [Kortti] -> Bool
kakskytYks = any ((==) 21) . mahdPisteet

paras :: [Kortti] -> Int
paras = maximum . filter ((>=) 21) . mahdPisteet

--Ota kortti omaan käteen
otaKortti :: [Kortti]
otaKortti = otaXKortti 0

teePeli :: StdGen -> Strategia -> Peli
teePeli x strategia = Peli
    { pakka = p'
    , pelaajaKasi = pelaajaK
    , pelaajaLiike = Jaa kortti
    , jakajaKasi = jakajaK
    , jakajaLiike = Jaa kortti
    , jakajaStrategia = strategia }
    where p = execState sekoitaPakka $ teePeli x
          ((pelaajaK, jakajaK), p') = runState jaa $ p

--Jää
jaa :: PakanTila ([Kortti], [Kortti])
jaa = do
    pelaaja   <- ota
    jakaja    <- ota
    pelaaja'  <- ota
    jakaja'   <- ota
    let pelaaja1 = [pelaaja, pelaaja']
        jakaja1  = [jakaja, jakaja']
    return (pelaaja1, jakaja1)

--Vuoron asetus funktio, 1=pelaaja 2=jakaja
--setVuoro :: Int -> Osallistuja
--setVuoro x = 

--Vuoron vaihto funktio
{-
vaihdaVuoroa :: Osallistuja -> Osallistuja
vaihdaVuoroa = do x=getVuoro
    if x==Pelaaja return Jakaja
    else return Pelaaja

--Vuoron get funktio
getVuoro :: Osallistuja
getVuoro = do x=vuoro
    return x
-}
--Pisteiden get funktio
getPisteet :: Int
getPisteet = do

--Jakajan ja pelaajan korttien tulostamiseen
--getTilanne :: 

--Jakajan liikeen päättämiseen
{-
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
    3 -> 3
    _ -> 0 --Käyttäjän väärälle syötteelle

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
-}


--Ohjeistus:
--Jakaja jakaa aluksi kaksi korttia sekä pelaajalle että itselleen.
--Kuvakortit (kuningas, kuningatar ja jätkä) ovat numeroarvoltaan 10, ässä pelaajan valinnan mukaan joko 1 tai 11 ja muiden korttien numeroarvo on normaali.
--Ensimmäisen jaon jälkeen pelaaja voi vuorollaan joko pyytää lisäkortin tai tyytyä nykyisiin.
--Jakajan on pakko ottaa uusi kortti, mikäli hänellä on kasassa pisteitä 16 tai vähemmän, ja hänen on pakko pysähtyä, mikäli hänellä on pisteitä 17 tai enemmän.
--Mikäli pelaaja saa ventin eli tasan 21 pistettä, hän voittaa. Mikäli hän ylittää ventin, hän häviää.
--Mikäli pelaaja pääsi lähemmäs venttiä (sitä kuitenkaan ylittämättä), hän voittaa.
--Mikäli pelaaja ja jakaja päätyivät tasapeliin, jakaja voittaa.
--Toteuta järjestelmä, jossa käyttäjä pääsee pelaamaan venttiä tietokonejakajaa vastaan.
