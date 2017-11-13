{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric#-}
module Handler.Pagina where

import Import
import Text.Julius


{--
hj vai explicar sobre internacionalização

vamos usar o foundation.hs

vai em routes e antes das rotas ja existem vai escrever uma rota de front-end:
/                               HomeR    GET

@ interpolar link

# interpolar 

^ interpolar widget

$maybe -> Handler (Maybe a)

$forall -> Handler([a])    
    
--}
getPag1R :: Handler Html
getPag1R = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color: pink;
            }
        |]
        [whamlet| 
            <h1> Pagina 1
            <a href=@{HomeR}> Voltar </a>
        |]
        
getPag2R :: Handler Html
getPag2R = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color: yellow;
            }
        |]
        [whamlet| 
            <h1> Pagina 2
            <a href=@{HomeR}> Voltar </a>
        |]
        
getPag3R :: Handler Html
getPag3R = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color: #4e4e4e;
            }
        |]
        [whamlet| 
            <h1> Pagina 3
            <a href=@{HomeR}> Voltar </a>
        |]

soma :: Int -> Int -> Int
soma x y = x + y

getAddR :: Int -> Int -> Handler Html
getAddR x y = do
    defaultLayout $ do
        [whamlet|
            <h1> A SOMA E: #{soma x y}
        |]

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do 
        toWidgetHead $ $(juliusFile "templates/home.julius")
        addStylesheet $ (StaticR css_home_css)
        addStylesheet $ (StaticR css_bootstrap_css)
        $(whamletFile "templates/home.hamlet")
        
paginaDentro :: Widget
paginaDentro = do 
    toWidget $ [cassius|
        h1
            color:orange;
    |]
    [whamlet|
        <h1> Ola mundo
    |]
        
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (x:xs) = Just xs

getTailR :: Text -> Handler Html
getTailR palavra = do
    palString <- return $ unpack palavra
    t <- return $ safeTail palString --Handler (Maybe [a])
    defaultLayout $ do
        [whamlet|
            $maybe jt <- t
                <h1> O TAIL E: #{jt}
            $nothing
                <h1> ERRO
        |]

getExemploR :: Handler Html
getExemploR = do
        defaultLayout $ do
            addStylesheet $ (StaticR css_bootstrap_css)
            [whamlet|
                <div class="container">
                    ^{paginaDentro}
            |]
            
getListR :: Handler Html
getListR = do
    lista <- return $ ["Santos","Gremio","Palmeiras","Cruzeiro","Botafogo","Flamengo"] :: Handler [String]
    defaultLayout $ do
        [whamlet|
            <ul>
            $forall time <- lista
                <li> #{time}
        |]