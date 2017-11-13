{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric#-}
module Handler.Compra where

import Import

import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postCompraR :: Handler TypedContent
postCompraR = do
    compra <- requireJsonBody :: Handler Compra
    _ <- runDB $ get404 $ compraCliid compra
    _ <- runDB $ get404 $ compraProid compra
    compid <- runDB $ insert compra
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey compid)])
    
third :: (a,b,c) -> a
third (a,_,_) = a

getListaProdR :: ClienteId -> Handler TypedContent
getListaProdR cid = do 
    lista <- runDB $ rawSql
        ("SELECT ??, ??, ?? \
        \FROM produto INNER JOIN compra \
        \ON produto.id=compra.proid INNER JOIN cliente \
        \ON compra.cliid = cliente.id WHERE cliente.id = " <> (pack $ show $ fromSqlKey cid))
        [] :: Handler [(Entity Produto, Entity Compra, Entity Cliente)]
    produtos <- return $ fmap third lista
    produtosSemId <- return $ fmap (\(Entity _ prod) -> prod) produtos
    sendStatusJSON ok200 (object ["resp" .= (toJSON produtosSemId)])
    
getListaProdFR :: ClienteId -> Handler TypedContent
getListaProdFR cid = do
    lista' <- runDB $ selectList [CompraCliid ==. cid] []
    lista <- return $ fmap (\(Entity _ comp) -> comp) lista'
    prodsIds <- return $ fmap compraProid lista 
    produtos <- sequence $ fmap (\pid -> runDB $ get404 pid) prodsIds
    sendStatusJSON ok200 (object ["resp" .= (toJSON produtos)])
    