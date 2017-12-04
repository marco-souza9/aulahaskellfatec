{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric#-}
module Handler.Produto where

import Import

import Network.HTTP.Types.Status
import Database.Persist.Postgresql

data Nome = Nome {nome :: Text} deriving Generic
instance ToJSON Nome where
instance FromJSON Nome where

patchAlteraNomeR :: ProdutoId -> Handler Value
patchAlteraNomeR pid = do
    _ <- runDB $ get404 pid
    novoNome <- requireJsonBody :: Handler Nome
    runDB $ update pid [ProdutoNome =. (nome novoNome)]
    sendStatusJSON noContent204 (object ["resp" .= ("UPDATED " ++ show (fromSqlKey pid))])

putAlteraProdR :: ProdutoId -> Handler Value
putAlteraProdR pid = do
    _ <- runDB $ get404 pid
    novoProd <- requireJsonBody :: Handler Produto
    runDB $ replace pid novoProd
    sendStatusJSON noContent204 (object ["resp" .= ("UPDATED " ++ show (fromSqlKey pid))])

getBuscaProdR :: ProdutoId -> Handler Value
getBuscaProdR pid = do
    produto <- runDB $ get404 pid
    sendStatusJSON ok200 (object ["resp" .= (toJSON produto)])

--REST
--O get404 procura um registro, se achar prossegue, se nao, 
--barra o restante da funcao jogando um status 404
deleteProdutoDelR :: ProdutoId -> Handler Value
deleteProdutoDelR pid = do
    _ <- runDB $ get404 pid
    runDB $ delete pid
    sendStatusJSON noContent204 (object ["resp" .= ("DELETED " ++ show (fromSqlKey pid))])
 
postProdutoR :: Handler Value
postProdutoR = do
    prod <- requireJsonBody :: Handler Produto
    pid <- runDB $ insert prod
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey pid)])
    
getMenorEstoqueR :: Int -> Handler TypedContent
getMenorEstoqueR estoque = do
   -- PRODS EH UMA LISTA
   prods <- runDB $ selectList [ProdutoEstoque <=. estoque] [Asc ProdutoNome]
   sendStatusJSON ok200 (object ["resp" .= toJSON prods])