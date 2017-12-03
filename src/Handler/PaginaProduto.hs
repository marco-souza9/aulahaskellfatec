{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.PaginaProduto where

import Import
import Database.Persist.Postgresql

formProduto :: Form Produto 
formProduto = renderBootstrap $ Produto 
    <$> areq textField "Nome: " Nothing
    <*> areq doubleField "Preco: " Nothing
    <*> areq intField    "Estoque: " Nothing
    

getCadProdutoR :: Handler Html
getCadProdutoR = do 
     (widget, enctype) <- generateFormPost formProduto
     defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <form action=@{CadProdutoR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enviar">
            
        |]

postCadProdutoR :: Handler Html
postCadProdutoR = do 
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess produto -> do 
            runDB $ insert produto 
            redirect CadProdutoR
        _ -> redirect HomeR
        
getTodosProdR :: Handler Html
getTodosProdR = do 
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <td> Id
                        <td> Nome 
                        <td> Preco 
                        <td> Produto
                        <td> 
                
                <tbody>
                    $forall (Entity pid produto) <- produtos
                        <tr> 
                            <td> #{fromSqlKey pid}
                            <td> #{produtoNome produto}
                            <td> #{produtoPreco produto}
                            <td> #{produtoEstoque produto}
                            <td> 
                                <form action=@{ApagarProdR pid} method=post>
                                    <input type="submit" value="Deletar">
                            
        |]

postApagarProdR :: ProdutoId -> Handler Html 
postApagarProdR pid = do 
    runDB $ delete pid
    redirect TodosProdR        

getDetalheProdR :: ProdutoId -> Handler Html
getDetalheProdR pid = do 
    produto <- runDB $ get404 pid
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <div class="col-lg-12">
                <strong> Nome: #{produtoNome produto}<br>
                <strong> Preco: #{produtoPreco produto}<br>
                <strong> Estoque: #{produtoEstoque produto}
        |]