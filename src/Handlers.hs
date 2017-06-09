{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}  -- comentar o que é isso e os pontos abaixo 
module Handlers where
import Import
import Foundation
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text  
import Yesod.Static
import Data.Maybe

import Database.Persist.Postgresql

mkYesodDispatch "HaskellLivro" pRoutes


-- cadastro de usuário
formUser :: Form (Perfil ,Usuario)
formUser = renderDivs $ (,) <$> (Perfil <$> --
          areq textField "Nome: " Nothing <*>
          areq textField "CPF: " Nothing <*>
          areq textField "Cidade: " Nothing <*>
          areq textField "Email: " Nothing <*>
          areq textField "Endereço: " Nothing <*>
          areq textField "Telefone: " Nothing  <*>
          areq hiddenField "" (Just (toSqlKey 0)) <*> --
          areq (selectField listarInstituicao) "Instituicao" Nothing) <*>
          (Usuario <$>
          areq textField "Login: " Nothing <*>
          areq passwordField "Senha: " Nothing) 
          
          
listarInstituicao = do
       entidades <- runDB $ selectList [] [Asc InstituicaoNome] 
       optionsPairs $ fmap (\ent -> (instituicaoNome $ entityVal ent, entityKey ent)) entidades


-- form de login
formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
          areq textField "Login: " Nothing <*>
          areq passwordField "Senha: " Nothing

-- form de BUSCA
formBusca :: Form Text
formBusca = renderDivs  $ 
          areq textField "Titulo do livro: " Nothing 
           
-- cadastro de livro           
formLivro :: Form (Livro, LivroGenero)
formLivro = renderDivs $ (,) <$> (Livro <$>
          areq textField "Titulo: " Nothing <*>
          areq textField "Autor: " Nothing <*>
          areq textField "Editora: " Nothing <*>
          areq (selectField listarInstituicao) "Instituição" Nothing <*>
          areq textField "Quantidade: " Nothing)<*>(LivroGenero <$>
          areq hiddenField "" (Just (toSqlKey 0)) <*>
          areq (selectField listarGenero) "Genero" Nothing) 
          --genero e livro dentro de uma tupla para enviar 
          
          
-- cadastro de instituicao           
formInstituicao :: Form Instituicao
formInstituicao = renderDivs $ Instituicao <$>
          areq textField "Nome: " Nothing <*>
          areq textField "cnpj: " Nothing <*>
          areq textField "endereco: " Nothing <*>
          areq textField "telefone: " Nothing 
          
-- cadastro de instituicao           
formGenero :: Form Genero
formGenero = renderDivs $ Genero <$>
          areq textField "Nome: " Nothing 

listarGenero = do
       entidades <- runDB $ selectList [] [Asc GeneroNome ] 
       optionsPairs $ fmap (\ent -> (generoNome $ entityVal ent, entityKey ent)) entidades 
          




----------------------- POSTS

-- página de login
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsuarioLogin ==. login, UsuarioSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)
               _ -> redirect ErroR
               
-- validação cadastro de usuário
postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess (perfil@(Perfil nome cpf cidade email endereco telefone usuario instituicao), user) -> (runDB $ do
               piid <- insert user 
               insert(Perfil nome cpf cidade email endereco telefone piid instituicao))
               
               -- >>= \piid ->  
                 --   runDB $ insert(Perfil nome cpf cidade email endereco telefone piid instituicao) >>= \ _ ->   redirect (UsuariosR)
                 >> redirect (UsuariosR) 
               _ -> redirect ErroR


-- validação cadastro de livro
postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formLivro
           case result of 
               FormSuccess (livro, genero) -> (runDB $ do
                    liid <-insert livro
                    insert(LivroGenero liid (livroGeneroGenero genero))
                    return liid)
                    >>= \idenlivro -> redirect (LivroR idenlivro)
               _ -> redirect ErroR
               
               

-- validação cadastro de instituicao
postCadastroInR :: Handler Html
postCadastroInR = do
           ((result, _), _) <- runFormPost formInstituicao
           case result of 
               FormSuccess instituicao -> (runDB $ insert instituicao) >>= \aiid -> redirect (InstituicaoR aiid)
               _ -> redirect ErroR
               
               
-- validação cadastro de genero
postCadastroGenR :: Handler Html
postCadastroGenR = do
           ((result, _), _) <- runFormPost formGenero
           case result of 
               FormSuccess genero -> (runDB $ insert genero) >>= \aiid -> redirect (GeneroR aiid)
               _ -> redirect ErroR               
             
                
-- validação update de livro
postUpdateLivroR :: LivroId -> Handler Html
postUpdateLivroR aid = do
           ((result, _), _) <- runFormPost formLivro
           case result of 
               FormSuccess (livro, genero) -> (runDB $ Database.Persist.Postgresql.replace aid livro) >>= \eiid -> redirect (GerenciarBooksR)
               _ -> redirect ErroR


-- validação update de instituicao
postUpdateInstituicaoR :: InstituicaoId -> Handler Html
postUpdateInstituicaoR aid = do
           ((result, _), _) <- runFormPost formInstituicao
           case result of 
               FormSuccess instituicao -> (runDB $ Database.Persist.Postgresql.replace aid instituicao) >>= \eiid -> redirect (GerenciarInstituicaoR)
               _ -> redirect ErroR
               


-- validação update de genero
postUpdateGeneroR :: GeneroId -> Handler Html
postUpdateGeneroR aid = do
           ((result, _), _) <- runFormPost formGenero
           case result of 
               FormSuccess genero -> (runDB $ Database.Persist.Postgresql.replace aid genero) >>= \eiid -> redirect (GerenciarGeneroR)
               _ -> redirect ErroR
               
               
               

-- BUSCA PRINCIPAL
postBooksR ::  Handler Html
postBooksR = do 
        ((FormSuccess result, _), _) <- runFormPost formBusca
        books <- runDB $ selectList [Filter LivroTitulo (Left $ Data.Text.concat["%",result,"%"])(BackendSpecificFilter "ILIKE")] []
        nomeinst <- runDB $ mapM (\(Entity _ vlivro) -> get404 $ livroInstituicao vlivro )books --pegando o usuario da chave estrangeira do perfil "inner join"
        generolivro <- runDB $ mapM (\(Entity eid vgener) -> selectFirst [LivroGeneroLivro ==. eid ] [])books
        nomegenero <- runDB $ mapM (\(Entity _ livrogen) -> get404 $ livroGeneroGenero livrogen )(catMaybes generolivro) --pegando o usuario da chave estrangeira do perfil "inner join"
        idNomes <- return $ Prelude.zip3 books nomeinst nomegenero 
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/livro.hamlet")
            


----------------------- GETS


-- CADASTRO DO USUÁRIO
getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/cadastrouser.hamlet")
           


-- PAGINA INICIAL DO USUÁRIO
getPerfilR :: UsuarioId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
      addStylesheet $ StaticR style_css
      toWidget $ $(whamletFile "templates/perfil.hamlet")
      
      

-- PÁGINA COM TODOS OS USUÁRIOS
getUsuariosR :: Handler Html
getUsuariosR = do
        listaUsuarios <- runDB $ selectList [] []
        nomes <- runDB $ mapM (\(Entity uid _) -> selectFirst [PerfilUsuario ==. uid] [])listaUsuarios --pegando o usuario da chave estrangeira do perfil "inner join"
        idNomes <- return $ Prelude.zip listaUsuarios (catMaybes nomes)
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listusers.hamlet")
            
            
            
-- EXCLUIR USUÁRIO
getExcluirUsuarioR :: UsuarioId -> Handler Html
getExcluirUsuarioR uid = do
        runDB $ get404 uid
        runDB $ deleteCascade $ uid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect UsuariosR
        
        
        
-- EXCLUIR LIVRO
getExcluirLivroR :: LivroId -> Handler Html
getExcluirLivroR aid = do
        runDB $ get404 aid
        runDB $ delete $ aid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect GerenciarBooksR
        
        
-- EXCLUIR INSTITUICAO
getExcluirInstituicaoR :: InstituicaoId -> Handler Html
getExcluirInstituicaoR aid = do
        runDB $ get404 aid
        runDB $ delete $ aid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect GerenciarInstituicaoR
        

-- EXCLUIR GENERO
getExcluirGeneroR :: GeneroId -> Handler Html
getExcluirGeneroR aid = do
        runDB $ get404 aid
        runDB $ delete $ aid
        setMessage $ [shamlet| Registro excluído com sucesso! |]
        redirect GerenciarGeneroR
        
        
        
-- PÁGINA COM TODOS OS LIVROS ADMIN E USUARIO
getGerenciarBooksR :: Handler Html
getGerenciarBooksR = do
        listaLivro <- runDB $ selectList [] [Asc LivroTitulo]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listlivros.hamlet")
            
            
-- PÁGINA COM TODOS AS INSTITUICOES
getGerenciarInstituicaoR :: Handler Html
getGerenciarInstituicaoR = do
        listaInstituicao <- runDB $ selectList [] [Asc InstituicaoNome]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listinstituicao.hamlet")
            
            

-- PÁGINA COM TODOS OS GENEROS
getGerenciarGeneroR :: Handler Html
getGerenciarGeneroR = do
        listaGenero <- runDB $ selectList [] [Asc GeneroNome]
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/listgenero.hamlet")

            
           
            
-- CADASTRO DO LIVRO ADMIN E USUARIO
getCadastroR ::  Handler Html
getCadastroR = do
           (widget, enctype) <- generateFormPost formLivro
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/cadastrolivro.hamlet")



-- CADASTRO DAS INSTITUICOES ADMIN
getCadastroInR ::  Handler Html
getCadastroInR = do
           (widget, enctype) <- generateFormPost formInstituicao
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/cadastroinstituicao.hamlet")
               
               
-- CADASTRO DOS GENEROS ADMIN
getCadastroGenR ::  Handler Html
getCadastroGenR = do
           (widget, enctype) <- generateFormPost formGenero
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/cadastrogenero.hamlet")


               
-- UPDATE DO LIVRO ADMIN E USUARIO
getUpdateLivroR :: LivroId -> Handler Html
getUpdateLivroR aid = do
           (widget, enctype) <- generateFormPost formLivro
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/atualizar.hamlet")  
               
               

-- UPDATE DA INSTITUICAO ADMIN 
getUpdateInstituicaoR :: InstituicaoId -> Handler Html
getUpdateInstituicaoR aid = do
           (widget, enctype) <- generateFormPost formInstituicao
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/atualizarinstituicao.hamlet")   
               
               
              
-- UPDATE DO GENERO ADMIN 
getUpdateGeneroR :: GeneroId -> Handler Html
getUpdateGeneroR aid = do
           (widget, enctype) <- generateFormPost formGenero
           defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/atualizargenero.hamlet")  
               
               
               
               
-- PERFIL DO livro ADMIN E USUARIO (cadastro com sucesso!)
getLivroR :: LivroId -> Handler Html
getLivroR aid = do
        (livro) <- runDB $ get404 aid
        inst <- runDB $ get404 $ livroInstituicao livro
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/perfilivro.hamlet")
            

-- PERFIL Da instituicao ADMIN  (cadastro com sucesso!)
getInstituicaoR :: InstituicaoId -> Handler Html
getInstituicaoR aid = do
        (instituicao) <- runDB $ get404 aid
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/perfinstituicao.hamlet")
            
            
-- PERFIL do genero ADMIN  (cadastro com sucesso!)
getGeneroR :: GeneroId -> Handler Html
getGeneroR aid = do
        (genero) <- runDB $ get404 aid
        defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget $ $(whamletFile "templates/perfigenero.hamlet")
            
            
        


-- PÁGINA INICIAL (ANTES DO LOGIN)           BUSCA 
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    (widget, enctype) <- handlerToWidget $ generateFormPost formBusca
    addStylesheet $ StaticR style_css
    toWidget $ $(whamletFile "templates/home.hamlet")
               
               
               
-- PÁGINA INICIAL (DEPOIS DO LOGIN) USUARIO
getInicioR :: Handler Html
getInicioR = defaultLayout $ do
               addStylesheet $ StaticR style_css
               toWidget $ $(whamletFile "templates/inicio.hamlet")
               
               


-- PÁGINA INICIAL DO ADMIN
getAdminR :: Handler Html
getAdminR = defaultLayout $ do
           addStylesheet $ StaticR style_css
           toWidget $ $(whamletFile "templates/admin.hamlet")
           
           

-- PÁGINA DE LOGIN (FORMULÁRIO)
getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ do
           addStylesheet $ StaticR style_css
           toWidget $ $(whamletFile "templates/login.hamlet")
           
           

-- PÁGINA DE ERRO
getErroR :: Handler Html
getErroR = do
     defaultLayout $ do
     addStylesheet $ StaticR style_css
     toWidget $ $(whamletFile "templates/erro.hamlet")




-- MENSAGEM AO FAZER LOGOUT
getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout $ do
     addStylesheet $ StaticR style_css
     toWidget $ $(whamletFile "templates/logout.hamlet")
    

