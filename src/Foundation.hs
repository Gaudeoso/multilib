{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
module Foundation where
import Import
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Maybe
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)
import Yesod.Static
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )
    
    
data HaskellLivro = HaskellLivro {getStatic :: Static, connPool :: ConnectionPool }
                 

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Perfil 
   nome Text
   cpf Text
   cidade Text
   email Text
   endereco Text
   telefone Text
   usuario UsuarioId
   instituicao InstituicaoId
   
Livro 
   titulo Text
   autor Text
   editora Text
   instituicao InstituicaoId
   quantidade Text
   
   

Usuario 
    login Text
    senha Text
  
   
Instituicao 
    nome Text
    cnpj Text
    endereco Text
    telefone Text

Genero 
    nome Text
    UniqueNome nome

LivroGenero
    livro LivroId
    genero GeneroId
    

   
|]

staticFiles "static"

mkYesodData "HaskellLivro" pRoutes

instance Yesod HaskellLivro where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized   --login do usuario (todos)
    isAuthorized ErroR _ = return Authorized     -- erro generico (todos)
    isAuthorized HomeR _ = return Authorized     -- home (todos)
    isAuthorized UsuarioR _ = isAdmin  -- cadastro do usuario (Admin)
    isAuthorized CadastroR _ = return Authorized   -- cadastro de livros (admin)
    isAuthorized CadastroInR _ = isAdmin   -- cadastro de instiruicoes (admin)
    isAuthorized CadastroGenR _ = isAdmin   -- cadastro de generos (admin)
    isAuthorized BooksR _ = return Authorized   -- lista dos livros (todos)
    isAuthorized (InstituicaoR _) _ = isAdmin   -- lista das instiruicoes (admin)
    isAuthorized (GeneroR _) _ = isAdmin   -- lista dos generos (admin)
    isAuthorized InicioR _ = isUser -- home logado usuario  ( users)
    isAuthorized AdminR _ = isAdmin  -- home logado do admin (admin)
    isAuthorized UsuariosR _ = isAdmin -- listar usuarios (admin)
    isAuthorized _ _ = isUser
    

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser um administrador para acessar essa página"
    

instance YesodPersist HaskellLivro where
   type YesodPersistBackend HaskellLivro = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage HaskellLivro FormMessage where
    renderMessage _ _ = defaultFormMessage
