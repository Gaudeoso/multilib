{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
module Import where

import Yesod
import Yesod.Static

--mkYesod "HaskellLivro" 

pRoutes = [parseRoutes|
/ HomeR GET
/login LoginR GET POST                                          -- página de login
/inicio InicioR GET                                             -- página inicial
/admin AdminR GET                                               -- página do admin
/erro ErroR GET                                                 -- página de erro
/usuario UsuarioR GET POST                                      -- cadastro de usuário
/perfil/#UsuarioId PerfilR GET                                  -- perfil de usuário
/excluir_usuario/#UsuarioId ExcluirUsuarioR GET                  -- excluir usuário
/excluir_livro/#LivroId ExcluirLivroR GET                       -- excluir Livro
/excluir_instituicao/#InstituicaoId ExcluirInstituicaoR GET    -- excluir instituicao
/excluir_genero/#GeneroId ExcluirGeneroR GET    -- excluir instituicao
/cadastro CadastroR GET POST                                    -- cadastro de livros
/cadastro_genero CadastroGenR GET POST                                 -- cadastro de generos
/cadastro_instituicao CadastroInR GET POST                      -- cadastro de instiruicao
/update/#LivroId UpdateLivroR GET POST                          -- update do livro
/update_instituicao/#InstituicaoId UpdateInstituicaoR GET POST              -- update da instituicao
/update_genero/#GeneroId UpdateGeneroR GET POST                        -- update do genero
/livro/#LivroId LivroR GET                                      -- perfil do livro 
/instituicao/#InstituicaoId InstituicaoR GET                          -- perfil da instituicao 
/genero/#GeneroId GeneroR GET                                   -- perfil do genero 
/livros BooksR POST                                             -- lista de todos os Books (users) -- BUSCA PRINCIAL
/gerenciar_livros GerenciarBooksR GET                           -- lista de todos os Books (admin)
/gerenciar_instituicao GerenciarInstituicaoR GET                 -- lista de todos as instituicoes (admin)
/gerenciar_genero GerenciarGeneroR GET                          -- lista de todos os generos (admin)
/usuarios UsuariosR GET                                         -- lista de usuários
/logout LogoutR GET

/static StaticR Static getStatic


|]


---------------------------------------------------------------------------Duvidas
--Como fazer a busca funcionar
--Como fazer alteração de somente um campo em UpdateLivroR
--fazer os git hubs paulatinamente 
--alternar os gits 
-- arrumar erro nas paginas de user e admin (faazer voltar para a pagina certa)
-- arrumar layout (mudar)
--
--
--
------------------------------------------------------------------------------------------