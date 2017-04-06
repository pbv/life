{- 
   -----------------------------------------------------------------
   O jogo da vida de J.H.Conway
   Pedro Vasconcelos, 2010, 2011. 
   -----------------------------------------------------------------
   Baseado na solução de G.Hutton do livro
   "Programming in Haskell", Cambridge University Press, 2007.
-}   
module Life where
import Data.List (nub)

-- definições de tipos da dados
-- uma posição na grelha (coluna, linha)
type Pos = (Int,Int)

--  lista de posições das células
type Cells  =  [Pos]

-- verificar se uma posição tem ou não uma célula
isAlive  :: Cells -> Pos -> Bool
isAlive g p = elem p g
 
isEmpty :: Cells -> Pos -> Bool
isEmpty g p =  not (isAlive g p)


-- os oito vizinhos duma posição
neighbs :: Pos -> [Pos]
neighbs (x,y)  =  [(x-1,y-1), (x,y-1),
                   (x+1,y-1), (x-1,y),
                   (x+1,y)  , (x-1,y+1),
                   (x,y+1)  , (x+1,y+1)] 

-- garantir que uma posição fica dentro da grelha
-- edgeWrap :: Pos -> Pos
-- edgeWrap (x,y) =  (x`mod`width, y`mod`height)

-- número de vizinhos vivos duma posição
liveneighbs :: Cells -> Pos -> Int
liveneighbs g  
    = length . filter (isAlive g) . neighbs

-- as células que sobrevivem nesta geração
survivors :: Cells -> [Pos]
survivors g  
    =  [p | p <- g, elem (liveneighbs g p) [2,3]]

-- as células que nascem nesta geração
births :: Cells -> [Pos]
births g 
    =  [p | p <- nub (concat (map neighbs g)),
                 isEmpty g p &&
                 liveneighbs g p == 3]

-- a próxima geração
nextgen :: Cells -> Cells
nextgen g =  survivors g ++ births g


-- select a group of connected cells
-- breadth-first search trough the neighbours
connected :: Pos -> Cells -> Cells
connected loc cells = bfs [loc] []
  where
    bfs [] viz = viz
    bfs (loc:locs) viz 
      | loc`elem`viz   = bfs locs viz
      | loc`elem`cells = bfs (neighbs loc ++ locs) (loc:viz)
      | otherwise      = bfs locs viz


