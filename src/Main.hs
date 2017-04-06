{- 
   -----------------------------------------------------------------
   O jogo da vida de J.H.Conway
   Pedro Vasconcelos, 2010, 2011. 
   -----------------------------------------------------------------
   Baseado na solução de G.Hutton do livro
   "Programming in Haskell", Cambridge University Press, 2007.
   Visualização e interface gráfica usando wxWidgets
-}   

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Main where

import            Graphics.UI.WX 
import            System.Process
import            System.Random
import            Control.Concurrent
import            Control.Exception (SomeException, IOException, catch)
import            Data.List ((\\), union, nub, delete, partition, insertBy)  
import            Life
import qualified StateVar as StateVar


-- limite interior da grelha 
-- onde o jogador pode colocar células
width, height :: Int
width  = 8
height = 8

-- número de células aleatorias
numCells :: Int
numCells = 50

-- número máximo de gerações
timeToLive :: Int
timeToLive = 100

-- diâmetro duma célula (pixels)
diameter :: Int
diameter = 20

-- verificar se uma posição está nos limites interiores
inner :: Pos -> Bool
inner (x,y) = x+width>=0 && x<width && y+height>=0 && y<height

-- lista de posições de alvos
type Targets = [Pos]

-- the internal game state
data State = State { ngen :: Int,             -- generation count
                     score :: Int,            -- current score
                     running :: Bool,         -- stop/go
                     cells :: Cells,          -- current cells
                     targets :: Targets,      -- uncaptured targets
                     dragging :: Bool,         -- are we draging selection?
                     dragPos :: Pos,           -- last drag location
                     dragCells :: Cells,       -- selected cells
                     -- initial configuration
                     initialCells :: Cells, 
                     initialTargets :: Targets
                   }

-- initial state
initialState :: Cells -> Targets -> State
initialState cs ts = State { ngen=0 
                           , score=0 
                           , running=False 
                           , cells=cs 
                           , targets=ts 
                           , dragging= False 
                           , dragPos=(0,0) 
                           , dragCells=[] 
                           , initialCells=cs 
                           , initialTargets=ts 
                           }
 
-- update the game state
nextState :: State -> State
nextState s = s { ngen = 1+ngen s, 
                  cells = cells', 
                  targets = targets',
                  score = score s + pts 
                }
    where cells' = nextgen (cells s)
          (captures,targets') = partition (`elem`cells') (targets s)
          n = timeToLive - ngen s
          pts = n*(length captures) 

-- end game conditions
endGame :: State -> Bool
endGame s = running s && 
            (ngen s == timeToLive || null (cells s) || null (targets s))

-- highscore table: list of (name, score)
type ScoreTable = [(String,Int)]   

-- number of highscores to keep 
numHighScores :: Int
numHighScores = 10

-- add a new highscore 
newHighScore ::  String -> Int -> ScoreTable -> ScoreTable
newHighScore name score table
    = take numHighScores $ 
      reverse $ insertBy cmp2nd (name,score) $ reverse table
    where cmp2nd (_,s1) (_,s2) = compare s1 s2

-- empty highscore table
emptyScores :: ScoreTable
emptyScores = [] 

----------------------------------------------------------
-- código de visualização 
----------------------------------------------------------

main :: IO ()
main = start life

life = do scores <- readScores   -- intialize state variables
          targetLocs <- randomTargets 
          vscores<- StateVar.new scores
          vstate <- StateVar.new (initialState [] targetLocs)
          -- initialize GUI elements
          f <- frame [text := "Jogo da Vida", resizeable := True]
          sf <- statusField [] 
          gr <- panel f []   -- grelha
          bs <- panel f []   -- botões
          b1 <- bitmapButton bs [picture := pathClear,
                                 tooltip := "Limpar a grelha"]
          b2 <- bitmapButton bs [picture := pathDocnew,
                                 tooltip := "Colocar células aleatoriamente"]
          b3 <- bitmapButton bs [picture := pathUndo,
                                  tooltip := "Recomeçar"]
          b4 <- bitmapButton bs [picture := pathPlay,
                                 tooltip := "Avançar/Parar"]
          b5 <- bitmapButton bs [picture := pathRewind,
                                 tooltip := "Mais lento"]
          b6 <- bitmapButton bs [picture := pathForward,
                                 tooltip := "Mais rápido"]
          
          st1 <- staticText bs [font:= fontFixed, fontSize:=16]
          st2 <- staticText bs [font:= fontFixed, fontSize:=16, textColor := red]
          -- barra de progresso (gerações)
          hg <- hgauge bs 100 [tooltip := "Progresso (gerações)"] 

          hs <- listCtrl bs [columns := [("Nome",AlignRight,100), 
                                         ("Pontuação",AlignLeft,100)]]
          -- set callback for highscore updates
          StateVar.watch vscores (fill_scores hs)
          -- set callback for state updates
          StateVar.watch vstate $ \s -> 
              do { update_status b1 b2 b3 b4 b5 b6 hg sf st1 st2 s
                 ; repaint gr
                 }
                                    
          let clean_cells = StateVar.set vstate (initialState [] targetLocs)

          let random_cells = do { locs <- randomCells numCells
                                ; StateVar.set vstate (initialState locs targetLocs)
                                }

          let try_again = StateVar.update vstate $ \s -> 
                               initialState (initialCells s) (initialTargets s)

          let start_stop = StateVar.update vstate $ \s -> 
                                 if ngen s==0 then
                                   s { running = True, 
                                       initialCells = cells s, 
                                       initialTargets = targets s } 
                                   else s { running = not (running s) }

          t <- timer f [interval := 200, 
                        on command := update_gen vstate vscores f]
          
          set b1 [on command := clean_cells]
          set b2 [on command := random_cells]
          set b3 [on command := try_again]
          set b4 [on command := start_stop]
          set b5 [on command := set t [interval :~ (\t -> min 800 (2*t))]]
          set b6 [on command := set t [interval :~ (\t -> max 100 (t`div`2))]]
          
          -- cell grid event handlers and such
          set gr [ bgcolor := white
                 , layout := fill $ expand $ space (4*width*diameter) (4*height*diameter)
                 , on click := \pt -> windowPos gr pt >>= mouseClick vstate 
                 , on clickRight := \pt -> windowPos gr pt >>= mouseClickRight vstate
                 , on drag := \pt -> windowPos gr pt >>= mouseDrag vstate
                 , on unclick := \pt -> mouseUnclick vstate 
                 , on paint := \dc rect -> do { s<-StateVar.get vstate 
                                              ; redraw_grid s dc rect
                                              } 
                 ]

          -- menus
          game <- menuPane       [ text := "&Jogo" ] 
          clear <- menuItem game [ text := "Limpar a grelha",
                                   on command := clean_cells
                                 ]
          rand  <- menuItem game  [ text := "Colocar células aleatóriamente" 
                                  , on command := random_cells
                                  ] 
          start <- menuItem game [ text := "Iniciar o jogo!", 
                                   on command := start_stop ]          
          menuLine game 
          quit <- menuQuit game [ text := "&Terminar\tCtrl+Q"
                                , help := "Sair deste programa"
                                , on command :=  close f 
                                ]
          ajuda <- menuHelp  []
          info <- menuItem ajuda [text := "Ajuda", 
                                  help := "Informação sobre o jogo da Vida",
                                  on command := info_help]
          about <- menuAbout ajuda [help := "Acerca deste programa",
                                    on command := info_about f]

          -- toplevel frame layout and such
          set f [ statusBar := [sf]
                , menuBar := [game,ajuda]
                , layout := row 0 [margin 5 $ container bs $ 
                                   column 0 [margin 5 $ row 0 [boxed "Gerações" $ hfill (widget st1),
                                                               boxed "Pontuação" $ hfill (widget st2)],
                                             hfill (widget hg),
                                             margin 5 $ 
                                             row 5 $ map (minsize $ sz 40 40) 
                                                     [widget b3,
                                                      widget b5,
                                                      widget b4,
                                                      widget b6,
                                                      widget b1,
                                                      widget b2],
                                             glue,
                                             margin 5 $ boxed "Melhores pontuações" $ 
                                             fill $ expand (widget hs)],
                                   margin 5 $ fill $ expand (widget gr)                                
                                  ]
                , on paint := \dc rect -> repaint gr
                ]
          return ()

info_about w 
    = infoDialog w "Acerca deste programa" $
      init $
      unlines ["Copyright 2011-2017 Pedro Vasconcelos <pbv@dcc.fc.up.pt>",
               "",
               "Este programa é distribuido com uma licença livre.",
               "Por favor veja o ficheiro LICENSE: http://github.com/pbv/life"
              ]

info_help = do forkOS (callProcess "evince" ["life.pdf"] >> return ())
               return ()
{-
info_help = 
  do forkOS $ catch (system "evince life.pdf" >> return ()) (\_ -> return ())
     return ()
-}

-- actualizar estado de botões, etc.
update_status b1 b2 b3 b4 b5 b6 hg sf st1 st2 s 
  = do set b1 [enabled := not (running s)]
       set b2 [enabled := not (running s)]
       set b3 [enabled := ngen s>0 && not (running s)]
       set b4 [picture := play_or_pause, enabled := ngen s<timeToLive]
       set b5 [enabled := running s]
       set b6 [enabled := running s]
       set hg [selection := ngen s]
       set sf [text := if ngen s>0 then 
                           show (ngen s) ++ " gerações, " ++ show (score s) ++ " pontos."
                       else info_txt]
       set st1 [text := show (ngen s)]
       set st2 [text := show (score s)]
    where 
      info_txt = "Clique-esquerdo coloca células; clique-direito apaga; clique-e-arraste para mover células."
      play_or_pause | running s  = pathPause 
                    | otherwise  = pathPlay 

-- preencher a tabela de pontos
fill_scores w table = set w [items := t]
    where t = [[name,show score] | (name,score)<-table]
            

-- update state to the next generation
-- detects end of game and updates highscores if necessary
-- 'f' is the toplevel frame (for showing dialogs)
update_gen vstate vscores f 
    = do s <- StateVar.get vstate
         if endGame s then 
             do { StateVar.update vstate $ \s -> s { running=False } 
                ; update_highscores vscores f (score s)
                }
           else when (running s) $ StateVar.update vstate nextState 


update_highscores vscores f pts
    = do scores <- StateVar.get vscores
         when (null scores || pts > snd (last scores)) $  
              do { name <- textDialog f "Escreva o seu nome" "Nova melhor pontuação!" ""
                 ; when (name /= "") $ 
                   do { StateVar.update vscores (newHighScore name pts)
                      ; StateVar.get vscores >>= writeScores
                      }
                 }


-- convert a grid location to view coordinates
viewPos :: Rect2D Int -> Pos -> Point2 Int
viewPos view (col,row) = point ((col+w)*diameter) ((row+h)*diameter)
    where w = rectWidth view `div` (2*diameter) 
          h = rectHeight view `div` (2*diameter)


-- inverse transformation
windowPos :: Window a -> Point2 Int -> IO Pos
windowPos w pt = do { view<- get w area; return (viewPos_inv view pt) }

viewPos_inv :: Rect2D Int -> Point2 Int -> Pos
viewPos_inv view pt = (x-w, y-h)
    where x = pointX pt `div` diameter
          y = pointY pt `div` diameter
          w = (rectWidth view)`div`(2*diameter)
          h = (rectHeight view)`div`(2*diameter)




-- redesenhar a grelha
redraw_grid s dc view =  
    do drawGrid dc view                    -- desenhar a grelha
       when (ngen s==0) $ drawLimits dc view
       drawTargets dc view (targets s)     -- desenhar os alvos
       set dc [penKind:=PenSolid, penWidth:=1, penColor:=black,
               brushKind := BrushSolid, brushColor := black]
       drawCells dc view (cells s)         -- desenhar as células
       set dc [penKind:= if dragging s then PenDash DashShort else PenSolid,
               brushColor := if dragging s then grey else black]
       drawCells dc view (filter inner (dragCells s))

-- desenhar células
drawCells dc view locs 
    = sequence_ [ drawRect dc (rect pt s) [] | loc<-locs, let pt=pointMove u (viewPos view loc)]
    where s = sz (diameter-1) (diameter-1)
          u = vec 1 1


-- desenhar os alvos
drawTargets dc view locs
    = do set dc [penColor := red, penKind := PenSolid, penWidth := 2]
         sequence_ [drawTarget (viewPos view loc) | loc<-locs]
    where
      drawTarget pt =  drawBitmap dc bitmapTarget pt True []


-- desenhar a grelha e limites interiores
drawGrid dc view = 
    do set dc [penColor := grey, penKind := PenSolid, penWidth := 1]
       sequence_ [line dc (pt 0 y) (pt w y) [] | y<-[0,diameter..h]]
       sequence_ [line dc (pt x 0) (pt x h) [] | x<-[0,diameter..w]]
    where w = rectWidth view
          h = rectHeight view

-- desenhar limites interiores
drawLimits dc view
    = polyline dc (map (viewPos view) pts) 
              [penColor:= blue, penKind:= PenDash DashShort, penWidth:=2]
    where  pts = [(-width,-height), (width,-height),
                  (width,height), (-width,height), (-width,-height)] 


-- actuar um clique do rato: colocar uma célula
mouseClick vstate loc 
    = StateVar.update vstate $ \s -> 
      if ngen s==0 then select (mark s) else s
    where mark s | loc`elem`cells s = s
                 | otherwise        = s {cells=loc:cells s}
          select s = let conn = connected loc (cells s)
                     in s { cells = cells s \\ conn
                          --, dragging = True
                          , dragPos = loc
                          , dragCells = conn
                          }
{-
          flip s | loc`elem`cells s  = s {cells=delete loc (cells s)}
                 | otherwise         = s {cells=loc:cells s}
-}

mouseClickRight vstate loc
    = StateVar.update vstate $ \s -> 
             if ngen s==0 then
                 s { cells = delete loc (cells s) }
             else s


-- drag selected cells
mouseDrag vstate (x,y)
    = StateVar.update vstate $ \s -> 
             if ngen s==0 then
                 s { dragging = True
                   , dragPos = (x,y)
                   , dragCells = map (move (dragPos s)) (dragCells s) }
             else s
    where move (x',y') (u,v) = (u+x-x', v+y-y')

-- paste dragged cells
mouseUnclick  vstate
    = StateVar.update vstate $ \s -> 
             if ngen s ==0 then
                 s { dragging = False
                   , dragCells = []
                   , cells = filter inner (dragCells s) `union` cells s 
                   }
             else s



-- gerar células em posições aleatórias
randomCells :: Int -> IO [Pos]
randomCells n
  = do g <- getStdGen
       let (locs, g')= randomPositions n (-width,-height) (height-1,width-1) g
       setStdGen g'
       return locs


-- gerar alvos em posições aleatórias
randomTargets :: IO [Pos]
randomTargets 
    = do g0 <- getStdGen 
         let (locs1, g1) = randomPositions 2 (-width-4,-height-4) (-width-1,-height-1) g0
         let (locs2, g2) = randomPositions 2 (width,height) (width+4,height+4) g1
         let (locs3, g3) = randomPositions 2 (-width-4,height) (-width-1,height+4) g2
         let (locs4, g4) = randomPositions 2 (width,-height-4) (width+4,-height-1) g3
         setStdGen g4
         return (locs1++ locs2 ++locs3 ++locs4)


-- gerar uma lista de posições aleatórias sem repetições
randomPositions :: RandomGen g => Int -> Pos -> Pos -> g -> ([Pos], g)
randomPositions n xy0 xy1 g = worker n g []
    where worker n g locs
              | n>0 = let (loc,g')= randomPos xy0 xy1 g
                      in if loc`elem`locs 
                         then worker n g' locs
                         else worker (n-1) g' (loc:locs)
              | otherwise = (locs, g)

randomPos :: RandomGen g => Pos -> Pos -> g -> (Pos, g)
randomPos (x0,y0) (x1,y1) g = ((x,y), g'')
    where (x,g') = randomR (x0,x1) g
          (y,g'')= randomR (y0,y1) g'

-- read & write high score tables
readScores :: IO ScoreTable
readScores 
  = catch (do {txt<-readFile scoreFilepath; return (read txt)}) 
          (\(_ :: IOException) -> do { putStrLn "WARNING: couldn't read score file"
                    ; return emptyScores })

writeScores :: ScoreTable -> IO ()
writeScores table = catch (writeFile scoreFilepath (show table)) 
                    (\(_::IOException) -> putStrLn "WARNING: couldn't write score file")
                    
scoreFilepath :: FilePath
scoreFilepath = "highscores.txt"

                    

                    
                    
                
-- bitmap file paths 
bitmapDir = "/usr/share/icons/gnome/24x24/actions/"

pathPlay =  bitmapDir ++ "gtk-media-play-ltr.png"
pathPause =  bitmapDir ++ "gtk-media-pause.png"
pathRewind = bitmapDir ++ "gtk-media-rewind-ltr.png"
pathForward = bitmapDir ++ "gtk-media-rewind-rtl.png"
pathUndo = bitmapDir ++ "edit-undo.png"
pathClear = bitmapDir ++ "edit-clear.png"
pathDocnew = bitmapDir ++ "document-new.png"
pathHelpFaq = bitmapDir ++ "help-faq.png"

bitmapTarget = bitmap "/usr/share/icons/gnome/24x24/emblems/emblem-favorite.png"

