-- State variables for wxHaskell 
-- Encapsulate callback functions for state changes
-- pbv, 2011
module StateVar
    ( StateVar,
      new, get, set, update, watch
    )  where

import Graphics.UI.WX hiding (get,set,update)

-- a state variable is pair of mutable value and (mutable) callback
data StateVar a = StateVar (Var a) (Var (a -> IO ()))

-- make a new state var with given value and null callback
new :: a -> IO (StateVar a)
new x = do var <- varCreate x
           callback <- varCreate (\_ -> return ())
           return (StateVar var callback)

-- assign to a state var
set :: StateVar a -> a -> IO ()
set (StateVar var callback) x
    = do varSet var x 
         cb <- varGet callback 
         cb x

-- fetch the value of a state var
get :: StateVar a -> IO a
get (StateVar var _) = varGet var

-- update a state var using a pure function
update :: StateVar a -> (a -> a) -> IO ()
update (StateVar var callback) f
    = do varUpdate var f
         x <- varGet var
         cb <- varGet callback
         cb x

-- modify the callback for a state var
-- executes the callback with current value
watch :: StateVar a -> (a -> IO ()) -> IO ()
watch (StateVar var callback) cb 
  = do varSet callback cb
       x <- varGet var
       cb x

       


