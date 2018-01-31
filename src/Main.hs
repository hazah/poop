module Main where


import Data.Map
import Control.Monad.Trans.State


data Object = Object { objectIdentifier :: Int
                     , objectMixins :: [Int]
                     , objectSlots :: Map String Object
                     } deriving Show

newtype Message = Message String
                       
data Runtime = Runtime (Map Int Object, Int) deriving Show

type RuntimeState = State Runtime


createObject :: Maybe Int -> [Object] -> RuntimeState Object
createObject parent arguments = do
    Runtime (runtime, id) <- get
    
    let mixins = case parent of
                    Nothing -> []
                    Just parentObject -> [parentObject]
    
    let object = Object { objectIdentifier = id + 1
                        , objectMixins = mixins
                        , objectSlots = empty
                        }

    put(Runtime (insert (id + 1) object runtime, id + 1))

    return object

extendObject :: Object -> [Object] -> RuntimeState Object
extendObject object arguments = do
    Runtime (runtime, id) <- get
    object <- createObject (Just (objectIdentifier object)) arguments

    return object

getObject :: Int -> RuntimeState (Maybe Object)
getObject id = do
    Runtime (runtime, _) <- get
    return $ Data.Map.lookup id runtime
    
sendMessage :: Message -> Object -> [Object] -> RuntimeState Object
sendMessage (Message message) reciever arguments = do
    let sendMsg = case message of 
                    "extend" -> extendObject
                    otherwise -> undefined

    object <- sendMsg reciever arguments

    return object

doTheThings :: RuntimeState ()
doTheThings = do
    
    object <- createObject Nothing []
    klass <- sendMessage (Message "extend") object []
    
    return ()


main :: IO ()
main =
    print $ runStateT doTheThings $ Runtime (empty, 0)