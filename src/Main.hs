module Main where


import Data.Map
import Control.Monad.Trans.State

newtype ObjectIdentifier = Identifier Int deriving (Show, Ord, Eq)
newtype Message = Message String

data Object = Object { objectIdentifier :: ObjectIdentifier
                     , objectMixins :: [ObjectIdentifier]
                     , objectSlots :: Map String Object
                     } deriving Show

                       
type ObjectMap = Map ObjectIdentifier Object
type ReferenceMap = Map String Object

data Runtime = Runtime (ObjectMap, ObjectIdentifier) deriving Show

type RuntimeState = State Runtime


createObject :: Maybe ObjectIdentifier -> [Object] -> RuntimeState Object
createObject parent arguments = do
    Runtime (runtime, Identifier id) <- get
    
    let mixins = case parent of
                    Nothing -> []
                    Just parentObject -> [parentObject]
    
    let id' = id + 1
    let object = Object { objectIdentifier = Identifier id'
                        , objectMixins = mixins
                        , objectSlots = empty
                        }

    put(Runtime (insert (Identifier id') object runtime, Identifier id'))

    return object


extendObject :: Object -> [Object] -> RuntimeState Object
extendObject base arguments =
    createObject (Just $ objectIdentifier base) arguments


getObject :: ObjectIdentifier -> RuntimeState (Maybe Object)
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
    
    Runtime (runtime, _) <- get
    
    let id = objectIdentifier object
    let mixins = (objectIdentifier klass) : (objectMixins object)

    let modifiedObject = Object { objectIdentifier = id
                        , objectMixins = mixins
                        , objectSlots = objectSlots object
                        }

    put(Runtime (insert id modifiedObject runtime, id))

    return ()


main :: IO ()
main =
    print $ runStateT doTheThings $ Runtime (empty, Identifier 0)