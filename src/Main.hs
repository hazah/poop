module Main where


import Data.Map
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import Text.Show.Pretty

newtype ObjectIdentifier = Identifier Int deriving (Show, Ord, Eq)
newtype Message = Message String

type ReferenceMap = Map String ObjectIdentifier

data Object = Object { objectIdentifier :: ObjectIdentifier
                     , objectMixins :: [ObjectIdentifier]
                     , objectSlots :: ReferenceMap
                     } deriving Show

                       
type ObjectMap = Map ObjectIdentifier Object

data RuntimeState = RuntimeState (ObjectMap, ReferenceMap, ObjectIdentifier) deriving Show

type Runtime = State RuntimeState


createObject :: Maybe ObjectIdentifier -> [Object] -> Runtime Object
createObject parent arguments = do
    RuntimeState (runtime, reference_map, Identifier id) <- get
    
    let mixins = case parent of
                    Nothing -> []
                    Just parentObject -> [parentObject]
    
    let id' = id + 1
    let object = Object { objectIdentifier = Identifier id'
                        , objectMixins = mixins
                        , objectSlots = empty
                        }

    put(RuntimeState (insert (Identifier id') object runtime, reference_map, Identifier id'))

    return object


getObject :: ObjectIdentifier -> Runtime Object
getObject id = do
    RuntimeState (runtime, reference_map, _) <- get
    
    case Data.Map.lookup id runtime of
        Nothing -> do
            Just id' <- return $ Data.Map.lookup "Void" reference_map
            Just void <- return $ Data.Map.lookup id' runtime

            return void
        
        Just object ->
            return object


addReference :: String -> ObjectIdentifier -> Runtime ()
addReference name id = do
    RuntimeState (runtime, reference_map, id') <- get
    put(RuntimeState (runtime, insert name id reference_map, id'))
    return ()
    

extendObject :: Object -> [Object] -> Runtime Object
extendObject base arguments =
    createObject (Just $ objectIdentifier base) arguments


evaluateMethod :: Object -> [Object] -> Runtime Object
evaluateMethod object arguments = do
    return object


evaluateVariable :: Object -> Runtime Object
evaluateVariable object = do
    return object


evaluateObject :: Object -> [Object] -> Runtime Object
evaluateObject object arguments = do
    RuntimeState (runtime, reference_map, _) <- get
    let mixins = objectMixins object

    case Data.Map.lookup "Method" reference_map of
        Nothing -> undefined
        Just methodId -> if elem methodId mixins
            then evaluateMethod object arguments
            else case Data.Map.lookup "Variable" reference_map of
                Nothing -> undefined
                Just variableId -> if elem variableId mixins
                    then evaluateVariable object
                    else return object


addMixins :: Object -> [Object] -> Runtime Object
addMixins object mixins = do
    let newMixins = Prelude.map objectIdentifier mixins ++ objectMixins object

    result <- modifyObject object newMixins $ objectSlots object
    return result


addSlots :: Object -> [Object] -> Runtime Object
addSlots object slots = do
    return object


sendMessage :: Message -> Object -> [Object] -> Runtime Object
sendMessage (Message message) reciever arguments = do
    RuntimeState (runtime, reference_map, _) <- get

    case Data.Map.lookup "Void" reference_map of
        Nothing -> undefined
        -- Thou shall not send messages to the Void!
        Just voidId -> if voidId == objectIdentifier reciever then
                case Data.Map.lookup "VoidMessage" reference_map of
                    Nothing -> undefined
                    Just exceptionId -> do
                        exception <- getObject exceptionId
                        return exception

            else do
                let sendMsg = case message of 
                                "extend" -> extendObject
                                "addMixins" -> addMixins
                                "evaluate" -> evaluateObject
                                otherwise -> undefined

                object <- sendMsg reciever arguments

                return object


modifyObject :: Object -> [ObjectIdentifier] -> ReferenceMap -> Runtime Object
modifyObject object mixins slots = do
    let modifiedObject = Object { objectIdentifier = objectIdentifier object
                        , objectMixins = mixins
                        , objectSlots = slots
                        }
    
    RuntimeState (runtime, reference_map, id) <- get
    put(RuntimeState (insert (objectIdentifier object) modifiedObject runtime, reference_map, id))

    return modifiedObject
    


doTheThings :: Runtime ()
doTheThings = do    
    object <- createObject Nothing []
    addReference "Object" $ objectIdentifier object

    klass <- extendObject object []
    addReference "Class" $ objectIdentifier klass
    
    void <- extendObject object []
    addReference "Void" $ objectIdentifier void

    exception <- extendObject object []
    addReference "Exception" $ objectIdentifier exception

    voidMessage <- extendObject exception []
    addReference "VoidMessage" $ objectIdentifier voidMessage

    method <- extendObject object []
    addReference "Method" $ objectIdentifier method

    variable <- extendObject object []
    addReference "Variable" $ objectIdentifier variable

    block <- extendObject object []
    addReference "Block" $ objectIdentifier block

    sendMessage (Message "addMixins") object [klass]

    return ()


initialState :: RuntimeState
initialState = RuntimeState (empty, empty, Identifier 0)


main :: IO ()
main =
    pPrint $ runStateT doTheThings initialState