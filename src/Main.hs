{-# OPTIONS -Wall #-}

module Main where


import Data.Map
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.List
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

data ObjectValue
    = IntValue Int
    | StringValue String
    deriving (Show, Ord, Eq)

type ValueMap = Map ObjectValue ObjectIdentifier

data RuntimeState = RuntimeState (ObjectMap, ReferenceMap, ObjectIdentifier, ValueMap) deriving Show

type Runtime = State RuntimeState


putObject :: Maybe ObjectIdentifier -> [ObjectIdentifier] -> ReferenceMap -> Runtime ObjectIdentifier
putObject parentId mixins properties = do
    RuntimeState (runtime, referenceMap, Identifier id, valueMap) <- get
    
    let id' = id + 1
    let mixins' = case parentId of
                    Nothing -> mixins
                    Just parentMixin -> [parentMixin] ++ mixins
    let objectId = Identifier id'
    let object = Object { objectIdentifier = objectId
                        , objectMixins = mixins'
                        , objectSlots = properties
                        }
    let runtime' = insert objectId object runtime

    put $ RuntimeState (runtime', referenceMap, objectId, valueMap)

    return objectId


getObject :: ObjectIdentifier -> Runtime Object
getObject id = do
    RuntimeState (runtime, _, _, _) <- get

    if member id runtime then
        lookupObjectId id
    else
        lookupVoid


lookupObjectId :: ObjectIdentifier -> Runtime Object
lookupObjectId id = do
    RuntimeState (runtime, _, _, _) <- get

    case Data.Map.lookup id runtime of
        Just object -> return object
        Nothing -> undefined


lookupReference :: String -> Runtime Object
lookupReference reference = do
    RuntimeState (_, referenceMap, _, _) <- get

    case Data.Map.lookup reference referenceMap of
        Just id -> lookupObjectId id
        Nothing -> do
            let builtInReferences =
                    [ "Object"
                    , "Void"
                    , "Class"
                    , "String"                    
                    , "Exception"
                    , "Variable" ]

            if elem reference builtInReferences
                then undefined
                else lookupVoid


lookupObject :: Runtime Object
lookupObject = lookupReference "Object"


lookupVoid :: Runtime Object
lookupVoid = lookupReference "Void"


lookupClass :: Runtime Object
lookupClass = lookupReference "Class"


lookupString :: Runtime Object
lookupString = lookupReference "String"


lookupException :: Runtime Object
lookupException = lookupReference "Exception"


lookupVariable :: Runtime Object
lookupVariable = lookupReference "Variable"


putReference :: String -> ObjectIdentifier -> Runtime ()
putReference name id = do
    RuntimeState (runtime, referenceMap, id', valueMap) <- get
    let referenceMap' = insert name id referenceMap
    
    put(RuntimeState (runtime, referenceMap', id', valueMap))


putValue :: ObjectValue -> ObjectIdentifier -> Runtime ()
putValue value id = do
    RuntimeState (runtime, referenceMap, id', valueMap) <- get
    let valueMap' = insert value id valueMap
    
    put(RuntimeState (runtime, referenceMap, id', valueMap'))

    

insertObject :: Maybe ObjectIdentifier -> [ObjectIdentifier] -> ReferenceMap -> Maybe String -> Runtime ObjectIdentifier
insertObject parentId mixins properties referenceName = do
    id <- putObject parentId mixins properties

    case referenceName of
        Nothing -> return ()
        Just name ->
            putReference name id

    return id


modifyObject :: ObjectIdentifier -> [ObjectIdentifier] -> ReferenceMap -> Runtime ()
modifyObject id mixins properties = do
    RuntimeState (runtime, referenceMap, id', valueMap) <- get
    let object = Object { objectIdentifier = id
                        , objectMixins = mixins
                        , objectSlots = properties
                        }
    let runtime' = insert id object runtime
    put(RuntimeState (runtime', referenceMap, id', valueMap))


addSlot :: ObjectIdentifier -> String -> ObjectIdentifier -> Runtime ()
addSlot id name slotId = do
    RuntimeState (runtime, _, _, _) <- get
    object <- getObject id

    let mixins = objectMixins object
    let properties = insert name slotId $ objectSlots object

    modifyObject id mixins properties
    

evaluateObject :: Object -> [Object] -> Runtime Object
evaluateObject object arguments = do
    void <- lookupVoid

    if objectIdentifier object == objectIdentifier void
        then lookupException
        else return object


createObject :: Object -> [Object] -> Runtime Object
createObject base arguments = do
    base' <- evaluateObject base []
    let parentId = Just $ objectIdentifier base'
    
    id <- insertObject parentId [] empty Nothing
    getObject id


setupRuntime :: Runtime ()
setupRuntime = do
    object <- insertObject Nothing [] empty (Just "Object")
    void <- insertObject (Just object) [] empty (Just "Void")
    klass <- insertObject (Just object) [] empty (Just "Class")
    string <- insertObject (Just klass) [] empty (Just "String")
    exception <- insertObject (Just klass) [] empty (Just "Exception")
    variable <- insertObject (Just klass) [] empty (Just "Variable")

    return ()


doTheThings :: Runtime ()
doTheThings = do    
    setupRuntime

    klass <- lookupClass
    string <- lookupString
    variable <- lookupVariable

    employee <- createObject klass []
    putReference "Employee" $ objectIdentifier employee 

    bobObject <- createObject employee []
    bobVariableName <- createObject string []
    putValue (StringValue "bob") $ objectIdentifier bobVariableName
    
    bob <- createObject variable []
    addSlot (objectIdentifier bob) "name" (objectIdentifier bobVariableName)
    addSlot (objectIdentifier bob) "object" (objectIdentifier bobObject)

    return ()


initialState :: RuntimeState
initialState = RuntimeState (empty, empty, Identifier 0, empty)


main :: IO ()
main =
    pPrint $ runStateT doTheThings initialState
