{-# OPTIONS -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Text.Show.Functions()
import Data.Map
import Data.Tuple
import Data.Dynamic
import Control.Exception
import Control.Monad.Trans.State

import Text.Show.Pretty


data RuntimeException
    = ObjectLookupFailed
    | ObjectIdentifierNotFound
    | ObjectValueNotFound
    | ObjectHasNoNameProperty
    | VariableHasNoObjectProperty
    | MethodNotImplemented
    | BuiltinMethodNotFound
    | NotAMethod
    | ClassNotFound
    deriving (Show, Typeable)

instance Exception RuntimeException


newtype ObjectIdentifier = Identifier Int deriving (Show, Ord, Eq)
newtype Message = Message String

type ReferenceMap = Map String ObjectIdentifier

data Object = Object { objectIdentifier :: ObjectIdentifier
                     , objectMixins :: [ObjectIdentifier]
                     , objectSlots :: ReferenceMap
                     } deriving Show

                       
type ObjectMap = Map ObjectIdentifier Object

newtype BuiltinMethodIdentifier = MethodIdentifier Int deriving (Show, Ord, Eq)

type BuiltinMethodMap = Map BuiltinMethodIdentifier (Object -> [Object] -> Runtime Object)

data ObjectValue
    = IntValue Int
    | StringValue String
    | ListValue [ObjectIdentifier]
    | BuiltinMethodValue BuiltinMethodIdentifier
    deriving (Show, Ord, Eq)

type ValueMap = Map ObjectValue ObjectIdentifier

data RuntimeState = RuntimeState    ( ObjectMap
                                    , ObjectIdentifier
                                    , ValueMap
                                    , BuiltinMethodMap
                                    , BuiltinMethodIdentifier) deriving Show

type Runtime = State RuntimeState


putObject :: Maybe ObjectIdentifier -> [ObjectIdentifier] -> ReferenceMap -> Runtime ObjectIdentifier
putObject parentId mixins properties = do
    RuntimeState (runtime, Identifier _id, valueMap, methodMap, methodId) <- get
    
    let _id' = _id + 1
    let mixins' = case parentId of
                    Nothing -> mixins
                    Just parentMixin -> [parentMixin] ++ mixins
    let objectId = Identifier _id'
    let object = Object { objectIdentifier = objectId
                        , objectMixins = mixins'
                        , objectSlots = properties
                        }
    let runtime' = insert objectId object runtime

    put $ RuntimeState (runtime', objectId, valueMap, methodMap, methodId)

    return objectId


lookupObjectId :: ObjectIdentifier -> Runtime Object
lookupObjectId _id = do
    RuntimeState (runtime, _, _, _, _) <- get

    case Data.Map.lookup _id runtime of
        Just object -> return object
        Nothing -> throw ObjectLookupFailed


getObject :: ObjectIdentifier -> Runtime Object
getObject _id = do
    RuntimeState (runtime, _, _, _, _) <- get

    if member _id runtime then
        lookupObjectId _id
    else
        throw ObjectIdentifierNotFound


putValue :: ObjectValue -> ObjectIdentifier -> Runtime ()
putValue value _id = do
    RuntimeState (runtime, _id', valueMap, methodMap, methodId) <- get
    let valueMap' = insert value _id valueMap
    
    put(RuntimeState (runtime, _id', valueMap', methodMap, methodId))

    
putBuiltinMethod :: (Object -> [Object] -> Runtime Object) -> Runtime BuiltinMethodIdentifier
putBuiltinMethod builtinMethod = do
    RuntimeState (runtime, _id, valueMap, methodMap, MethodIdentifier methodId) <- get
    let methodId' = methodId + 1
    let methodMap' = insert (MethodIdentifier methodId') builtinMethod methodMap
    
    put(RuntimeState (runtime, _id, valueMap, methodMap', MethodIdentifier methodId'))

    return $ MethodIdentifier methodId'

-- lookupValueObject :: ObjectValue -> Runtime Object
-- lookupValueObject value = do
--     RuntimeState (_, _, valueMap) <- get

--     case Data.Map.lookup value valueMap of
--         Just _id -> lookupObjectId _id
--         Nothing -> do
--             valueClass <- case value of
--                 IntValue _ -> undefined
--                 StringValue _ -> undefined

--             object <- createObject valueClass []
--             putValue value $ objectIdentifier object

--             return object


lookupObjectValue :: Object -> Runtime ObjectValue
lookupObjectValue object = do
    RuntimeState (_, _, valueMap, _, _) <- get

    let objectMap = fromList $ Prelude.map swap (toList valueMap)
    let _id = objectIdentifier object
    case Data.Map.lookup _id objectMap of
        Nothing -> throw ObjectValueNotFound
        Just value -> return value


modifyObject :: ObjectIdentifier -> [ObjectIdentifier] -> ReferenceMap -> Runtime ()
modifyObject _id mixins properties = do
    RuntimeState (runtime, _id', valueMap, methodMap, methodId) <- get
    let object = Object { objectIdentifier = _id
                        , objectMixins = mixins
                        , objectSlots = properties
                        }
    let runtime' = insert _id object runtime
    put(RuntimeState (runtime', _id', valueMap, methodMap, methodId))


putSlot :: ObjectIdentifier -> String -> ObjectIdentifier -> Runtime ()
putSlot _id name slotId = do
    object <- getObject _id

    let mixins = objectMixins object
    let properties = insert name slotId $ objectSlots object

    modifyObject _id mixins properties


-- createVariable :: String -> Object -> Runtime Object
-- createVariable name object = do
--     variable <- lookupVariable
--     variableName <- lookupValueObject $ StringValue name
    
--     objectVariable <- createObject variable []
--     let _id = objectIdentifier object
--     let variableId = objectIdentifier objectVariable
--     let nameId = objectIdentifier variableName

--     addSlot variableId "name" nameId
--     addSlot variableId "object" _id

--     putReference name variableId

--     return objectVariable


slotFromVariable :: Object -> Runtime (String, ObjectIdentifier)
slotFromVariable variable = do
    let slots = objectSlots variable
    case Data.Map.lookup "name" slots of
        Nothing -> throw ObjectHasNoNameProperty
        Just nameId -> do
            nameString <- getObject nameId
            nameValue <- lookupObjectValue nameString
            let name = case nameValue of
                    IntValue int -> show int
                    StringValue string -> string
                    ListValue _ -> undefined
                    BuiltinMethodValue _ -> undefined

            return (name, objectIdentifier variable)


slotsFromVariables :: [Object] -> ReferenceMap -> Runtime ReferenceMap
slotsFromVariables [] slots = return slots
slotsFromVariables (variable:variables) slots = do
    (name, _id) <- slotFromVariable variable
    slotsFromVariables variables (insert name _id slots)


slotsFromObjects :: [ObjectIdentifier] -> [(String, ObjectIdentifier)] -> Runtime [(String, ObjectIdentifier)]
slotsFromObjects [] slots = return slots
slotsFromObjects (objectId:objects) slots = do
    object <- getObject objectId
    slots' <- slotsFromObject object
    rest <- slotsFromObjects objects slots'
    return $ slots ++ rest


slotsFromObject :: Object -> Runtime [(String, ObjectIdentifier)]
slotsFromObject object = do
    let parents = objectMixins object
    parentSlots <- slotsFromObjects parents []

    let slots = toList $ objectSlots object
    return $ slots ++ parentSlots


slotsFromList :: [ObjectIdentifier] -> [(String, ObjectIdentifier)] -> Runtime [(String, ObjectIdentifier)]
slotsFromList [] slots = return slots
slotsFromList (_id:ids) slots = do
    object <- getObject _id
    slots' <- slotsFromObject object
    rest <- slotsFromList ids slots'
    return $ slots ++ rest


messageObject :: String -> Object -> [Object] -> Runtime Object
messageObject message object arguments = do
    RuntimeState (_, _, _, methodMap, _) <- get
    
    receiver <- if (message == "evaluate")
            then return object
            else messageObject "evaluate" object []

    slotList <- slotsFromObject receiver
    block <- case Data.Map.lookup message $ fromList slotList of
            Nothing -> throw MethodNotImplemented
            Just _id -> do
                variable <- getObject _id
                messageObject "evaluate" variable []

    objectValue <- lookupObjectValue block

    case objectValue of
        IntValue _ -> throw NotAMethod
        StringValue _ -> throw NotAMethod
        ListValue _ -> throw NotAMethod
        BuiltinMethodValue methodId ->
            case Data.Map.lookup methodId methodMap of
                Nothing -> throw BuiltinMethodNotFound
                Just method ->
                    method receiver arguments


findArgument :: String -> [Object] -> Runtime (Maybe Object)
findArgument _ [] = return Nothing
findArgument key (argument:arguments) = do
    let slots = objectSlots argument
    case Data.Map.lookup "name" slots of
        Nothing -> throw ObjectHasNoNameProperty
        Just nameStringId -> do
            nameString <- getObject nameStringId
            value <- lookupObjectValue nameString
            case value of
                IntValue _ -> undefined
                BuiltinMethodValue _ -> undefined
                ListValue _ -> undefined
                StringValue name -> do
                    if name == key
                        then return $ Just argument
                        else findArgument key arguments


createClass :: Object -> [Object] -> Runtime Object
createClass base arguments = do
    extendsVar <- findArgument "extends" arguments
    mixins <- case extendsVar of
        Nothing -> undefined
        Just list -> do
            extendsList <- messageObject "evaluate" list []
            extendsListValue <- lookupObjectValue extendsList
            case extendsListValue of
                IntValue _ -> undefined
                BuiltinMethodValue _ -> undefined
                StringValue _ -> undefined
                ListValue idList -> return idList

    baseSlotList <- slotsFromObject base
    objectClass <- case Data.Map.lookup "Object" $ fromList baseSlotList of
            Nothing -> throw ClassNotFound
            Just _id -> do
                variable <- getObject _id
                messageObject "evaluate" variable []

    object <- createObject objectClass []

    let _id = objectIdentifier object
    let mixins' = objectMixins object
    let properties = objectSlots object 

    modifyObject _id (mixins' ++ mixins) properties

    return object
    

createObject :: Object -> [Object] -> Runtime Object
createObject base arguments = do
    let parentId = Just $ objectIdentifier base
    slots <- slotsFromVariables arguments (objectSlots base)
    
    _id <- putObject parentId [] slots
    getObject _id


evaluateVariable :: Object -> [Object] -> Runtime Object
evaluateVariable variable _ = do
    let slots = objectSlots variable
    case Data.Map.lookup "object" slots of
        Nothing -> throw VariableHasNoObjectProperty
        Just _id -> getObject _id


evaluateMethod :: Object -> [Object] -> Runtime Object
evaluateMethod _ _ = undefined


evaluateObject :: Object -> [Object] -> Runtime Object
evaluateObject object _ = do
    return object


bootstrapObject :: String -> ObjectIdentifier -> ObjectIdentifier -> ObjectIdentifier -> ObjectIdentifier -> Runtime ()
bootstrapObject name objectId stringId variableId contextId = do
    nameStringId <- putObject (Just stringId) [] empty
    putValue (StringValue name) nameStringId

    let objectVarSlots = fromList [("name", nameStringId), ("object", objectId)]

    objectVarId <- putObject (Just variableId) [] objectVarSlots
    putSlot contextId name objectVarId

    return ()


bootstrapBuiltinMethod :: String -> ObjectIdentifier -> ObjectIdentifier -> ObjectIdentifier -> ObjectIdentifier -> (Object -> [Object] -> Runtime Object) -> Runtime ()
bootstrapBuiltinMethod name objectId stringId blockId variableId method = do
    objectMethodStringId <- putObject (Just stringId) [] empty
    putValue (StringValue name) objectMethodStringId
    
    objectMethodId <- putObject (Just blockId) [] empty
    builtinMethodId <- putBuiltinMethod method
    putValue (BuiltinMethodValue builtinMethodId) objectMethodId
    
    let objectMethodVarSlots =
            fromList [("name", objectMethodStringId), ("object", objectMethodId)]

    methodVarId <- putObject (Just variableId) [] objectMethodVarSlots
    putSlot objectId name methodVarId

    return ()    


bootstrapRuntime :: Runtime ()
bootstrapRuntime = do
    -- the environment mixin
    contextId <- putObject Nothing [] empty
    
    -- bootstrap Object
    
    -- first setup the necessary basic containers
    objectId <- putObject Nothing [contextId] empty
    klassId <- putObject (Just objectId) [] empty
    variableId <- putObject (Just klassId) [] empty
    stringId <- putObject (Just klassId) [] empty

    -- then stitch them all together
    bootstrapObject "Object" objectId stringId variableId contextId
    bootstrapObject "Class" klassId stringId variableId contextId
    bootstrapObject "Variable" variableId stringId variableId contextId
    bootstrapObject "String" stringId stringId variableId contextId

    -- setup methods
    methodId <- putObject (Just klassId) [] empty
    bootstrapObject "Method" methodId stringId variableId contextId

    
    -- setup built in methods
    blockId <- putObject (Just klassId) [] empty
    bootstrapObject "Block" blockId stringId variableId contextId

    bootstrapBuiltinMethod "evaluate" objectId stringId blockId variableId evaluateObject
    bootstrapBuiltinMethod "evaluate" variableId stringId blockId variableId evaluateVariable

    bootstrapBuiltinMethod "create" objectId stringId blockId variableId createObject
    bootstrapBuiltinMethod "create" klassId stringId blockId variableId createClass

    
    return ()

setupRuntime :: Runtime ()
setupRuntime = do
    bootstrapRuntime

    return ()


doTheThings :: Runtime ()
doTheThings = do    
    setupRuntime

    -- klass <- lookupClass
    -- string <- lookupString
    -- variable <- lookupVariable
    
    -- employee <- createObject klass []
    -- putReference "Employee" $ objectIdentifier employee 

    -- bobObject <- createObject employee []
    -- bobVariableName <- lookupValueObject (StringValue "bob")
    
    -- bob <- createObject variable []
    -- addSlot (objectIdentifier bob) "name" (objectIdentifier bobVariableName)
    -- addSlot (objectIdentifier bob) "object" (objectIdentifier bobObject)

    return ()


initialState :: RuntimeState
initialState = RuntimeState (empty, Identifier 0, empty, empty, MethodIdentifier 0)


main :: IO ()
main =
    pPrint $ runStateT doTheThings initialState
