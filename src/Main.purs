module Main where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen (PropName(..), liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.Storage.Storage (Storage)
import Web.Storage.Storage as Storage
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State = {
  items :: Map ItemId String
, items_order :: Array ItemId
, m_backup :: Maybe StoredState
, selectedItems :: Map ItemId String
, m_storage :: Maybe Storage
, m_movingItemIx :: Maybe Int
}

type StoredState = {
  items :: Map ItemId String
, items_order :: Array ItemId
}

type ItemId = Int

initialState :: State
initialState = {
  items: Map.empty
, items_order: []
, m_backup: Nothing
, selectedItems: Map.empty
, m_storage: Nothing
, m_movingItemIx: Nothing
}


data Action 
  = Init
  | AddItem
  | StartMovingItem ItemId
  | ProcessKey SubscriptionId String
  | UpdateItem ItemId String
  | DeleteAllItems
  | Undo
  | SelectItem ItemId
  | DeselectItem ItemId
  | UpdateSelected ItemId String


component :: forall output input query. H.Component query input output Aff
component =
  H.mkComponent
    { 
      initialState: \_ -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction, initialize = Just Init }
      -- { handleAction = handleAction }
    }
  where
  render {items, items_order, selectedItems, m_movingItemIx, m_backup} = 
    HH.div_ [
      HH.table_ $
        [
          HH.tr_ [ HH.th_ [HH.text $ "Item"]]
        ]
        <> map itemRow (Array.zip (0..(numberOfItems-1)) items_order),
      addButton, 
      clearOrUndoButton,
      HH.table_ $
        [
          HH.tr_ [ HH.th_ [HH.text $ "Selected"] ]
        ]
        <> map selectedRow items_order,
      result
    ]
    where
    numberOfItems = Array.length items_order
    result = HH.textarea 
      [ HP.value text
      , HP.prop (PropName "size") "100"
      ]
      where
      text = String.joinWith "<br/>" selectedItemsTexts
      selectedItemsTexts = Array.catMaybes $ map pickSelected items_order
        where
        pickSelected = flip Map.lookup selectedItems

    addButton = 
      HH.button [HP.title "new", HE.onClick \ _ -> AddItem ] [HH.text "new"]
    -- deleteButton itemId = 
    --   HH.button [HP.title "delete", HE.onClick \ _ -> DeleteItem itemId ] [HH.text "X"]
    clearButton = 
      HH.button [HP.title "CLEAR", HE.onClick \ _ -> DeleteAllItems ] [HH.text "CLEAR"]
    undoButton = 
      HH.button [HP.title "UNDO", HE.onClick \ _ -> Undo ] [HH.text "UNDO"]
    clearOrUndoButton =
      case m_backup of
        Nothing -> clearButton
        Just _ -> undoButton

    moveButton itemIx = 
      HH.button [HP.title "↕", HE.onClick \ _ -> StartMovingItem itemIx ] [HH.text "↕"]
    selectButton itemId = 
      HH.button [HP.title "select", HE.onClick \ _ -> SelectItem itemId ] [HH.text "select"]
    deselectButton itemId = 
      HH.button [HP.title "deselect", HE.onClick \ _ -> DeselectItem itemId ] [HH.text "desel."]
    itemInput text action =
      HH.input 
        [ HP.prop (PropName "size") "100"
        , HP.value text
        , HE.onValueInput action
        ]
    itemRow (Tuple itemIx itemId) = 
      HH.tr_ 
        [ 
          -- HH.td_ [ deleteButton itemId, itemInput itemText (UpdateItem itemId) ]
          HH.td_ [ moveControl, itemInput itemText (UpdateItem itemId), selectToggleButton ]
        ]
        where
        itemText = 
          maybe "" identity $ Map.lookup itemId items
        selectToggleButton =
           case Map.lookup itemId selectedItems of
              Nothing -> selectButton itemId
              Just _ -> deselectButton itemId
        moveControl = 
          case m_movingItemIx of
            Nothing -> moveButton itemIx
            Just movingItemIx 
              | movingItemIx == itemIx -> HH.text "↕"
              | otherwise -> HH.text ""
    selectedRow itemId = 
      HH.tr_ 
        [ 
          HH.td_ selectedItemControls
        ]
        where
        selectedItemControls =
           case Map.lookup itemId selectedItems of
              Nothing -> [ ]
              Just selectedItemText -> 
                [ itemInput selectedItemText (UpdateSelected itemId)
                , deselectButton itemId
                ]

  handleAction = case _ of
    Init -> do
      -- get hold of this window's local storage:
      storage <- liftEffect $ window >>= Window.localStorage
      H.modify_ $ _ { m_storage = Just storage }
      readLocalStorage

    AddItem -> do
      H.modify_ addItem
      updateLocalStorage
      where
      addItem s@{ items, items_order } = s { items = items', items_order = items_order', m_backup = Nothing }
        where
        items_order' = items_order <> [newItemId]
        items' = Map.insert newItemId ("item " <> (show newItemId)) items
        newItemId = 1 + (maybe 0 identity $ map _.key $ Map.findMax items)

    -- DeleteItem itemId -> do
    --   H.modify_ $ \ s-> s { items = Map.delete itemId s.items }
    --   updateLocalStorage
    --   handleAction $ DeselectItem itemId

    DeleteAllItems -> do
      H.modify_ $ \ s -> 
        initialState 
          { m_storage = s.m_storage, m_backup = Just {items: s.items, items_order: s.items_order} }
      updateLocalStorage

    Undo -> do
      {m_backup, m_storage} <- H.get
      case m_backup of
        Nothing -> pure unit
        Just {items, items_order} -> do
          H.modify_ $ \ _ -> initialState { m_storage = m_storage, items = items, items_order = items_order }
          updateLocalStorage

    UpdateItem itemId itemText -> do
      H.modify_ $ \ s-> s { items = updateItem s.items }
      updateLocalStorage
      where
      updateItem items = Map.insert itemId itemText items

    UpdateSelected itemId itemText -> do
      H.modify_ $ \ s-> s { selectedItems = updateItem s.selectedItems }
      where
      updateItem items = Map.insert itemId itemText items

    SelectItem itemId -> do
      H.modify_ $ \ s-> s { selectedItems = selectItem s }
      where
      selectItem {items, selectedItems} = 
        case Map.lookup itemId items of
          Just itemText -> Map.insert itemId itemText selectedItems
          Nothing -> selectedItems

    DeselectItem itemId -> do
      H.modify_ $ \ s-> s { selectedItems = Map.delete itemId s.selectedItems }
    
    StartMovingItem itemId -> do
      H.modify_ $ _ { m_movingItemIx = Just itemId }
      subscribeToKeys ProcessKey
    ProcessKey sid keyName -> do
      {m_movingItemIx} <- H.get
      case m_movingItemIx of
        Nothing -> pure unit
        Just movingItemIx ->
          case keyName of
            "ArrowUp"   -> H.modify_ $ moveItemTo (movingItemIx - 1)
            "ArrowDown" -> H.modify_ $ moveItemTo (movingItemIx + 1)
            _ -> do -- on any other key, stop moving the item
              H.modify_ $ _ { m_movingItemIx = Nothing }
              H.unsubscribe sid              
              updateLocalStorage
          where
          moveItemTo j s@{ items_order } = 
            case 0 <= j && j < Array.length items_order of
              false -> s -- no change possible since j is out of bounds
              true -> 
                s { items_order = swap movingItemIx j items_order
                  , m_movingItemIx = Just j }
          swap i j order =
            case m_order' of
              Nothing -> order
              Just order' -> order'
            where
            m_order' = do
              item_i <- order Array.!! i
              item_j <- order Array.!! j
              order1 <- Array.updateAt i item_j order
              order2 <- Array.updateAt j item_i order1
              pure order2

  updateLocalStorage = do
    {m_storage, items, items_order} <- H.get
    let jsonS = stringify $ encodeJson {items, items_order}
    case m_storage of
      Nothing -> pure unit
      Just s -> 
        liftEffect $ do
          log $ "updateLocalStorage: jsonS = " <> jsonS
          Storage.setItem "items" jsonS s

  readLocalStorage = do
    {m_storage} <- H.get
    case m_storage of
      Nothing -> pure unit
      Just storage -> do
        -- attempt to get history from local storage:
        m_itemsS <- liftEffect $ Storage.getItem "items" storage
        let m_items = parseItems m_itemsS
        case m_items of
          Nothing -> pure unit
          Just {items: items', items_order: items_order'} ->
            H.modify_ $ _ { items = items', items_order = items_order' }
        where
        parseItems :: Maybe String -> Maybe StoredState
        parseItems m_itemsS = do
          itemsS <- m_itemsS
          json   <- hush $ parseJson itemsS
          hush $ decodeJson json

subscribeToKeys :: 
  forall m output slots action state. MonadAff m => 
  (SubscriptionId -> String -> action) -> 
  H.HalogenM state action slots output m Unit
subscribeToKeys keyAction = do
  document <- H.liftEffect $ Window.document =<< Web.window
  H.subscribe' \sid ->
    eventListener
      KET.keydown
      (HTMLDocument.toEventTarget document)
      (map (keyHandler sid) <<< KE.fromEvent)
  where
  keyHandler sid ev = keyAction sid (KE.key ev)
