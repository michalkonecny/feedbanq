module Main where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (PropName(..), liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage)
import Web.Storage.Storage as Storage

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State = {
  items :: Map ItemId String
, items_order :: Array ItemId
, selectedItems :: Map ItemId String
, m_storage :: Maybe Storage
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
, selectedItems: Map.empty
, m_storage: Nothing
}


data Action 
  = Init
  | AddItem
  | UpdateItem ItemId String
  | DeleteItem ItemId
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
  render {items, items_order, selectedItems} = 
    HH.div_ [
      HH.table_ $
        [
          HH.tr_ [ HH.th_ [HH.text $ "Item"]]
        ]
        <> map itemRow items_order,
      addButton,
      HH.table_ $
        [
          HH.tr_ [ HH.th_ [HH.text $ "Selected"] ]
        ]
        <> map selectedRow items_order,
      result
    ]
    where
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
    itemRow itemId = 
      HH.tr_ 
        [ 
          -- HH.td_ [ deleteButton itemId, itemInput itemText (UpdateItem itemId) ]
          HH.td_ [ itemInput itemText (UpdateItem itemId), selectToggleButton ]
        ]
        where
        itemText = 
          maybe "" identity $ Map.lookup itemId items
        selectToggleButton =
           case Map.lookup itemId selectedItems of
              Nothing -> selectButton itemId
              Just _ -> deselectButton itemId
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
      storage <- liftEffect $ window >>= localStorage
      H.modify_ $ _ { m_storage = Just storage }
      readLocalStorage

    AddItem -> do
      H.modify_ addItem
      updateLocalStorage
      where
      addItem s@{ items, items_order } = s { items = items', items_order = items_order' }
        where
        items_order' = [newItemId] <> items_order
        items' = Map.insert newItemId ("item " <> (show newItemId)) items
        newItemId = 1 + (maybe 0 identity $ map _.key $ Map.findMax items)

    DeleteItem itemId -> do
      H.modify_ $ \ s-> s { items = Map.delete itemId s.items }
      updateLocalStorage
      handleAction $ DeselectItem itemId

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

  updateLocalStorage = do
    {m_storage, items, items_order} <- H.get
    case m_storage of
      Nothing -> pure unit
      Just s -> 
        liftEffect $ 
          Storage.setItem "items" 
            (stringify $ encodeJson {items, items_order}) s

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

