module Halogen.Tooltip where


import Prelude

import Control.MonadZero (guard)
import CSS (position, relative, absolute)
import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Maybe (Maybe(..))
import Data.Record (insert)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ

-- | Queries! receive an input (used after the initial render, when the parent
-- | rerenders); pass along an output from the inner component to the outer;
-- | try to query the inner component; or update hovering state.
data Query g i o a
  = Input (Input i) a
  | Output o a
  | Query (Coyoneda g a)
  | Hover Boolean a

-- | The infos passed to the component as input and also kept in state: an
-- | input passed along to the inner component, the tooltip text, and a delay
-- | (okay I lowkey didn't use the delay).
type Infos i r =
  ( input :: i
  , tooltip :: String
  , delay :: Number
  | r
  )

-- | The input, just the above items.
type Input i = Record (Infos i ())

-- | The state, which also has hovering status.
type State i = Record (Infos i ( hovered :: Boolean ))

-- | A higher-order Halogen component that adds a tooltip.
-- |
-- | Note that we don't add anything to the output, but if we did want to pass
-- | up like hover status, we would use a sum, like
-- | `Variant ( hover :: Boolean, output :: o )`.
gimmeAToolTip :: forall      g             i  o m.
  -- look mom, no constraints on `m`!
  H.Component HH.HTML        g             i  o m ->
  H.Component HH.HTML (Query g i o) (Input i) o m
gimmeAToolTip lowerComponent =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Input
    }
  where
    initialState = insert (SProxy :: SProxy "hovered") false

    render :: State i -> H.ParentHTML (Query g i o) g Unit m
    render { input, tooltip, delay, hovered } =
      HH.div
        -- track hover status with mouseover/mouseout
        [ HE.onMouseOver (HE.input_ (Hover true))
        , HE.onMouseOut (HE.input_ (Hover false))
        , HP.class_ $ H.ClassName "tooltip-container"
        , style do
            position relative
        ] $
          [ HH.slot unit lowerComponent input (HE.input Output) ] <> ttip
      where
        ttip =
          guard hovered $> HH.div
            [ HP.class_ $ H.ClassName "tooltip"
            , style do
                -- not even gonna style this
                position absolute
            ]
            [ HH.text tooltip ]

    eval :: Query g i o ~> H.ParentDSL (State i) (Query g i o) g Unit o m
    eval (Input { input, tooltip, delay } a) = a <$ do
      H.modify _ { input = input, tooltip = tooltip, delay = delay }
    eval (Output o a) = a <$ H.raise o
    -- stolen from Halogen HOC example
    eval (Query iq) = iq # unCoyoneda \k q -> do
      result <- H.query unit q
      case result of
        Nothing ->
          HQ.halt "HOC inner component query failed (this should be impossible)"
        Just a -> pure (k a)
    eval (Hover hovered a) = a <$ H.modify _ { hovered = hovered }
