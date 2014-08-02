{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module API.IB.Request where

import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as BC
import           Data.ByteString.Lazy.Builder       (Builder, byteString,
                                                     stringUtf8)
import           Data.ByteString.Lazy.Builder.ASCII (doubleDec, intDec)
import qualified Data.Map                           as Map (size, toList)
import           Data.Maybe
import           Data.Monoid                        hiding (All, Product)
import           Data.String
import           Data.Time
import           Prelude                            hiding (takeWhile)
import           System.Locale

import           API.IB.Constant
import           API.IB.Data
import           API.IB.Enum
import           API.IB.Util

-- -----------------------------------------------------------------------------
-- Utilities

ibMsg :: Int -> IBRequestType -> [Builder] -> ByteString
ibMsg version reqtype builders = bMake sepC $ bEncode reqtype : intDec version : builders

ibMsgConcat :: Int -> IBRequestType -> [[Builder]] -> ByteString
ibMsgConcat version reqtype builders = ibMsg version reqtype $ concat builders

-- -----------------------------------------------------------------------------
-- Message Factory

createClientIdMsg :: Int -> ByteString
createClientIdMsg clientid = BC.pack (show clientid) <> sepB

createClientVersionMsg :: Int -> ByteString
createClientVersionMsg cversion = BC.pack (show cversion) <> sepB

createMsg :: Int -> IBRequest -> ByteString
createMsg sv request = case request of
  (RequestMarketData tid c gtl ss) -> createRequestMarketDataMsg sv tid c gtl ss
  (CancelMarketData tid) -> createCancelMarketDataMsg sv tid
  (PlaceOrder oid c o) -> createPlaceOrderMsg sv oid c o
  (CancelOrder oid) -> createCancelOrderMsg sv oid
  (RequestOpenOrders) -> createRequestOpenOrdersMsg sv
  (RequestAccountData sub ac) -> createRequestAccountDataMsg sv sub ac
  (RequestExecutions rid ef) -> createRequestExecutionsMsg sv rid ef
  (RequestIds n) -> createRequestIdsMsg sv n
  (RequestContractData rid c) -> createRequestContractDataMsg sv rid c
  (RequestAutoOpenOrders ab) -> createRequestAutoOpenOrdersMsg sv ab
  (RequestAllOpenOrders) -> createRequestAllOpenOrdersMsg sv
  (RequestManagedAccounts ) -> createRequestManagedAccountsMsg sv
  (RequestHistoricalData tid c dt dur bs bb rth fd) -> createRequestHistoricalDataMsg sv tid c dt dur bs bb rth fd
  (CancelHistoricalData tid) -> createCancelHistoricalDataMsg sv tid
  (RequestCurrentTime) -> createRequestCurrentTimeMsg sv
  (RequestRealTimeBars tid c bs bb rth) -> createRequestRealTimeBarsMsg sv tid c bs bb rth
  (CancelRealTimeBars tid) -> createCancelRealTimeBarsMsg sv tid
  (RequestGlobalCancel) -> createRequestGlobalCancelMsg sv
  (RequestMarketDataType mdt) -> createRequestMarketDataTypeMsg sv mdt
  (RequestPositions) -> createRequestPositionsMsg sv
  (RequestAccountSummary rid g tl) -> createRequestAccountSummaryMsg sv rid g tl
  (CancelAccountSummary rid) -> createCancelAccountSummaryMsg sv rid
  (CancelPositions) -> createCancelPositionsMsg sv

-- -----------------------------------------------------------------------------

createRequestMarketDataMsg :: Int -> Int -> IBContract -> [IBGenericTickType] -> Bool -> ByteString
createRequestMarketDataMsg sversion tickerid IBContract{..} genticktypes snapshot =
  ibMsgConcat 10 ReqMktDataT
    [ [ intDec tickerid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 _conExpiry
      , doubleDec _conStrike
      , stringUtf8 _conRight
      , stringUtf8 _conMultiplier
      , stringUtf8 _conExchange
      , stringUtf8 _conPrimaryExch
      , stringUtf8 _conCurrency
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass]
    , [ stringUtf8 "0"
      , bSep ',' (map bEncode genticktypes)
      , intDec $ boolBinary snapshot
      ]
    ]

-- -----------------------------------------------------------------------------

createCancelMarketDataMsg :: Int -> Int -> ByteString
createCancelMarketDataMsg _ tickerid = ibMsg 1 CancelMktDataT [intDec tickerid]

-- -----------------------------------------------------------------------------

createPlaceOrderMsg :: Int -> ByteString -> IBContract -> IBOrder -> ByteString
createPlaceOrderMsg sversion orderid IBContract{..} IBOrder{..} =
  ibMsgConcat 40 PlaceOrderT
    [ [ byteString orderid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 _conExpiry
      , doubleDec _conStrike
      , stringUtf8 _conRight
      , stringUtf8 _conMultiplier
      , stringUtf8 _conExchange
      , stringUtf8 _conPrimaryExch
      , stringUtf8 _conCurrency
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
      ]
    , [ stringUtf8 _conSecIdType
      , stringUtf8 _conSecId
      ]
    , [ bEncode _orderAction
      , intDec _orderTotalQuantity
      , bEncode _orderType
      , maybe bEmpty doubleDec _orderLmtPrice
      , maybe bEmpty doubleDec _orderAuxPrice
      , bEncode _orderTif
      , maybe bEmpty stringUtf8 _orderOcaGroup
      , stringUtf8 _orderAccount
      , bEncode _orderOpenClose
      , bEncode _orderOrigin
      , stringUtf8 _orderRef
      , intDec $ boolBinary _orderTransmit
      , maybe (intDec 0) intDec _orderParentId
      , intDec $ boolBinary _orderBlockOrder
      , intDec $ boolBinary _orderSweepToFill
      , maybe (intDec 0) intDec _orderDisplaySize
      , bEncode _orderTriggerMethod
      , intDec $ boolBinary _orderOutsideRth
      , intDec $ boolBinary _orderHidden
      ]
    , if _conSecType == IBBag then
        let
          _conComboLegsCount = length _conComboLegs
          _orderComboLegsCount = length _orderComboLegs
          _orderSmartComboRoutingParamsCount = Map.size _orderSmartComboRoutingParams
        in
          if _conComboLegsCount > 0 then
            intDec _conComboLegsCount :
            concatMap
              (\l ->
                [ intDec $ _comConId l
                , intDec $ _comRatio l
                , stringUtf8 $ _comAction l
                , stringUtf8 $ _comExchange l
                , intDec $ _comOpenClose l
                , intDec $ _comShortSaleSlot l
                , stringUtf8 $ _comDesignatedLocation l
                , intDec $ _comExemptCode l
                ])
              _conComboLegs
          else
            [intDec 0]
          <>
          if _orderComboLegsCount > 0 then
            intDec _orderComboLegsCount :
            map
              (doubleDec . _oclPrice)
              _orderComboLegs
          else
            [intDec 0]
          <>
          if _orderSmartComboRoutingParamsCount > 0 then
            intDec _orderSmartComboRoutingParamsCount :
            concatMap
              (\(tag,value) -> map stringUtf8 [tag,value])
              (Map.toList _orderSmartComboRoutingParams)
          else
            [intDec 0]
      else
        [],
      [ bEmpty
      , doubleDec _orderDiscretionaryAmt
      , maybe bEmpty stringUtf8 _orderGoodAfterTime
      , maybe bEmpty stringUtf8 _orderGoodTillDate
      , stringUtf8 _orderFAGroup
      , stringUtf8 _orderFAMethod
      , stringUtf8 _orderFAPercentage
      , stringUtf8 _orderFAProfile
      , maybe bEmpty bEncode _orderShortSaleSlot
      , maybe bEmpty stringUtf8 _orderDesignatedLocation
      , intDec _orderExemptCode
      , maybe bEmpty bEncode _orderOcaType
      , maybe bEmpty bEncode _orderRule80A
      , stringUtf8 _orderSettlingFirm
      , intDec $ boolBinary _orderAllOrNone
      , maybe (intDec 0) intDec _orderMinQty
      , maybe bEmpty doubleDec _orderPercentOffset
      , intDec $ boolBinary _orderETradeOnly
      , intDec $ boolBinary _orderFirmQuoteOnly
      , maybe bEmpty doubleDec _orderNBBOPriceCap
      , maybe bEmpty bEncode _orderAuctionStrategy
      , maybe bEmpty doubleDec _orderStartingPrice
      , maybe bEmpty doubleDec _orderStockRefPrice
      , maybe bEmpty doubleDec _orderDelta
      , maybe bEmpty doubleDec _orderStockRangeLower
      , maybe bEmpty doubleDec _orderStockRangeUpper
      , intDec $ boolBinary _orderOverridePercentageConstraints
      , maybe bEmpty doubleDec _orderVolatility
      , maybe bEmpty bEncode _orderVolatilityType
      , maybe bEmpty stringUtf8 _orderDeltaNeutralOrderType
      , maybe bEmpty doubleDec _orderDeltaNeutralAuxPrice
      ]
    , if isJust _orderDeltaNeutralOrderType then
        [ maybe (intDec 0) intDec _orderDeltaNeutralConId
        , maybe bEmpty stringUtf8 _orderDeltaNeutralSettlingFirm
        , maybe bEmpty stringUtf8 _orderDeltaNeutralClearingAccount
        , maybe bEmpty stringUtf8 _orderDeltaNeutralClearingIntent
        ]
      else
        []
    , if isJust _orderDeltaNeutralOrderType && sversion >= minServerVersionDeltaNeutralOpenClose then
        [ maybe bEmpty bEncode _orderDeltaNeutralOpenClose
        , intDec $ boolBinary _orderDeltaNeutralShortSale
        , maybe bEmpty bEncode _orderDeltaNeutralShortSaleSlot
        , maybe bEmpty stringUtf8 _orderDeltaNeutralDesignatedLocation
        ]
      else
        []
    , [ maybe (intDec 0) intDec _orderContinuousUpdate
      , maybe bEmpty bEncode _orderReferencePriceType
      , maybe bEmpty doubleDec _orderTrailStopPrice
      , maybe bEmpty doubleDec _orderTrailingPercent
      , maybe bEmpty intDec _orderScaleInitLevelSize
      , maybe bEmpty intDec _orderScaleSubsLevelSize
      , maybe bEmpty doubleDec _orderScalePriceIncrement
      ]
    , if isJust _orderScalePriceIncrement && (fromJust _orderScalePriceIncrement > 0) then
        [ maybe bEmpty doubleDec _orderScalePriceAdjustValue
        , maybe bEmpty intDec _orderScalePriceAdjustInterval
        , maybe bEmpty doubleDec _orderScaleProfitOffset
        , intDec $ boolBinary _orderScaleAutoReset
        , maybe bEmpty intDec _orderScaleInitPosition
        , maybe bEmpty intDec _orderScaleInitFillQty
        , intDec $ boolBinary _orderScaleRandomPercent
        ]
      else
        []
    , [ maybe bEmpty bEncode _orderHedgeType
      ]
    , if isJust _orderHedgeType then
        maybe [bEmpty] ((:[]) . stringUtf8) _orderHedgeParam
      else
        []
    , [ intDec $ boolBinary _orderOptOutSmartRouting,
        stringUtf8 _orderClearingAccount,
        stringUtf8 _orderClearingIntent,
        intDec $ boolBinary _orderNotHeld
      ]
    , if isJust _conUnderComp then
        let
          uc = fromJust _conUnderComp
        in
          [ intDec $ _ucConId uc
          , doubleDec $ _ucDelta uc
          , doubleDec $ _ucPrice uc
          ]
      else
        [ intDec $ boolBinary False]
    , [ maybe bEmpty stringUtf8 _orderAlgoStrategy
      ]
    , if isJust _orderAlgoStrategy then
        intDec (Map.size _orderAlgoParams) : concatMap (\(k,v) -> map stringUtf8 [k,v]) (Map.toList _orderAlgoParams)
      else
        []
    , [ intDec $ boolBinary _orderWhatIf
      ]
    ]

-- -----------------------------------------------------------------------------

createCancelOrderMsg :: Int -> ByteString -> ByteString
createCancelOrderMsg _ orderid = ibMsg 1 CancelOrderT [byteString orderid]

-- -----------------------------------------------------------------------------

createRequestOpenOrdersMsg :: Int -> ByteString
createRequestOpenOrdersMsg _ = ibMsg 1 ReqOpenOrdersT []

-- -----------------------------------------------------------------------------

createRequestAccountDataMsg :: Int -> Bool -> String -> ByteString
createRequestAccountDataMsg sversion subscribe accountcode =
  ibMsg 2 ReqAccountDataT $
    intDec (boolBinary subscribe) : [stringUtf8 accountcode | sversion >= 9]

-- -----------------------------------------------------------------------------

createRequestExecutionsMsg :: Int -> Int -> IBExecutionFilter -> ByteString
createRequestExecutionsMsg _ requestid IBExecutionFilter{..} =
  ibMsg 3 ReqExecutionsT
    [ intDec requestid
    , intDec _efClientId
    , stringUtf8 _efAcctCode
    , stringUtf8 _efTime
    , stringUtf8 _efSymbol
    , bEncode _efSecType
    , stringUtf8 _efExchange
    , stringUtf8 _efSide
    ]

-- -----------------------------------------------------------------------------

createRequestIdsMsg :: Int -> Int -> ByteString
createRequestIdsMsg _ numids = ibMsg 1 ReqIdsT [intDec numids]

-- -----------------------------------------------------------------------------

createRequestContractDataMsg :: Int -> Int -> IBContract -> ByteString
createRequestContractDataMsg sversion requestid IBContract{..} =
  ibMsgConcat 7 ReqContractDataT
    [ [ intDec requestid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 _conExpiry
      , doubleDec _conStrike
      , stringUtf8 _conRight
      , stringUtf8 _conMultiplier
      , stringUtf8 _conExchange
      , stringUtf8 _conCurrency
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
      ]
    , [ intDec $ boolBinary _conIncludeExpired
      , stringUtf8 _conSecIdType
      , stringUtf8 _conSecId
      ]
    ]

-- -----------------------------------------------------------------------------

createRequestAutoOpenOrdersMsg :: Int -> Bool -> ByteString
createRequestAutoOpenOrdersMsg _ autobind =
  ibMsg 1 ReqAutoOpenOrdersT [intDec $ boolBinary autobind]

-- -----------------------------------------------------------------------------

createRequestAllOpenOrdersMsg :: Int -> ByteString
createRequestAllOpenOrdersMsg _ = ibMsg 1 ReqAllOpenOrdersT []

-- -----------------------------------------------------------------------------

createRequestManagedAccountsMsg :: Int -> ByteString
createRequestManagedAccountsMsg _ = ibMsg 1 ReqManagedAccountsT []

-- -----------------------------------------------------------------------------

createRequestHistoricalDataMsg :: Int -> Int -> IBContract -> LocalTime -> IBDuration -> Int -> IBBarBasis -> Bool -> IBFormatDate -> ByteString
createRequestHistoricalDataMsg sversion tickerid IBContract{..} enddatetime duration@(IBDuration i u) barsize barbasis userth formatdate
  | i <= 0 = error "invalid duration in RequestHistoricalDataMsg."
  | i > 1 && u == Y = error "invalid duration in RequestHistoricalDataMsg."
  | barsize `notElem` [1,5,15,30,60,120,180,300,900,1800,3600,86400] = error "invalid barsize in RequestHistoricalDataMsg."
  | otherwise = ibMsgConcat 5 ReqHistoricalDataT
      [ [ intDec tickerid
        ]
      , [ intDec _conId | sversion >= minServerVersionTradingClass]
      , [ stringUtf8 _conSymbol
        , bEncode _conSecType
        , stringUtf8 _conExpiry
        , doubleDec _conStrike
        , stringUtf8 _conRight
        , stringUtf8 _conMultiplier
        , stringUtf8 _conExchange
        , stringUtf8 _conPrimaryExch
        , stringUtf8 _conCurrency
        , stringUtf8 _conLocalSymbol
        ]
      , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass]
      , [ intDec $ boolBinary _conIncludeExpired,
          stringUtf8 $ formatTime defaultTimeLocale "%Y%m%d %H:%M:%S" enddatetime,
          stringUtf8 $ formatSeconds barsize,
          bEncode duration,
          intDec $ boolBinary userth,
          bEncode barbasis,
          bEncode formatdate
        ]
      ]

-- -----------------------------------------------------------------------------

createCancelHistoricalDataMsg :: Int -> Int -> ByteString
createCancelHistoricalDataMsg _ tickerid =
  ibMsg 1  CancelHistoricalDataT [intDec tickerid]

-- -----------------------------------------------------------------------------

createRequestCurrentTimeMsg :: Int -> ByteString
createRequestCurrentTimeMsg _ = ibMsg 1 ReqCurrentTimeT []

-- -----------------------------------------------------------------------------
createRequestRealTimeBarsMsg :: Int -> Int -> IBContract -> Int -> IBBarBasis -> Bool -> ByteString
createRequestRealTimeBarsMsg sversion tickerid IBContract{..} barsize barbasis userth
  | barsize /= 5 = error "invalid barsize in RequestRealTimeBarMsg."
  | barbasis `notElem` [BarBasisTrades,BarBasisBid,BarBasisAsk,BarBasisMidpoint] = error "invalid barbasis in RequestRealTimeBarMsg."
  | otherwise = ibMsgConcat 2 ReqRealTimeBarsT
      [ [ intDec tickerid
        ]
      , [ intDec _conId | sversion >= minServerVersionTradingClass
        ]
      , [ stringUtf8 _conSymbol
        , bEncode _conSecType
        , stringUtf8 _conExpiry
        , doubleDec _conStrike
        , stringUtf8 _conRight
        , stringUtf8 _conMultiplier
        , stringUtf8 _conExchange
        , stringUtf8 _conPrimaryExch
        , stringUtf8 _conCurrency
        , stringUtf8 _conLocalSymbol
        ]
      , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
        ]
      , [ intDec barsize
        , bEncode barbasis
        , intDec $ boolBinary userth
        ]
      ]

-- -----------------------------------------------------------------------------

createCancelRealTimeBarsMsg :: Int -> Int -> ByteString
createCancelRealTimeBarsMsg _ tickerid = ibMsg 1 CancelRealTimeBarsT [intDec tickerid]

-- -----------------------------------------------------------------------------

createRequestGlobalCancelMsg :: Int -> ByteString
createRequestGlobalCancelMsg _ = ibMsg 1 ReqGlobalCancelT []

-- -----------------------------------------------------------------------------

createRequestMarketDataTypeMsg :: Int -> IBMarketDataType -> ByteString
createRequestMarketDataTypeMsg _ marketdatatype =
  ibMsg 1 ReqMarketDataTypeT [bEncode marketdatatype]

-- -----------------------------------------------------------------------------

createRequestPositionsMsg :: Int -> ByteString
createRequestPositionsMsg _ = ibMsg 1 ReqPositionsT []

-- -----------------------------------------------------------------------------

createRequestAccountSummaryMsg :: Int -> Int -> IBGroup -> [IBTag] -> ByteString
createRequestAccountSummaryMsg sversion reqid grp tags
  | sversion < minServerVersionAccountSummary = error "invalid server version in RequestAccountSummaryMsg."
  | otherwise = ibMsg 1 ReqAccountSummaryT
      [ intDec reqid
      , bEncode grp
      , bSep ',' (map bEncode tags)
      ]

-- -----------------------------------------------------------------------------

createCancelAccountSummaryMsg :: Int -> Int -> ByteString
createCancelAccountSummaryMsg sversion requestid
  | sversion < minServerVersionAccountSummary = error "invalid server version in CancelAccountSummaryMsg."
  | otherwise = ibMsg 1 CancelAccountSummaryT [intDec requestid]

-- -----------------------------------------------------------------------------

createCancelPositionsMsg :: Int -> ByteString
createCancelPositionsMsg sversion
  | sversion < minServerVersionAccountSummary = error "invalid server version in CancelPositionsMsg."
  | otherwise = ibMsg 1 CancelPositionsT []
