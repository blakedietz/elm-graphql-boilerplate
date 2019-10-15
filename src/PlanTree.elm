module PlanTree exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import PlanParsers.Json exposing (..)


type alias SelectedNode =
    Maybe Plan


type alias Config msg =
    { onMouseEnteredNode : Plan -> msg
    , onMouseLeftNode : Plan -> msg
    }


render : Config msg -> PlanJson -> SelectedNode -> Element msg
render config planJson selectedNode =
    let
        details =
            case selectedNode of
                Nothing ->
                    [ text "" ]

                Just plan ->
                    detailPanelContent plan
    in
    row [ width fill, paddingEach { top = 20, left = 0, right = 0, bottom = 0 } ]
        [ column [ width (fillPortion 7), height fill, alignTop ] <|
            planNodeTree config planJson.plan
        , column
            [ width (fillPortion 3 |> maximum 500)
            , height fill
            , alignTop
            , padding 5
            , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
            , Border.color Color.grey
            ]
          <|
            details
        ]


planNodeTree : Config msg -> Plan -> List (Element msg)
planNodeTree config plan =
    let
        nodeTypeEl nodeType =
            el [ Font.bold ] <| text nodeType

        treeNode node nodeDetails =
            [ el
                [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color lightBlue
                , mouseOver [ Background.color lightYellow ]
                , padding 4
                , onMouseEnter <| config.onMouseEnteredNode plan
                , onMouseLeave <| config.onMouseLeftNode plan
                ]
              <|
                paragraph [] (nodeTypeEl node.common.nodeType :: nodeDetails)
            , childNodeTree config node.common.plans
            ]
    in
    case plan of
        PCte cteNode ->
            treeNode cteNode
                [ text " on "
                , el [ Font.italic ] <| text cteNode.cteName
                , text <| " (" ++ cteNode.alias_ ++ ")"
                ]

        PGeneric genericNode ->
            treeNode { common = genericNode }
                []

        PResult resultNode ->
            treeNode resultNode
                []

        PSeqScan seqScanNode ->
            treeNode seqScanNode
                [ text " on "
                , el [ Font.italic ] <| text seqScanNode.relationName
                , text <| " (" ++ seqScanNode.alias_ ++ ")"
                ]

        PSort sortNode ->
            treeNode sortNode
                [ text " on "
                , el [ Font.italic ] <| text <| String.join ", " sortNode.sortKey
                ]


childNodeTree : Config msg -> Plans -> Element msg
childNodeTree config (Plans plans) =
    column [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ] <|
        List.concatMap (planNodeTree config) plans


detailPanelContent : Plan -> List (Element msg)
detailPanelContent plan =
    let
        attr name value =
            wrappedRow [ width fill ]
                [ el
                    [ width (px 200)
                    , paddingEach { right = 10, left = 10, top = 3, bottom = 3 }
                    , alignTop
                    ]
                  <|
                    text name
                , paragraph [ width fill, Font.bold, scrollbarX ] [ text value ]
                ]

        header name =
            el [ paddingEach { top = 10, bottom = 5, left = 10, right = 0 } ] <|
                el
                    [ Font.bold
                    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , Border.color lightGrey
                    ]
                <|
                    text name

        commonAttrs common =
            [ attr "Startup cost" <| String.fromFloat common.startupCost
            , attr "Total cost" <| String.fromFloat common.totalCost
            , attr "Schema" common.schema
            ]
    in
    case plan of
        PCte node ->
            commonAttrs node.common

        PGeneric node ->
            commonAttrs node

        PResult node ->
            commonAttrs node.common

        PSeqScan node ->
            commonAttrs node.common
                ++ [ header "Filter"
                   , attr "Filter" node.filter
                   , attr "Width" <| String.fromInt node.rowsRemovedByFilter
                   ]

        PSort node ->
            commonAttrs node.common
                ++ [ header "Sort"
                   , attr "Sort Key" <| String.join ", " node.sortKey
                   , attr "Sort Method" node.sortMethod
                   , attr "Sort Space Type" node.sortSpaceType
                   , attr "Sort Space Used" <| String.fromInt node.sortSpaceUsed
                   ]
