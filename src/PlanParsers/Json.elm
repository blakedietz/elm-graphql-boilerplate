module PlanParsers.Json exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)


type alias PlanJson =
    { executionTime : Float
    , plan : Plan
    , planningTime : Float
    , triggers : List String
    }


type Plan
    = PCte CteNode
    | PGeneric CommonFields
    | PResult ResultNode
    | PSeqScan SeqScanNode
    | PSort SortNode


type alias CteNode =
    { common : CommonFields
    , alias_ : String
    , cteName : String
    }


type alias ResultNode =
    { common : CommonFields
    , parentRelationship : String
    }


type alias SeqScanNode =
    { common : CommonFields
    , alias_ : String
    , filter : String
    , relationName : String
    , rowsRemovedByFilter : Int
    }



{-
   The generally preferred Elm solution for shared fields is to nest them in a field, so that’s what we are going to do.
   This fields is used as a "common" field for others.
-}


type alias CommonFields =
    { nodeType : String
    , plans : Plans
    , relationName : String
    , schema : String
    , startupCost : Float
    , totalCost : Float
    }


type alias SortNode =
    { common : CommonFields
    , sortKey : List String
    , sortMethod : String
    , sortSpaceUsed : Int
    , sortSpaceType : String
    }



{-
   Note that CommonFields contains a field of type Plans. This is simply a list of plans, but due to the way Elm deals
   with recursion as a strict language, Plans has to be a single- constructor custom type rather than a type alias.
-}


type Plans
    = Plans (List Plan)


decodeCommonFields : Decode.Decoder CommonFields
decodeCommonFields =
    Decode.succeed CommonFields
        |> required "Node Type" Decode.string
        |> optional "Plans" decodePlans (Plans [])
        |> optional "Relation Name" Decode.string ""
        |> optional "Schema" Decode.string ""
        |> required "Startup Cost" Decode.float
        |> required "Total Cost" Decode.float


decodePlans : Decode.Decoder Plans
decodePlans =
    Decode.map Plans <| Decode.list decodePlan


decodeCteNode : Decode.Decoder Plan
decodeCteNode =
    let
        innerDecoder =
            Decode.succeed CteNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> required "CTE Name" Decode.string
    in
    Decode.map PCte innerDecoder


decodeSeqScanNode : Decode.Decoder Plan
decodeSeqScanNode =
    let
        innerDecoder =
            Decode.succeed SeqScanNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> optional "Filter" Decode.string ""
                |> required "Relation Name" Decode.string
                |> optional "Rows Removed by Filter" Decode.int 0
    in
    Decode.map PSeqScan innerDecoder


decodeSortNode : Decode.Decoder Plan
decodeSortNode =
    let
        innerDecoder =
            Decode.succeed SortNode
                |> custom decodeCommonFields
                |> required "Sort Key" (Decode.list Decode.string)
                |> required "Sort Method" Decode.string
                |> required "Sort Space Used" Decode.int
                |> required "Sort Space Type" Decode.string
    in
    Decode.map PSort innerDecoder


decodeGenericNode : Decode.Decoder Plan
decodeGenericNode =
    Decode.map PGeneric decodeCommonFields


decodePlan : Decode.Decoder Plan
decodePlan =
    Decode.field "Node Type" Decode.string |> Decode.andThen decodeNode


decodeNode : String -> Decode.Decoder Plan
decodeNode nodeType =
    case nodeType of
        "CTE Scan" ->
            decodeCteNode

        "Result" ->
            decodeResultNode

        "Seq scan" ->
            decodeSeqScanNode

        "Sort" ->
            decodeSortNode

        _ ->
            decodeGenericNode



{-
   Decoders for other node types will follow the same pattern. Remember that the ordering of operations is important
   when constructing a decoder. Since we called custom first, it will be used to populate the first field of ResultNode,
   which is common. Luckily, if you don’t get the order right, you will see an error at compile time rather than at
   run time (unless both fields happen to be of the same type, which is something to watch out for).
-}


decodeResultNode : Decode.Decoder Plan
decodeResultNode =
    let
        innerDecoder =
            Decode.succeed ResultNode
                |> custom decodeCommonFields
                |> required "Parent Relationship" Decode.string
    in
    Decode.map PResult innerDecoder



{-
   succeed is a Json.Decode function which we use to kick off the process of building the decoder. We pass it the
   constructor of the type we want to construct from JSON, and then provide definitions for each field with the help of
   the pipe operator.

   Keep in mind that the order of fields here has to match the order of fields in the type alias. For each optional
   field, we need to supply a default value of the right type.
-}


decodePlanJson : Decode.Decoder PlanJson
decodePlanJson =
    Decode.succeed PlanJson
        |> optional "Execution time" Decode.float 0
        {-
           For “Execution Time”, “Planning Time”, and “Triggers”, I’m using the basic decoders supplied in Json.Decode.
           For the “Plan” field, I have to supply a custom decoder (decodePlan) because it’s an object rather than a
           simple value. We will also need to define the Plan type which this decoder will produce. We will do it a bit
           later.
        -}
        |> required "Plan" decodePlan
        |> optional "Planning Time" Decode.float 0
        |> optional "Triggers" (Decode.list Decode.string) []
