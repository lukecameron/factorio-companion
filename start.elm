import Html exposing (Html, Attribute, text, div, input, pre)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, int, string, dict, object6, object5, object4, object3, maybe, float, tuple2, (:=), at, oneOf)
import Json.Decode as Json
import Result exposing (..)
import Basics


main =
  beginnerProgram { model = "", view = view, update = update }

-- PROCESS
decodeCount : Decoder Float
decodeCount =
  at ["count"] float

decodeIngredients : Decoder (List IngredientJson)
decodeIngredients =
  Json.list decodeIngredient

decodeRecipe : Decoder RecipeJson
decodeRecipe =
  object6 RecipeJson
    ("name" := string)
    (maybe ("result" := string))
    (maybe ("results" := decodeResults))
    ("ingredients" := decodeIngredients)
    ("type" := string)
    (maybe ("energy_required" := float))

attemptDecodeBasicIngredient : Decoder (Maybe (String, Float))
attemptDecodeBasicIngredient =
  maybe (tuple2 (,) string float)


constructTypedIngredient : String -> String -> Float -> IngredientJson
constructTypedIngredient n t a = Typed (TypedIngredientJson n t a)

constructBasicIngredient : String -> Float -> IngredientJson
constructBasicIngredient n a = Basic (n, a)

-- ingredients are either a tuple2 or an object3.
-- to do this I try to decode a tuple2, and based on the result
-- of that I decide which decoder to use
decodeIngredient : Decoder IngredientJson
decodeIngredient = attemptDecodeBasicIngredient `Json.andThen` \attempt ->
  case attempt of
    Just _         -> tuple2  constructBasicIngredient string float
    Nothing        -> object3 constructTypedIngredient
                              ("name" := string)
                              ("type" := string)
                              ("amount" := float)

decodeResults = Json.list decodeResult
type alias ComplexResult = {
  name : String,
  resultType : String,
  amount : Float
}

decodeResult = object3 ComplexResult
                       ("name" := string)
                       ("type" := string)
                       ("amount" := float)

decodeAllRecipes : Decoder (Dict String (Maybe RecipeJson))
decodeAllRecipes = dict (maybe decodeRecipe)

type alias RecipeJson = {
  name : String,
  result : Maybe String,
  results : Maybe (List ComplexResult),
  ingredients : List IngredientJson,
  recipeType : String,
  energy_required : Maybe Float
}

type alias TypedIngredientJson = { name : String, recipeType : String, amount : Float }
type IngredientJson =
  Basic (String, Float) | Typed TypedIngredientJson

-- UPDATE
type Msg = JSONChanged String
update : Msg -> String -> String
update (JSONChanged content) oldContent = content

-- VIEW
isNothing : Maybe a -> Bool
isNothing x = case x of
  Nothing -> True
  _ -> False

view content =
  let decodeResult = Json.decodeString decodeAllRecipes content
      displayText  = case decodeResult of
        Ok ob -> Basics.toString ob
        Err message -> message
      noGoodOnes = case decodeResult of
        Ok ob -> List.map (\(name, res) -> name) (List.filter (\(name, res) -> isNothing res) (Dict.toList ob))
        _ -> []
  in 
    div []
      [ input [ placeholder "JSON in", onInput JSONChanged, myStyle ] []
      , pre [ myStyle ] [ text displayText ]
      , Html.h1 [ myStyle ] [ text "errors" ]
      , pre [ myStyle ] [ text (Basics.toString noGoodOnes) ]
      ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
--    , ("text-align", "center")
    ]
