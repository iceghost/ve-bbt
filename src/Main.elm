port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (class, href, id, target)
import Html.Events exposing (onClick, onInput)
import Icon
import Process
import Table exposing (Problem(..), Row(..))
import Task


port output : String -> Cmd msg


port sendCopy : String -> Cmd msg


type Msg
    = GotInput String
    | Copy
    | ResetCopy


type CopyState
    = Idle
    | Copied


type alias Model =
    { rawInput : String
    , output : String
    , copy : CopyState
    }


sampleTable : List ( String, String )
sampleTable =
    [ ( "y=x^2"
      , "x -1 . 0 . 1\ny' . + 0 - .\ny +vc xuống 0 lên +vc"
      )
    , ( "y=1/x"
      , "x -vc . . 0 . . +vc\nf' . + . || . +\nf 0 lên +vc ||-4 -vc lên 0"
      )
    ]


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( Model "" "" Idle
                , Task.perform
                    (\_ ->
                        GotInput
                            (sampleTable
                                |> List.head
                                |> Maybe.withDefault ( "", "" )
                                |> Tuple.second
                            )
                    )
                    (Process.sleep 0)
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInput newInput ->
            ( { model
                | rawInput = newInput
                , output = newInput |> outputPipe
              }
            , output (newInput |> outputPipe)
            )

        Copy ->
            ( { model | copy = Copied }
            , Cmd.batch
                [ Task.perform
                    (\_ ->
                        ResetCopy
                    )
                    (Process.sleep 2000)
                , sendCopy model.output
                ]
            )

        ResetCopy ->
            ( { model | copy = Idle }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "flex flex-col justify-between font-body border-t-8 border-indigo-800", class "min-h-screen w-screen bg-orange-300" ]
        [ main_ [ class "mx-auto w-auto px-10 bg-white py-4" ]
            [ h1 [ class "text-indigo-900 text-3xl font-bold text-center" ]
                [ text "Tạo bảng biến thiên bằng LaTeX" ]
            , h2 [ class "font-bold text-xl text-indigo-700" ]
                [ text "Nhập vào đây" ]
            , p [ class "p-1" ]
                [ text "... hoặc chọn bảng mẫu"
                , select
                    [ class "border rounded-lg border-purple-200"
                    , onInput GotInput
                    ]
                  <|
                    List.map (\( label, content ) -> option [ Attr.value content ] [ text label ])
                        sampleTable
                ]
            , textarea
                [ class "bg-purple-100 w-full h-48 p-2 shadow-inner rounded-lg"
                , Attr.value model.rawInput
                , onInput GotInput
                ]
                []
            , h2 [ class "font-bold text-xl text-indigo-700" ]
                [ text "Kết quả" ]
            , div [ class "flex justify-center mt-2", id "output" ]
                []
            , a
                [ class "mx-auto bg-purple-600 text-white w-32 p-1 rounded-full flex"
                , class "items-center justify-center mt-2 border border-purple-600 hover:bg-white"
                , class "hover:text-purple-600 transition-colors duration-100 ease-in-out"
                , href
                    ("https://latex.codecogs.com/png.latex?\\dpi{300} \\bg_white " ++ model.output)
                , target "_blank"
                ]
                [ span []
                    [ text "Tải xuống"
                    ]
                , div [ class "ml-1 w-5 h-5" ] [ Icon.download ]
                ]
            , button
                [ class "mx-auto bg-orange-500 text-white w-32 p-1 rounded-full flex"
                , class "items-center justify-center mt-2 border border-orange-500 hover:bg-white"
                , class "hover:text-orange-500 transition-colors duration-100 ease-in-out"
                , onClick Copy
                ]
                [ span []
                    [ text
                        (if model.copy == Idle then
                            "Copy mã"

                         else
                            "Đã copy!"
                        )
                    ]
                , div [ class "ml-1 w-5 h-5" ] [ Icon.copyToClipboard ]
                ]
            ]
        , footer
            [ class "grid grid-cols-1 divide-y divide-indigo-300 p-2 grid-rows-2"
            , class "bg-indigo-800 text-white"
            ]
            [ span [ class "mx-auto py-2" ]
                [ a
                    [ class "flex items-center space-x-1"
                    , href "https://diendan.hocmai.vn/threads/ve-bang-bien-thien-tren-dien-dan.813437/"
                    , target "_blank"
                    ]
                    [ span []
                        [ text "Hướng dẫn" ]
                    , div [ class "ml-1 w-5 h-5" ] [ Icon.questionMark ]
                    ]
                ]
            , span [ class "mx-auto font-light italic pt-2" ]
                [ text "Made with love by "
                , a [ href "https://github.com/iceghosttth/ve-bbt" ] [ text "iceghost" ]
                ]
            ]
        ]



-- FORMATTER


outputPipe : String -> String
outputPipe input =
    input
        |> String.lines
        |> List.map
            (Table.parse
                >> handleError
                >> Table.rowToMatrix
            )
        |> format


handleError : Result Problem Row -> Row
handleError res =
    case res of
        Ok row ->
            row

        Err (StuckAt col row) ->
            Table.singleton <| "\\text{Kiểm tra lại hàng này ở ký tự thứ " ++ String.fromInt col ++ "...}"

        Err (ExpectingBracket col _) ->
            Table.singleton <| "\\text{Kiểm tra lại mấy cái ngoặc hàng này...}"

        Err (BadString prob) ->
            Table.singleton <| prob


format : List (List (List String)) -> String
format list =
    let
        max =
            List.map (List.head >> Maybe.withDefault [] >> List.length) list |> List.maximum |> Maybe.withDefault 0
    in
    "\\begin{array}{c|"
        ++ String.repeat (max - 1) "c"
        ++ "}\n"
        ++ (list |> List.map (List.map (String.join "&") >> String.join "\\\\\n") |> String.join "\\\\\n\\hline\n")
        ++ "\n\\end{array}"
