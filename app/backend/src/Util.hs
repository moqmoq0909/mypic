module Util ((|>), uncurry3) where

{-
Haskellにおけるinfixl 1 |>の定義は、カスタム演算子|>を定義しているもので、特に関数適用のためによく使われる「パイプライン演算子」として知られています。この演算子は、多くの関数型言語やライブラリで見られ、関数適用の順序を直感的にするために使用されます。

解説
(|>)は、左辺の値aを右辺の関数fに適用するカスタム演算子です。つまり、a |> fはf aと同等になります。これにより、関数適用を左から右へと読むことができるようになります。
infixlについて
infixlは「左結合」という意味です。これは、同じ演算子が連続している場合に、左から順に適用されることを示します。例えば、a |> f |> gは(a |> f) |> gと同等になります。

1は演算子の優先順位を示しており、0から9までの範囲で指定できます(ただし、実際には10以上の優先順位を持つ演算子も存在します)。優先順位が低いほど、その演算子は他の演算子よりも後に適用されます。1は非常に低い優先順位を意味し、これによりほとんどの演算よりも|>が後に適用されるようになります。
-}
infixl 1 |>

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

-- Haskellにおけるuncurry関数のように、3つの引数を取る関数を、1つの引数を取る関数に変換する（この引数は3つの要素を持つタプル）。
-- ※uncurryはカリー化された関数を非カリー化するために使用される。
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- Maybe値をリストに変換するヘルパー関数
-- maybeToList :: Maybe a -> [a]
-- maybeToList (Just x) = [x]
-- maybeToList Nothing = []