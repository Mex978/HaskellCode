-- Questão 1 (Letra a)
toBinario :: Int -> [Int]
toBinario 0 = [0]
toBinario 1 = [1]
toBinario n
  | n `mod` 2 == 1 = toBinario (n `div` 2) ++ [1]
  | n `mod` 2 == 0 = toBinario (n `div` 2) ++ [0]
---------------------------------------------------

-- Questão 1 (Letra b)
toDecimal :: [Int] -> Int
toDecimal [0] = 0
toDecimal lista = toDecimalAux lista (length(lista) - 1)

toDecimalAux :: [Int] -> Int -> Int
toDecimalAux [] _ = 0
toDecimalAux (x:xs) n = 
  (2^n) * x + toDecimalAux (xs) (n - 1)
---------------------------------------------------

-- Questão 2
fun :: [Int] -> [(Int, Int)]
fun [] = []
fun (x : xs) = [(x, tam_ciclo_n x)] ++ fun xs

tam_ciclo_n :: Int -> Int
tam_ciclo_n num
  | num == 1 = 1
  | num `mod` 2 == 0 = 1 + tam_ciclo_n (num `div` 2)
  | otherwise = 1 + tam_ciclo_n ((3 * num) + 1)
---------------------------------------------------

-- Questão 3
funIntercalar :: [String] -> [String]
funIntercalar lista =
  [getLetterIndex_n index lista | index <- [0..length(lista!!0) - 1] ]

getLetterIndex_n :: Int -> [String] -> [Char]
getLetterIndex_n _ [] = []
getLetterIndex_n index (x:xs) =
  [x!!index] ++ getLetterIndex_n index (xs)
---------------------------------------------------

-- Questão 4
fun4 :: [t] -> [[t]]
fun4 lista = [(sub_list_1 lista), (sub_list_2 lista)]

sub_list_1 :: [t] -> [t]
sub_list_1 lista =
  [lista!!n | n <- [0..length(lista) - 1], (n + 1) `mod` 2 /= 0]

sub_list_2 :: [t] -> [t]
sub_list_2 lista =
  [lista!!n | n <- [0..length(lista) - 1], (n + 1) `mod` 2 == 0]
---------------------------------------------------

-- Questão 6
intersecao :: [Int] -> [Int] -> [Int]
intersecao [] _ = []
intersecao (x:xs) (lista2)
  | pertence x lista2 = [x] ++ intersecao xs lista2
  | otherwise = intersecao xs lista2
  

pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence num (x:xs)
  | x == num = True
  | otherwise = pertence num xs
---------------------------------------------------

-- Questão 10
obterFatores :: Int -> [Int]
obterFatores num = [x | x <- [1 .. (num `div` 2)], (num `mod` x) == 0]

isPerfeito :: Int -> Bool
isPerfeito num
    | (sum (obterFatores num) == num ) = True
    | otherwise = False

listaPerfeita :: [Int] -> [Int]
listaPerfeita lista = filter isPerfeito lista
---------------------------------------------------

-- Questão 11
data Classe = Aluno String Float Float Float

calcMedias :: [Classe] -> [(String, Float)]
calcMedias [] = []
calcMedias (x:xs) = [getMedia x] ++ calcMedias xs

getMedia :: Classe -> (String, Float)
getMedia (Aluno nome n1 n2 n3) =
  (nome, ((n1 + n2 + n3) / 3.0))

---------------------------------------------------

-- Questão 12
maxValue :: Int -> Int -> Int
maxValue a b
  | a >= b = a 
  | otherwise = b

mmc :: Int -> Int -> Int
mmc a b = mmcAux a b (maxValue a b)

mmcAux :: Int -> Int -> Int -> Int
mmcAux a b n
  | n `mod` a == 0, n `mod` b == 0 = n
  | otherwise = mmcAux a b (n + 1)

---------------------------------------------------

-- Questão 14
bolha :: [Int] -> [Int]
bolha [] = []
bolha [x] = [x]
bolha (a:b:x) 
  | a <= b = a:bolha (b:x)
  | otherwise = b: bolha (a:x)
bubble :: [Int] -> Int -> [Int]
bubble lista n
  | n == (length lista) = lista
  | otherwise = bubble (bolha lista) (n+1)
bubbleSort :: [Int] -> [Int]
bubbleSort lista = 
  bubble lista 0
---------------------------------------------------