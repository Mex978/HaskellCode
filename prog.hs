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
listaPerfeita :: [Int] -> [Int]
listaPerfeita (x:xs)
  | isPerfeito x = [x] ++ listaPerfeita xs
  | otherwise = listaPerfeita xs

isPerfeito :: Int -> Bool
isPerfeito num
  | somatorioDivisores num 1 == num = True
  | otherwise = False

somatorioDivisores :: Int -> Int -> Int
somatorioDivisores num denominador
  | denominador >= num `div` 2 = 0
somatorioDivisores num denominador
  | num `mod` denominador == 0 = denominador + somatorioDivisores num (denominador + 1)
  | otherwise = somatorioDivisores num (denominador + 1)
---------------------------------------------------


