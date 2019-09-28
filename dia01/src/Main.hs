-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 09/2019


module Main where

dobra x = x + x
quadruplica x = dobra (dobra x)

fatorial2 n = product [1..n]

-- Exercício 01
--
-- Escreva uma função que retorne a raíz de uma equação
-- do segundo grau
raiz2Grau01 :: Floating a => a -> a -> a -> (a, a)
raiz2Grau01 a b c = ((-b + sqrt(b*b - 4 *a*c))/ (2*a), (-b - sqrt(b*b - 4 * a * c))/(2*a))

-- Exercício 02
--
-- Reescreva a função raiz2Grau utilizando where.
raiz2Grau02 :: Floating a => a -> a -> a -> (a, a)
raiz2Grau02 a b c = (x1, x2)
  where
    delta = b^2 - 4 *a*c
    a2    = 2 * a
    x1    = (-b + sqrt delta) / a2
    x2    = (-b - sqrt delta) / a2


-- Exercício 03
--
-- Utilizando condicionais, reescreva a função raiz2Grau para retornar
-- (0,0) no caso de delta negativo.  Note a mudança na assinatura.
raiz2Grau03 :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2Grau03 a b c = (x1, x2)
  where
    delta = b^2 - 4 *a*c
    a2    = 2 * a
    x1    =  if delta >= 0
             then (-b + sqrt delta) / a2
             else 0
    x2    = if delta >= 0
            then (-b - sqrt delta) / a2
            else 0


-- Exercício 04
--
-- Utilizando guards, reescreva a função raiz2Grau para retornar um
-- erro com raízes negativas. Utilize a função error
raiz2Grau04 :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2Grau04 a b c = (x1, x2)
  where
    delta = b^2 - 4 *a*c
    a2    = 2 * a
    x1    =  if delta >= 0
             then (-b + sqrt delta) / a2
             else 0
    x2    = if delta >= 0
            then (-b - sqrt delta) / a2
            else 0


-- Exercício 05
--
-- Considere o operador (&&&), simplique a definição para apenas dois
-- padrões.
(&&&) :: Bool -> Bool -> Bool
True  &&& True  = True
True  &&& False = False
False &&& True  = False
False &&& False = False


-- Exercício 06
--
-- O que a seguinte expressão retornará?
ex06 = head (tail [0..10])


-- Exercício 07
--
-- Implemente a sua versão do operador (!!) utilizando as funções
-- take/drop/head.  O nome foi mudado para (!!!) para evitar conflito
-- de nomes. Outra alternativa seria incluir no cabeçalho:
-- import Prelude hiding ((!!))
(!!!) :: [a] -> Int -> a -- Corrija os tipos
(!!!) xs n = head(drop n xs) -- Corrija os parâmetros


-- Exercício 08
--
-- Implemente a função fatorial utilizando o que aprendemos até agora.
fatorial :: a -- Corrija os tipos
fatorial = undefined -- corrija os parâmetros


-- Exercício 09
--
-- Implemente a função take. Se n <= 0 deve retornar uma lista vazia.
take' :: Int -> [a] -> [a]
take' n (x:xs)
  | n <= 0 = []
  | otherwise = 
    x : take' (n-1) xs


-- Exercício 10
--
-- Defina a função length utilizando compreensão de listas.  Dica,
-- você pode somar uma lista de 1s do mesmo tamanho da sua lista.
length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

-- Exercício 11
--
-- Utilizando a função divisores defina a função primo que retorna
-- True se um certo número é primo.
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

primo :: Int -> Bool -- Corrija o tipo
primo n = divisores n == [1,n]


-- Exercício 12
--
-- Utilizando a função pairs defina a função sorted que retorna
-- verdadeiro se uma lista está ordenada. Utilize também a função and
-- que retorna verdadeiro se todos os elementos da lista forem
-- verdadeiros.
pairs::[a]->[(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
-- sorted xs = length [1 | (x,y) <- pairs xs, x >= y] == 0 
sorted xs = and [x>=y | (x,y) <- pairs xs]



-- Exercício 13
--
-- Faça a versão caudal da função sum'
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (n:ns) = n + sum ns

sumTail :: Num a => [a] -> a
sumTail = undefined


-- Exercício 14
--
-- Crie uma função recursiva chamada insert que insere um valor x em
-- uma lista ys ordenada de tal forma a mantê-la ordenada.
insert :: Ord a => a -> [a] -> [a]
insert = undefined


-- Exercício 15
--
-- Crie uma função recursiva chamada isort que utiliza a função
-- insert para implementar o Insertion Sort.
isort :: Ord a => [a] -> [a]
isort = undefined


-- Exercício 16
--
-- Complete a função qsort que implementa o algoritmo Quicksort.
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort maiores
  where
    menores = undefined -- [a | ???]
    maiores = undefined -- [b | ???]


main :: IO ()
main = do
  putStrLn "hello world"
