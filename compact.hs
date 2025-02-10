import Data.Binary (Binary, decode)
import Data.ByteString.Char8 qualified as B
import Data.Char (chr, intToDigit, ord)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Sequence (Seq (Empty))
import Data.Text qualified as T
import Debug.Trace
import Numeric
import System.Environment (getArgs)

-- Definição do caractere de nós que não são folhas
-- Este caractere não pode estar presente no texto para compressão
char :: Char
char = '+'

-- Definição da árvore Binária
data BinaryTree a
  = Empty
  | Node (BinaryTree a) a (BinaryTree a)
  | Leaf a
  deriving (Show, Read)

----FUNÇÕES-AUXILIARES---------------------------------------------------
-- Reutilizadas das atividades anteriores
search list elm
  | null list = False
  | head list == elm = True
  | otherwise = search (tail list) elm

remove list elm
  | null list = []
  | head list == elm = remove (tail list) elm
  | otherwise = head list : remove (tail list) elm

intersect [] _ = []
intersect (x : xs) (y : ys)
  | search (y : ys) x = x : intersect xs (remove (y : ys) x)
  | otherwise = intersect xs (y : ys)

-- Buscar valor de um nó
getValue :: BinaryTree (Int, Char) -> (Int, Char)
getValue Main.Empty = (0, ' ')
getValue (Node _ value _) = value
getValue (Leaf value)
  | null value = (0, ' ')
  | otherwise = value

-- Inserir nó na fila de árvores
insertNode :: BinaryTree (Int, Char) -> [BinaryTree (Int, Char)] -> [BinaryTree (Int, Char)]
insertNode node [] = [node]
insertNode node (x : xs)
  | fst (getValue node) < fst (getValue x) = node : x : xs
  | otherwise = x : insertNode node xs

-- Verificar se nó é folha
isLeaf :: BinaryTree (Int, Char) -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

getLeft :: BinaryTree (Int, Char) -> BinaryTree (Int, Char)
getLeft (Node left _ _) = left

getRight :: BinaryTree (Int, Char) -> BinaryTree (Int, Char)
getRight (Node _ _ right) = right

binToInt :: String -> Int
binToInt (x : xs)
  | null xs && x == '0' = 0
  | null xs && x == '1' = 1
  | x == '1' = 2 ^ length xs + binToInt xs
  | otherwise = binToInt xs

-- Conversão de caractere para binário
charToBin :: String -> String
charToBin text = intToBin (charToInt text)

-- Conversão de caractere para inteiro
charToInt :: String -> [Int]
charToInt (x : xs)
  | null xs = [fromEnum x]
  | otherwise = fromEnum x : charToInt xs

-- Conversão de Lista de inteiro para String de binário
intToBin :: [Int] -> String
intToBin (x : xs)
  | null xs = numToBin x
  | otherwise = numToBin x ++ intToBin xs

-- Conversão de inteiro para binário
numToBin :: Int -> String
numToBin n = replicate (8 - length binary) '0' ++ binary
  where
    binary = showIntAtBase 2 intToDigit n ""

incompByte :: String -> String
incompByte code = charToBin (replicate (8 - length code) '0' ++ code)

compactTree :: String -> String
compactTree string = T.unpack (T.replace (T.pack ") (") (T.pack ")(") (T.replace (T.pack "(Leaf ") (T.pack "(L") (T.replace (T.pack "(Node ") (T.pack "(N") (T.pack string))))

descompTree :: String -> [Char]
descompTree string = T.unpack (T.replace (T.pack ")(") (T.pack ") (") (T.replace (T.pack "(L") (T.pack "(Leaf ") (T.replace (T.pack "(N") (T.pack "(Node ") (T.pack string))))

--------------------------------------------------------------------

---FUNÇÃO PRINCIPAL------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let nomeArquivo = head args
  let inComp = last args
  if inComp == "E"
    then compress nomeArquivo
    else
      if inComp == "D"
        then decompress nomeArquivo
        else putStrLn "Parâmetro inválido. Use E para encriptação/compressão e D para descompressão."

-- content <- readFile "Texto.txt"
-- let huffTree = huffmanTree (huffmanQueue (sortTable (freqTable content)))
-- let huffDic = dicHuffman huffTree
-- let codiFile = codificar (content, huffDic)
-- let compacFile = compactar codiFile
-- -- Convertendo árvore de Huffman para String
-- let huffTreeData = show huffTree
-- -- Número de bits do texto original
-- let utilBits = chr (8 - mod (length codiFile) 8)
-- -- Escrevendo árvore de Huffman e texto compactado em arquivo
-- writeFile "compactado1.txt" (huffTreeData ++ "\n" ++ [utilBits] ++ "\n" ++ compacFile)

-- -- Lendo arquivo compactado
-- content <- readFile "compactado1.txt"
-- -- separar árvore do texto compactado
-- let (treeStr, subCont) = break (== '\n') content
-- -- reconstruir árvore da string
-- let hTree = read treeStr :: BinaryTree (Int, Char)
-- -- separar número de bits do texto original
-- let (readBits, encoded) = break (== '\n') (tail subCont)
-- -- Obtendo código binário compactado
-- let auxDescomp = descompactar (tail encoded)
-- let utilBits = ord (head readBits)
-- let codiDescomp = take (length auxDescomp - utilBits) auxDescomp
-- -- Decodificando texto compactado
-- let decompText = decodificar (codiDescomp, hTree)
-- putStrLn decompText

------------------------------------------------------------------

-- FUNÇÕES-DE-COMPRESSÃO------------------------------------------------

compress dsArquivo = do
  byteString <- B.readFile dsArquivo
  let content = B.unpack byteString
  let (nomeArquivo, tpArquivo) = break (== '.') dsArquivo
  let huffTree = huffmanTree (huffmanQueue (sortTable (freqTable content)))
  let huffDic = dicHuffman huffTree
  let codiFile = codificar (content, huffDic)
  let compacFile = compactar codiFile
  -- Convertendo árvore de Huffman para String
  let huffTreeString = show huffTree
  let huffTreeData = compactTree huffTreeString
  -- Número de bits do texto original
  let utilBits = chr (8 - mod (length codiFile) 8)
  -- Escrevendo árvore de Huffman e texto compactado em arquivo
  writeFile (nomeArquivo ++ ".huff") (huffTreeData ++ "\n" ++ [utilBits] ++ "\n" ++ tpArquivo ++ "\n" ++ compacFile)

decompress dsArquivo = do
  content <- readFile dsArquivo
  let nomeArquivo = take (length dsArquivo - 5) dsArquivo
  -- separar árvore do texto compactado
  let (treeComp, subCont) = break (== '\n') content
  let treeFull = descompTree treeComp
  -- reconstruir árvore da string
  let hTree = read treeFull :: BinaryTree (Int, Char)
  -- separar número de bits do texto original
  let (readBits, aux) = break (== '\n') (tail subCont)
  -- Separa tipo de arquivo
  let (tpArquivo, encoded) = break (== '\n') (tail aux)
  -- Obtendo código binário compactado
  let auxDescomp = descompactar (tail encoded)
  -- Número de bits do texto original
  let utilBits = ord (head readBits)
  let codiDescomp = take (length auxDescomp - utilBits) auxDescomp
  -- Decodificando texto compactado
  let decompText = decodificar (codiDescomp, hTree)
  let decompTextByte = B.pack decompText
  B.writeFile (nomeArquivo ++ "-descomp" ++ tpArquivo) decompTextByte

-- CONSTRUIR TABELA DE FREQUÊNCIA ORDENADA--------------------------------------
-- Construir tabela de frequência das letras da String de entrada
freqTable content = [(length $ filter (== c) content, c) | c <- content `intersect` [chr i | i <- [0 .. 256]]]

-- Construir fila ordenada pela frequência dos caracteres
sortTable freqTable = sortBy (comparing fst) [(c, f) | (c, f) <- freqTable]

-- Converter elementos da fila ordenada em nós
huffmanQueue (x : xs) = Leaf x : huffmanQueue xs
huffmanQueue _ = []

--------------------------------------------------------------------------

-- CONSTRUIR ÁRVORE DE HUFFMAN-----------------------------------------
-- huffmanTree :: [BinaryTree (Int, Char)] -> BinaryTree (Int, Char)
huffmanTree [x] = Node Main.Empty (fst (getValue x), char) x
huffmanTree (x : y : xs)
  | getValue y == (0, ' ') = x
  | null xs = Node x (fst (getValue x) + fst (getValue y), char) y
  | otherwise = huffmanTree (insertNode (Node x (fst (getValue x) + fst (getValue y), char) y) xs)

--------------------------------------------------------------------------

-- CONSTRUIR DICIONÁRIO DE HUFFMAN--------------------------------------
-- Função que dada uma árvore de Huffman, retorna um dicionário com os códigos de cada caractere
dicHuffman :: BinaryTree (Int, Char) -> [(String, Char)]
dicHuffman tree = dicHuffman' tree ""

-- Iterações
dicHuffman' :: BinaryTree (Int, Char) -> String -> [(String, Char)]
dicHuffman' node code
  | getValue node == (0, ' ') = []
  | isLeaf node = [(code, snd (getValue node))]
  | otherwise = dicHuffman' (getLeft node) (code ++ "0") ++ dicHuffman' (getRight node) (code ++ "1")

--------------------------------------------------------------------------

-- CODIFICAÇÃO---------------------------------------------------------
-- Codificar texto Entrada: (texto, dicionário de Huffman)
codificar :: (String, [(String, Char)]) -> String
codificar (text, dic) = concat [code | c <- text, code <- [fst x | x <- dic, snd x == c]]

--------------------------------------------------------------------------

-- DECODIFICAÇÃO---------------------------------------------------------
-- Decodificar texto Entrada: (texto comprimido, árvore de Huffman)
decodificar :: (String, BinaryTree (Int, Char)) -> String
decodificar (text, htree)
  | null text = ""
  | otherwise = fst (decodificar' text htree) : decodificar (snd (decodificar' text htree), htree)

decodificar' :: String -> BinaryTree (Int, Char) -> (Char, String)
decodificar' text node
  | null text = (' ', "")
  | isLeaf node = (snd (getValue node), text)
  | otherwise = decodificar' (tail text) (if head text == '0' then getLeft node else getRight node)

--------------------------------------------------------------------------

-- Passa String binária para String de caracteres compactados
compactar :: String -> String
compactar code
  | null code = ""
  --   | length code < 8 = incompByte code
  | otherwise = chr (binToInt (take 8 code)) : compactar (drop 8 code)

-- Passa string de caracteres compactados para string binária
descompactar :: String -> String
descompactar (x : xs)
  | null xs = charToBin [x]
  | otherwise = charToBin [x] ++ descompactar xs

------------------------------------------------------------------------------------------------