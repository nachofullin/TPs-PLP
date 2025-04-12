module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico (ObjetoPP _) = False
pponAtomico (TextoPP _) = True
pponAtomico (IntPP _) = True

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP pares) = all (pponAtomico . snd) pares
pponObjetoSimple _ = False

-- intercalar (texto ", ") [texto "a", texto "b", texto "c"]
-- texto "a" <+> texto ", " <+> texto "b" <+> texto ", " <+> texto "c"
intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar sep docs = foldr1 (\x acc -> x <+> sep <+> acc) docs

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio fTexto fLinea
  where fTexto s acc = texto s <+> acc
        fLinea _ acc = texto " " <+> acc

pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s)           --show para ponerle las comillas y escapar caracteres especiales
pponADoc (IntPP i) = texto (show i)             --show para obtener el numero como un String
pponADoc (ObjetoPP pares) | pponObjetoSimple (ObjetoPP pares) = texto "{ " <+> intercalar (texto ", ") (map paresADoc pares) <+> texto " }"
                          | otherwise = entreLlaves (map paresADoc pares)
  where paresADoc (clave, valor) = texto (show clave) <+> texto ": " <+> pponADoc valor   --tomamos lista de pares claves y valores para transformarla en lista de Docs
-- Para este ejercico estamos usando recursion estructural ya que nos basamos en el dato PPON,
-- aplicando la funcion recursivamente a sus componentes (los valores de los pares de ObjetoPP).
-- Hacemos la recursion sobre el dato para llegar a nuestro casos bases de TextoPP e IntPP para devolver un valor fijo.
-- En el caso recursivo hacemos llamadas recursivas sobre partes del tipo PPON, sin manipular esas partes directamente.
