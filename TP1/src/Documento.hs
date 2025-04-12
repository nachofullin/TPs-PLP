module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio


-- foldDoc = error "PENDIENTE: Ejercicio 1"
--   caso base para Vacio, caso para Texto, caso para Linea
foldDoc :: a -> (String -> a -> a) -> (Int -> a -> a) -> Doc -> a
foldDoc fVacio fTexto fLinea doc = case doc of
  Vacio -> fVacio
  Texto s doc -> fTexto s (rec doc)
  Linea i doc -> fLinea i (rec doc)
  where rec = foldDoc fVacio fTexto fLinea


-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 fTexto fLinea d1
  where fTexto s Vacio = Texto s Vacio    -- este esta de mas creo
        fTexto s (Texto t acc) = Texto (s ++ t) acc
        fTexto s acc = Texto s acc
        
        fLinea i acc = Linea i acc
-- si d1 es Vacio, llegamos al final del doc y concatenamos d2 al agregarlo a lo se va a ir acumulando
-- si d1 es linea vamos reconstruyendo el documento a partir de lo acumulado sin modificaciones
-- si d1 es Texto hay 3 opciones, le sique vacio o linea y no cambia nada, o le sigue texto y lo concatenamos
-- Invariante:
-- En fTexto nunca modificamos el contenido de los String, solo reconstruimos la estructura original 
-- y contcatenamos cuando hay 2 o mas textos seguidos.
-- Entonces no pueden ser vacio ("") ni contener saltos de linea (/n) que los harian invalidos
-- En fLinea tampoco modiicamos los enteros i, dado que el documento original era válido, estos seran validos y mayores a 0
-- En el caso base Vacio, insertamos d2, que ya es un Doc válido porque no lo modificamos
-- En resumen, se preserva la validez de Doc porque:
-- No se introducen strings inválidos.
-- No se alteran los enteros i.
-- Solo se reestructura sin romper las condiciones de los constructores.

indentar :: Int -> Doc -> Doc
indentar n = foldDoc vacio fTexto fLinea
  where fTexto s acc = Texto s acc
        fLinea i acc = Linea (i+n) acc
-- Invariante:
-- Solamente se esta modificando los valores de i de los constructores Linea al hacer fLinea.
-- Dado que solo reemplazamos por Linea (i + n), y n > 0, entonces el nuevo valor siempre es mayor o igual que cero
-- por ende sigue siendo valido que i >= 0.
-- Con fTexto no se altera nada y en el caso vacio devolvemos vacio por lo que no hay inconvenientes ahi, 
-- simplemente rearmamos la estructura Doc que ya era valida, haciendo la nueva valida tambien.

mostrar :: Doc -> String
mostrar = foldDoc "" fTexto fLinea
  where fTexto s acc = s ++ acc
        fLinea i acc = "\n" ++ replicate i ' ' ++ acc

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
