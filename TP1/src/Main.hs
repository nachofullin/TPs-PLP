module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      -- a y vacio
      texto "a" <+> vacio ~?= texto "a",
      vacio <+> texto "a" ~?= texto "a",
      -- ab
      texto "a" <+> texto "b" ~?= texto "ab",
      -- a y nada
      texto "a" <+> texto "" ~?= texto "a",
      texto "" <+> texto "a" ~?= texto "a",
      -- a con linea y bc
      texto "a" <+> linea <+> texto "b" <+> texto "c" ~?= texto "a" <+> (linea <+> texto "bc"),

      -- asociatividad por infixr 6
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),
      (texto "a" <+> texto "b") <+> texto "c" ~?= texto "abc", 
      texto "a" <+> (texto "b" <+> texto "c")~?= texto "abc"
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      indentar 2 ((linea <+> texto "a") <+> linea) ~?= indentar 2 (linea <+> texto "a") <+> (indentar 2 linea),
      indentar 2 ((linea <+> texto "a") <+> linea) ~?= indentar 2 (linea <+> (texto "a" <+> linea)),
      indentar 2 ((linea <+> texto "a")) <+> linea ~?= indentar 2 linea <+> (texto "a" <+> linea)
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (texto "a" <+> linea <+> texto "b") ~?= "a\nb",
      
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      mostrar (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c") ~?= "a\nb\nc",
      mostrar (indentar 2 (texto "Hola" <+> linea <+> linea <+> texto "Mundo")) ~?= "Hola\n  \n  Mundo",
      mostrar (linea <+> linea <+> linea <+> linea <+> texto "Ind") ~?= "\n\n\n\nInd"
    ]

cleripe, pericles, merlina, addams, familias :: PPON
cleripe = ObjetoPP [("nombre",  cleripe), ("edad" , IntPP 3)]
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple familias ~?= False,
      pponObjetoSimple cleripe ~?= False -- ya que el mismo cleripe termina siendo compuesto, la paradoja cleripeana
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (intercalar (texto ", ") [indentar 2 (linea <+> texto "a")]) ~?= "\n  a",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      mostrar (entreLlaves [a, b <+> b]) ~?= "{\n  a,\n  bb\n}",
      mostrar (entreLlaves [a <+> linea, b, a]) ~?= "{\n  a\n,\n  b,\n  a\n}" -- ¿tendría que generar indentar 2 DE LA NADA???
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      mostrar (aplanar (linea)) ~?= " ",
      mostrar (aplanar (indentar 1 linea <+> texto "a")) ~?= " a",
      mostrar (aplanar (linea <+> texto "a" <+> linea)) ~?= " a "
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      mostrar (pponADoc cleripe) ~?= error "No puede estar la llamada al objeto dentro del objeto."
      -- no se me ocurre otro
    ]

