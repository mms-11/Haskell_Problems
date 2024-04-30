
type Comando = String
type Valor = Int


executar :: [(Comando, Valor)] -> Int ->Int
executar [] resultado = resultado 
executar ((c1, v1):rest) resultado
    | c1 == "Soma" =  executar rest (resultado + v1)
    | c1 == "Subtrai" = executar rest (resultado- v1)
    | c1 == "Multiplica" = executar rest (resultado* v1)
    | c1 == "Divide" && v1 == 0 = -666
    | c1 == "Divide" = executar rest ( resultado `div` v1)
    
    
executa :: [(Comando, Valor)] -> Int
executa (a:as) = executar (a:as) 0
    
main = do
  a <- getLine
  let result = executa (read a)
  print result