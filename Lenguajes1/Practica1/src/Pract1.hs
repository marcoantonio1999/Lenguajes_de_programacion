module Practica1 where
    data Instruction = I Int | B Bool
                    | ADD | AND | DIV | Eq | EXEC | GET| Gt
                    | Lt
                    | MUL | NOT | POP | REM | SEL | SUB | SWAP
                    | ES [ Instruction ]
                    deriving Show

    data Expr = N Int | T | F
            | Succ Expr | Pred Expr
            | Expr :+ Expr | Expr :- Expr
            | Expr :* Expr | Expr :/ Expr | Expr :% Expr | Not Expr | Expr :& Expr | Expr :| Expr
            | Expr :> Expr | Expr :< Expr | Expr := Expr
            | Expr :^ Expr | Max Expr Expr | Min Expr Expr
            | Fact Expr 
            deriving Show


    -- Pila de valores 
    type Stack = [ Instruction ]
    --  Secuencia de valores en la pila de valores
    type Program = [ Instruction ]

    {-Función que realiza las operaciones de las
    instrucciones aritméticas (add, div, mul, rem, sub). Los primeros dos
    argumentos corresponden a los operandos, y el tercero al operador. La
    función devuelve una literal entera.
    -}
    arithOperation :: Instruction -> Instruction -> Instruction -> Instruction
    arithOperation (I a) (I b) ADD = I (a + b)
    arithOperation (I a) (I b) MUL = I (a * b)
    arithOperation (I a) (I b) SUB = I (a - b)
    arithOperation (B a) _ _ = error "Tipo de dato incompatible"
    arithOperation _ (B b) _ = error "Tipo de dato incompatible"
    arithOperation _ (I 0) DIV = error "Division entre cero"
    arithOperation (I 0) _ DIV = error "Division entre cero"
    arithOperation (I a) (I b) DIV = I ( div a b)
    arithOperation (I a) (I b) REM = I (a `rem`b )

    {-  Función que realiza las operaciones de las instrucciones booleanas binarias (and). Los primeros dos argumentos cor-
     responden a los operandos, y el tercero al operador. La función devuelve una literal booleana.-}
    bboolOperation :: Instruction -> Instruction -> Instruction -> Instruction
    bboolOperation (I a) _ AND = error "Tipo de dato incompatible"
    bboolOperation _ (I a) AND = error "Tipo de dato incompatible"
    bboolOperation (B a) (B b) AND
        | a == True && b == True = (B True)
        | otherwise = (B False)
    bboolOperation _ _ _ = error "no reconocible"

    {-Función que realiza las operaciones de las
    instrucciones booleanas unarias (not). El primer argumento corresponde
    al operando, y el segundo al operador. La función devuelve una literal
    booleana.-}
    uboolOperation :: Instruction -> Instruction -> Instruction
    uboolOperation (I a) NOT = error "Tipo de dato incompatible"
    uboolOperation (B a) NOT
        | a == True = (B False)
        | otherwise = (B True)
    uboolOperation _ _ = error "no reconocible"

    {-Función que realiza las operaciones de las

    instrucciones relaciones (eq, gt, lt). Los primeros dos argumentos cor-
    responden a los operandos, y el tercero al operador. La función devuelve
    una literal booleana.-}

    relOperation :: Instruction -> Instruction -> Instruction -> Instruction
    relOperation (I a) (I b) Eq = B (a == b)
    relOperation (I a) (I b) Gt = B (a > b)
    relOperation (I a) (I b) Lt = B (a < b)
    relOperation (B a) _ _ = error "Tipo de dato incompatible"
    relOperation _ (B b) _ = error "Tipo de dato incompatible"
    relOperation _ _ _ = error "no reconocible"

    {-Función auxiliar para si una _Intruccion es igual a su mismo tipo-}
    auxBboolEq2 :: Instruction -> Instruction -> Bool
    auxBboolEq2 (B True)   (B True) = True
    auxBboolEq2 (B False)  (B False)= False
    auxBboolEq2 (I a)      (I b)    = a==b
    {-Lo mismo que  auxBboolEq2 solo que solo recibe una instruccion-}
    auxBboolEq :: Instruction -> Bool
    auxBboolEq (B True)  = True
    auxBboolEq (B False) = False    
    {-Función que realiza las operaciones de las
    instrucciones que alteran la pila de valores (literal entera, literal booleana,
    get, pop, sel, swap, secuencia ejecutable).-}
    stackOperation :: Stack -> Instruction -> Stack
    stackOperation xs SWAP 
        | length xs > 1 = [(xs!!1)]++[(xs!!0)]++(drop 2 xs)
        | otherwise = error "No hay suficientes elementos en la pila"
    stackOperation xs (ES ys) = [(ES ys)]++xs
    stackOperation (x:xs) POP = xs
    stackOperation [] POP = error "No hay suficientes elementos en la pila"
    stackOperation xs SEL 
        | length xs > 2 || auxBboolEq (xs!!2) = [(xs!!0)] ++ (drop 3 xs)
        | length xs > 2 = [(xs!!1)] ++ (drop 3 xs)
        | length xs < 3 = error "no hay suficientes elementos en la pila"
        | otherwise = error "el tercer elemento no es una literal booleana"  

    stackOperation xs (I a) = (I a):xs
    stackOperation xs (B a) = (B a):xs
    stackOperation ((I x):xs) GET 
        |  length ((I x):xs) < 1 = error "error la pila no tiene elementos"
        |  eleme (I x) xs == True = (I x):(eliminarElem (I x) xs)
        |  otherwise = error "el tope no se encuentra en la pila o el tope no es un numero "

        {-Función auxiliar que nos dice si una intrución esta en la pila-}
    eleme :: Instruction -> Stack -> Bool
    eleme  a []  = False
    eleme  a (x:xs) | auxBboolEq2 a x = True
                  | otherwise = eleme a xs
        {-Función auxiliar que elimina un elemento de la pila-}
    eliminarElem :: Instruction -> Stack -> Stack
    eliminarElem s [] = []
    eliminarElem s (x:xs) 
        | auxBboolEq2 s  x = xs
        | otherwise = (x:(eliminarElem s xs))
    {- Función que devuelve la secuencia de in-
    strucciones y la pila resultante de realizar la llamada a la operación exec.-}

    execOperation :: [Instruction] -> Stack -> Instruction -> ([Instruction], Stack)
    execOperation xs ((ES y):ys) EXEC = (y++xs,ys)
    execOperation _ (y:ys) _ = error "Operacion no compatible."
    execOperation _ [] _ = error "Pila vacia."

    {-Función que dada una secuencia de instruc-
    ciones y una pila de valores, obtiene la pila de valores resultante después

    ejecutar todas las instrucciones.-}

    executeProgram :: Program -> Stack -> Stack
    executeProgram [] l = l
    executeProgram ((I a):xs) l = executeProgram xs (stackOperation l (I a))
    executeProgram ((B a):xs) l = executeProgram xs (stackOperation l (B a))
    executeProgram ((ES a):xs) l = executeProgram xs (stackOperation l (ES a))
    executeProgram (EXEC:xs) ((ES a):ys) = executeProgram xs (executeProgram a ys)
    executeProgram (POP:xs) l = executeProgram xs (stackOperation l POP)
    executeProgram (GET:xs) l = executeProgram xs (stackOperation l GET)
    executeProgram (SEL:xs) l = executeProgram xs (stackOperation l SEL)
    executeProgram (SWAP:xs) l = executeProgram xs (stackOperation l SWAP)
    executeProgram (ADD:xs) ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (ADD)):ys)
    executeProgram (SUB:xs) ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (SUB)):ys)
    executeProgram (AND:xs) ((B a):((B b):ys)) = executeProgram xs ((bboolOperation (B a) (B b) (AND)):ys)
    executeProgram (DIV:xs) ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (DIV)):ys)
    executeProgram (Eq:xs)  ((I a):((I b):ys)) = executeProgram xs ((relOperation   (I a) (I b) (Eq)):ys)
    executeProgram (MUL:xs) ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (MUL)):ys)
    executeProgram (Gt:xs)  ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (Gt)):ys)
    executeProgram (Lt:xs)  ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (Lt)):ys)
    executeProgram (NOT:xs) ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (ADD)):ys)
    executeProgram (REM:xs) ((I a):((I b):ys)) = executeProgram xs ((arithOperation (I a) (I b) (REM)):ys)
    executeProgram _ _ = error "no reconocible"

    {-Función que dada una expresión aritmético-booleana,
    obtiene la secuencia de instrucciones que permite evaluarla.-}

    compile1 :: Expr-> Program
    compile1 (N a) = [I a]
    compile1 (T ) = [B True]
    compile1 (F ) = [B False]
    compile1 (Succ a)  = compile1 a  ++ [(I 1)] ++ [ADD] 
    compile1 (Pred a)  = compile1 a  ++ [(I 1)] ++ [SUB]
    compile1 ( a :+ b) = compile1 a  ++ compile1  b    ++ [ADD]   
    compile1 ( a :- b) = compile1 a  ++ compile1  b    ++ [SUB]   
    compile1 ( a :* b) = compile1 a  ++ compile1  b    ++ [MUL]   
    compile1 ( a :/ b) = compile1 a  ++ compile1  b    ++ [DIV]   
    compile1 ( a :% b) = compile1 a  ++ compile1  b    ++ [REM]   
    compile1 (Not a )  = compile1 a  ++ [NOT]
    compile1 ( a :| b) = compile1 (Not a) ++ compile1 (Not b) ++ [AND] ++ [NOT] -- ¬¬ (P or Q) = ¬ (P ^ Q)  
    compile1 ( a :< b) = compile1 a  ++ compile1  b    ++ [Lt]   
    compile1 ( a :> b) = compile1 a  ++ compile1  b    ++ [Gt]   
    compile1 ( a := b) = compile1 a  ++ compile1  b    ++ [Eq]   
    compile1 ( a :&  b)= compile1 a  ++ compile1  b    ++ [AND]
    --compile1 (Fact a ) =  
    compile1 (Max  a  b) =  compile1 a ++ compile1 b ++ compile1 a ++ compile1 b ++ [Gt] ++ [SEL]
    compile1 (Min  a  b) =  compile1 a ++ compile1 b ++ compile1 a ++ compile1 b ++ [Gt] ++ [NOT] ++ [SEL]     
    --compile1 ( a :^ b) 
      --              | a == (N 0) = (compile1 (N 1))
        --            | b == (N 1) = (compile1 (a))
          --          | otherwise = (compile1  (a :^ (executeProgram(compile1 b) []))) (N 1)))) ++ (compile1 a) ++ [MUL]
    compile1 _ = error "no reconocible"

    {-Función que dada una expresión aritmético-booleana,
    realiza la evaluación del programa resultante y devuelve una literal entera
    o una literal booleana.-}

    execute :: Expr -> [Instruction]
    execute e = (executeProgram (compile1 e) ([]) )