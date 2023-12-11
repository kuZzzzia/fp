module CPS (cbn, cbv, scanTerms) where

import Data.Char (isSpace)
import Data.Maybe (isNothing)

data Term 
        = Lambda {
            lambdaVar   :: [String],
            lambdaBody  :: [Term]
        }
        | Paren [Term]
        | Const String
        | Var   String
    deriving (Show)

cbn :: [Term] -> String
cbn = cpsToString . translateToCBN

cbv :: [Term] -> String
cbv = cpsToString . translateToCBV

cpsToString :: [Term] -> String
cpsToString []            = []
cpsToString (head : tail) = case head of
    (Const  val)       -> " "  ++ val ++ cpsToString tail
    (Var    val)       -> " "  ++ val ++ cpsToString tail
    (Paren  terms)     -> "("  ++ cpsToString terms ++ ")"  ++ cpsToString tail
    (Lambda vars body) -> " \\" ++ varsToString vars ++ "->" ++ cpsToString body ++ cpsToString tail
        where
            varsToString :: [String] -> String
            varsToString []            = []
            varsToString (head : tail) = head ++ " " ++ varsToString tail

translateToCBN :: [Term] -> [Term]
translateToCBN ts = translateToCPS (getAllVars ts) cbnTermTranslator cbnMnTranslator ts
    where 
        cbnTermTranslator :: Term -> String -> [Term]
        cbnTermTranslator t newK = [Paren 
                                    [Lambda [newK] 
                                            [t, Var newK]]]

        cbnMnTranslator   :: [[Term]] -> [String] -> [Term]
        cbnMnTranslator [tM, tN] [newK, newKK, _] = [Lambda [newK] 
                                                            (tM ++ [Paren 
                                                                    [Lambda [newKK] 
                                                                            ((Var newKK : tN) ++ [Var newK])]])]

translateToCBV :: [Term] -> [Term]
translateToCBV ts = translateToCPS (getAllVars ts) cbvTermTranslator cbvMnTranslator ts
    where 
        cbvTermTranslator :: Term -> String -> [Term]
        cbvTermTranslator t newK = [Paren 
                                    [Lambda [newK] 
                                            [Var newK, t]]]
                                                                
        cbvMnTranslator   :: [[Term]] -> [String] -> [Term]
        cbvMnTranslator [tM, tN] [newK, newKK, newKKK] = [Lambda [newK] 
                                                            (tM ++ [Paren 
                                                                    [Lambda [newKK] 
                                                                            (tN ++ [Paren 
                                                                                    [Lambda [newKKK] 
                                                                                            [Var newKK, Var newKKK, Var newK]]])]])]                                                                              

translateToCPS :: [String] -> (Term -> String -> [Term]) -> ([[Term]] -> [String] -> [Term]) -> [Term] -> [Term]
translateToCPS varDict termTranslator mnTranslator = go 0
    where
        go :: Int -> [Term] -> [Term]
        go kNum terms = let
            (newK, nextK) = getNewK kNum varDict
            in case reverse terms of
                []            -> []
                [head]        -> case head of
                    (Const  val)       -> [Paren [Lambda [newK] 
                                                         [Var newK, Const val]]]
                    (Paren  terms)     -> [Paren (go nextK terms)]
                    (Var    val)       -> termTranslator (Var val) newK
                    (Lambda vars body) -> translateLambdaToCBN nextK vars body
                        where
                            translateLambdaToCBN :: Int -> [String] -> [Term] -> [Term] 
                            translateLambdaToCBN kNum vars body = let
                                (newK, nextK) = getNewK kNum varDict
                                in case vars of
                                    []            -> []
                                    [head]        -> [Lambda [newK] 
                                                             [Var newK, 
                                                              Paren 
                                                                [Lambda [head] 
                                                                        (go (nextK+1) body)]]]
                                    (head : tail) -> [Lambda [newK] 
                                                             [Var newK, 
                                                              Paren 
                                                                [Lambda [head]
                                                                        (translateLambdaToCBN (nextK+1) tail body)]]]
                (head : tail) -> let
                    (newKK, nextKK) = getNewK (nextK+1) varDict
                    (newKKK, nextKKK) = getNewK (nextKK+1) varDict
                    in mnTranslator [go nextKKK (reverse tail), go (nextKKK+1) [head]] [newK, newKK, newKKK]

getNewK :: Int -> [String] -> (String, Int)
getNewK k dict = let 
    nextK = go k dict 
    in ('k': show nextK, nextK)
    where 
        go :: Int -> [String] -> Int
        go kk dict
            | ('k': show kk) `elem` dict = go (kk+1) dict
            | otherwise                  = kk

getAllVars :: [Term] -> [String]
getAllVars (head:tail) = case head of
    (Lambda vars terms) -> getAllVars terms ++ getAllVars tail ++ vars
    (Paren  terms)      -> getAllVars terms ++ getAllVars tail
    _                   -> [] 
getAllVars _           = []

scanTerms :: String -> [Term]
scanTerms ts = let 
    res = go [] Nothing $ scanTokens ts -- first argument - vars in the scope
    in if null $ snd res then fst res else [] 
    where 
        go :: [String] -> Maybe Term -> [String] -> ([Term], [String])
        go scope currTerm tokens = case tokens of
            []                        -> case currTerm of
                Nothing -> ([], [])
                Just x  -> ([x], [])
            (head : tail)
                |  head == "("
                || head == "$" 
                || head == "."        -> let
                    (parsedParen,  restTokens) = go scope (Just$ Paren []) tail
                    (parsedRest,  restTokens') = go scope Nothing restTokens
                    in (addTerm(Paren parsedParen) currTerm : parsedRest, restTokens')
                |  head == ")"        -> case currTerm of
                    Nothing -> ([], tail)
                    Just x -> ([x], tail)
                |  head == "\\"       -> let
                    (parsedVars, restTokens) = scanLambdaVars tail
                    in go (scope++parsedVars) (Just$ Lambda parsedVars []) restTokens
                |  isNothing currTerm -> let
                    (parsed, restTokens) = go scope Nothing tail
                    in (addTerm (makeVarOrConst scope head) currTerm : parsed, restTokens)
                |  otherwise          -> go scope (Just$ addTerm (makeVarOrConst scope head) currTerm) tail

addTerm :: Term -> Maybe Term -> Term
addTerm term Nothing = term
addTerm termToAppend (Just x) = case x of
    (Lambda var terms) -> Lambda var (terms ++ [termToAppend])
    (Paren  terms)     -> Paren (terms ++ [termToAppend]) 

makeVarOrConst :: [String] -> String -> Term
makeVarOrConst scope identifier
    | identifier `elem` scope = Var identifier
    | otherwise               = Const identifier                       

scanLambdaVars :: [String] -> ([String], [String])
scanLambdaVars = go []
    where
        go :: [String] -> [String] -> ([String], [String])
        go vars tokens = case tokens of
            []           -> (vars, [])
            ("->": tail) -> (vars, tail)
            (var : tail) -> go (vars++[var]) tail  
         
-- Token = "->" | "$" | "." | "(" | ")" | "\\" | [Char] (no whitespaces)
scanTokens :: String -> [String]
scanTokens = go Nothing
    where
        go :: Maybe String -> String -> [String]
        go parsed string = case string of
            []                  -> case parsed of
                Nothing -> []
                Just x  -> [x]
            ('-' : '>' : tail)  -> go parsed [] ++ ["->"] ++ go Nothing tail
            (head : tail)
                |  head == '\\' 
                || head == '(' 
                || head == ')' 
                || head == '$'
                || head == '.'  -> go parsed [] ++ [[head]] ++ go Nothing tail
                |  isSpace head -> go parsed [] ++ go Nothing tail
                |  otherwise    -> case parsed of
                    Nothing -> go (Just [head]) tail
                    Just x  -> go (Just $ x ++ [head]) tail     

