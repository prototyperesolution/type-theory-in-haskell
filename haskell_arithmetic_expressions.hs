--Defines all possible terms and an exception
data Term = 
    TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    | Exception
    deriving (Eq, Show)

--Checks whether they're numerical
isnumeric :: Term -> Bool
isnumeric TmZero = True
isnumeric (TmSucc val) = isnumeric val
isnumeric (TmPred val) = isnumeric val
isnumeric _ = False

--Checks whether they're any val (bool or numeric)
isval :: Term -> Bool
isval TmTrue = True
isval TmFalse = True
isval term = isnumeric term

--Small step evaluation
eval :: Term -> Term
eval t =
    case t of
        (TmIf TmTrue t2 t3) -> t2
        (TmIf TmFalse t2 t3) -> t3
        (TmIf t1 t2 t3) ->
            let t1' = eval t1
            in (TmIf t1' t2 t3)
        (TmSucc t1) ->
            let t1' = eval t1
            in (TmSucc t1')
        (TmPred t1) ->
            case isnumeric t1 of
                True ->
                    case t1 of
                        TmZero -> TmZero
                        TmSucc t2 -> t2
                        _ -> TmPred (eval t1)
                False -> Exception
        (TmIsZero t1) ->
            case isnumeric t1 of
                True ->
                    case t1 of
                        TmSucc t2 -> TmFalse
                        TmZero -> TmTrue
                        _ -> TmIsZero (eval t1)
                False -> Exception

eval1 :: Term -> Term
eval1 t1 = if eval t1 == Exception then t1 else eval1 (eval t1)
