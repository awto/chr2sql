module CHR2.AST.Untyped where

import qualified Data.Map as M

data Ty = IntTy
        | FloatTy
        | StringTy
        | UserTy String [(String,[(Maybe String,Ty)])]
        deriving (Eq,Show,Ord)

-- TODO: should be a dynamic value with class
data Const = I Int
           | F Float
           | S String
           deriving (Eq,Ord,Show)

data Term = Var String
          | Const Const
          | Ftr String [Term]
          deriving (Eq, Ord, Show)

type CTypes = M.Map String [Ty]

type C = (String, [Term])

data Rule = Rule {
      ruleName :: Maybe C,
      ruleHead :: [C],
      ruleBody :: [[C]]
    } deriving (Eq,Ord,Show)
