{-#LANGUAGE GADTs, FlexibleContexts, PatternSynonyms, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Carnap.Languages.PureFirstOrder.Logic
        (FOLogic(..), parseFOLogic,parseForallxQL, parseFOLProof, folSeqParser, phiS, phi, tau, ss, FOLSequentCalc, DerivedRule(..))
    where

import Data.Map as M (lookup, Map)
import Data.List (intercalate)
import Text.Parsec
import Carnap.Core.Data.Util (scopeHeight)
--import Carnap.Core.Util
import Carnap.Core.Unification.Unification
--import Carnap.Core.Unification.Combination
import Carnap.Core.Unification.FirstOrder
import Carnap.Core.Unification.ACUI
import Carnap.Core.Data.AbstractSyntaxClasses
import Carnap.Core.Data.AbstractSyntaxDataTypes
import Carnap.Languages.PureFirstOrder.Syntax
import Carnap.Languages.PureFirstOrder.Parser
import qualified Carnap.Languages.PurePropositional.Logic as P
import Carnap.Calculi.NaturalDeduction.Syntax
import Carnap.Calculi.NaturalDeduction.Parser
import Carnap.Languages.ClassicalSequent.Syntax
import Carnap.Languages.ClassicalSequent.Parser
import Carnap.Languages.Util.GenericConnectives

--------------------------------------------------------
--1. FirstOrder Sequent Calculus
--------------------------------------------------------

type FOLSequentCalc = ClassicalSequentOver PureLexiconFOL

--we write the Copula schema at this level since we may want other schemata
--for sequent languages that contain things like quantifiers
instance CopulaSchema FOLSequentCalc where 

    appSchema (SeqQuant (All x)) (LLam f) e = schematize (All x) (show (f $ SeqV x) : e)
    appSchema (SeqQuant (Some x)) (LLam f) e = schematize (Some x) (show (f $ SeqV x) : e)
    appSchema x y e = schematize x (show y : e)

    lamSchema f [] = "λβ_" ++ show h ++ "." ++ show (f (SeqSV (-1 * h)))
        where h = scopeHeight (LLam f)
    lamSchema f (x:xs) = "(λβ_" ++ show h ++ "." ++ show (f (SeqSV (-1 * h))) ++ intercalate " " (x:xs) ++ ")"
        where h = scopeHeight (LLam f)

pattern SeqQuant q        = FX (Lx2 (Lx1 (Lx2 (Bind q))))
pattern SeqSV n           = FX (Lx2 (Lx1 (Lx1 (Lx4 (StaticVar n)))))
pattern SeqVar c a        = FX (Lx2 (Lx1 (Lx4 (Function c a))))
pattern SeqTau c a        = FX (Lx2 (Lx1 (Lx5 (Function c a))))
pattern SeqV s            = SeqVar (Var s) AZero
pattern SeqT n            = SeqTau (SFunc AZero n) AZero

instance Eq (FOLSequentCalc a) where
        (==) = (=*)

instance ParsableLex (Form Bool) PureLexiconFOL where
        langParser = folFormulaParser

folSeqParser = seqFormulaParser :: Parsec String u (FOLSequentCalc Sequent)

--------------------------------------------------------
--2. Classical First-Order Logic
--------------------------------------------------------

data DerivedRule = DerivedRule { conclusion :: PureFOLForm, premises :: [PureFOLForm]}
               deriving Show

data FOLogic = MP | MT  | DNE | DNI | DD   | AX 
                  | CP1 | CP2 | ID1 | ID2  | ID3  | ID4 
                  | ADJ | S1  | S2  | ADD1 | ADD2 | MTP1 | MTP2 | BC1 | BC2 | CB  
                  | UD  | UI  | EG  | ED1  | ED2  | DER DerivedRule
                  | QN1 | QN2 | QN3 | QN4
               deriving Show

-- TODO do this right
instance Eq FOLogic where
        x == y = show x == show y

ss :: PureFOLForm -> FOLSequentCalc Succedent
ss = SS . liftToSequent

sa :: PureFOLForm -> FOLSequentCalc Antecedent
sa = SA . liftToSequent

phiS n = PPhi n AZero AZero

phi n x = PPhi n AOne AOne :!$: x

tau = PT 1

-- TODO use liftSequent to clean this up
instance Inference FOLogic PureLexiconFOL where
     premisesOf MP    = [ GammaV 1 :|-: ss (phiS 1 :->: phiS 2)
                        , GammaV 2 :|-: ss (phiS 1)
                        ]
     premisesOf MT    = [ GammaV 1 :|-: ss (phiS 1 :->: phiS 2)
                        , GammaV 2 :|-: ss (PNeg $ phiS 2)
                        ]
     premisesOf AX    = []
     premisesOf DD    = [ GammaV 1 :|-: ss (phiS 1) ]
     premisesOf DNE   = [ GammaV 1 :|-: ss (PNeg $ PNeg $ phiS 1) ]
     premisesOf DNI   = [ GammaV 1 :|-: ss (phiS 1) ]
     premisesOf CP1   = [ GammaV 1 :+: sa (phiS 1) :|-: ss (phiS 2) ]
     premisesOf CP2   = [ GammaV 1 :|-: ss (phiS 2) ]
     premisesOf ID1   = [ GammaV 1 :+: sa (phiS 1) :|-: ss (phiS 2) 
                        , GammaV 2 :+: sa (phiS 1) :|-: ss (PNeg $ phiS 2)
                        ]
     premisesOf ID2   = [ GammaV 1 :+: sa (phiS 1) :|-: ss (phiS 2) 
                        , GammaV 2 :|-: ss (PNeg $ phiS 2)
                        ]
     premisesOf ID3   = [ GammaV 1  :|-: ss (phiS 2) 
                        , GammaV 2 :+: sa (phiS 1) :|-: ss (PNeg $ phiS 2)
                        ]
     premisesOf ID4   = [ GammaV 1  :|-: ss (phiS 2) 
                        , GammaV 2  :|-: ss (PNeg $ phiS 2)
                        ]
     premisesOf ADJ   = [ GammaV 1  :|-: ss (phiS 1) 
                        , GammaV 2  :|-: ss (phiS 2)
                        ]
     premisesOf S1    = [ GammaV 1  :|-: ss (phiS 1 :&: phiS 2) ]
     premisesOf S2    = [ GammaV 1  :|-: ss (phiS 1 :&: phiS 2) ]
     premisesOf ADD1  = [ GammaV 1  :|-: ss (phiS 1) ]
     premisesOf ADD2  = [ GammaV 1  :|-: ss (phiS 1) ]
     premisesOf MTP1  = [ GammaV 1  :|-: ss (PNeg $ phiS 1) 
                        , GammaV 2  :|-: ss (phiS 1 :||: phiS 2)
                        ]
     premisesOf MTP2  = [ GammaV 1  :|-: ss (PNeg $ phiS 1) 
                        , GammaV 2  :|-: ss (phiS 2 :||: phiS 1)
                        ]
     premisesOf BC1   = [ GammaV 1  :|-: ss (phiS 1 :<->: phiS 2) ]
     premisesOf BC2   = [ GammaV 1  :|-: ss (phiS 1 :<->: phiS 2) ]
     premisesOf CB    = [ GammaV 1  :|-: ss (phiS 1 :->: phiS 2)
                        , GammaV 2  :|-: ss (phiS 2 :->: phiS 1) ]
     premisesOf UI    = [ GammaV 1  :|-: ss (PBind (All "v") (phi 1))]
     premisesOf EG    = [ GammaV 1 :|-: ss (phi 1 tau)]
     premisesOf UD    = [ GammaV 1 :|-: ss (phi 1 tau)]
     premisesOf ED1   = [ GammaV 1 :+:  sa (phi 1 tau) :|-: ss (phiS 1)
                        , GammaV 2 :|-: ss (PBind (Some "v") $ phi 1)
                        , sa (phi 1 tau) :|-: ss (phi 1 tau)]
     premisesOf ED2   = [ GammaV 1 :|-: ss (phiS 1)
                        , sa (phi 1 tau) :|-: ss (phi 1 tau)
                        , GammaV 2 :|-: ss (PBind (Some "v") $ phi 1)]
     premisesOf (DER r) = zipWith gammafy (premises r) [1..]
        where gammafy p n = GammaV n :|-: SS (liftToSequent p)
     premisesOf QN1   = [ GammaV 1 :|-: ss (PNeg $ PBind (Some "v") $ phi 1)]
     premisesOf QN2   = [ GammaV 1 :|-: ss (PBind (Some "v") $ \x -> PNeg $ phi 1 x)]
     premisesOf QN3   = [ GammaV 1 :|-: ss (PNeg $ PBind (All "v") $ phi 1)]
     premisesOf QN4   = [ GammaV 1 :|-: ss (PBind (All "v") $ \x -> PNeg $ phi 1 x)]

     conclusionOf MP    = (GammaV 1 :+: GammaV 2) :|-: ss (phiS 2)
     conclusionOf MT    = (GammaV 1 :+: GammaV 2) :|-: ss (PNeg $ phiS 1)
     conclusionOf AX    = sa (phiS 1) :|-: ss (phiS 1)
     conclusionOf DD    = GammaV 1 :|-: ss (phiS 1) 
     conclusionOf DNE   = GammaV 1 :|-: ss (phiS 1) 
     conclusionOf DNI   = GammaV 1 :|-: ss (PNeg $ PNeg $ phiS 1) 
     conclusionOf CP1   = GammaV 1 :|-: ss (phiS 1 :->: phiS 2) 
     conclusionOf CP2   = GammaV 1 :|-: ss (phiS 1 :->: phiS 2)
     conclusionOf ID1   = GammaV 1 :+: GammaV 2 :|-: ss (PNeg $ phiS 1)
     conclusionOf ID2   = GammaV 1 :+: GammaV 2 :|-: ss (PNeg $ phiS 1)
     conclusionOf ID3   = GammaV 1 :+: GammaV 2 :|-: ss (PNeg $ phiS 1)
     conclusionOf ID4   = GammaV 1 :+: GammaV 2 :|-: ss (PNeg $ phiS 1)
     conclusionOf ADJ   = GammaV 1 :+: GammaV 2 :|-: ss (phiS 1 :&: phiS 2)
     conclusionOf S1    = GammaV 1 :|-: ss (phiS 1)
     conclusionOf S2    = GammaV 1 :|-: ss (phiS 2)
     conclusionOf ADD1  = GammaV 1 :|-: ss (phiS 2 :||: phiS 1)
     conclusionOf ADD2  = GammaV 1 :|-: ss (phiS 1 :||: phiS 2)
     conclusionOf MTP1  = GammaV 1 :+: GammaV 2 :|-: ss (phiS 2)
     conclusionOf MTP2  = GammaV 1 :+: GammaV 2 :|-: ss (phiS 2)
     conclusionOf BC1   = GammaV 1 :|-: ss (phiS 2 :->: phiS 1)
     conclusionOf BC2   = GammaV 1 :|-: ss (phiS 1 :->: phiS 2)
     conclusionOf CB    = GammaV 1 :+: GammaV 2 :|-: ss (phiS 1 :<->: phiS 2)
     conclusionOf UI    = GammaV 1 :|-: ss (phi 1 tau)
     conclusionOf EG    = GammaV 1 :|-: ss (PBind (Some "v") (phi 1))
     conclusionOf UD    = GammaV 1 :|-: ss (PBind (All "v") (phi 1))
     conclusionOf ED1   = GammaV 1 :+: GammaV 2 :|-: ss (phiS 1)
     conclusionOf ED2   = GammaV 1 :+: GammaV 2 :|-: ss (phiS 1)
     conclusionOf (DER r) = gammas :|-: SS (liftToSequent $ conclusion r)
        where gammas = foldl (:+:) Top (map GammaV [1..length (premises r)])
     conclusionOf QN1   = GammaV 1 :|-: ss (PBind (All "v") $ \x -> PNeg $ phi 1 x)
     conclusionOf QN2   = GammaV 1 :|-: ss (PNeg $ PBind (All "v")  $ phi 1)
     conclusionOf QN3   = GammaV 1 :|-: ss (PBind (Some "v") $ \x -> PNeg $ phi 1 x)
     conclusionOf QN4   = GammaV 1 :|-: ss (PNeg $ PBind (Some "v") $ phi 1)

     restriction UD     = Just (eigenConstraint (SeqT 1) (ss (PBind (All "v") $ phi 1)) (GammaV 1))
     restriction ED1    = Just (eigenConstraint (SeqT 1) (ss (PBind (Some "v") $ phi 1) :-: ss (phiS 1)) (GammaV 1 :+: GammaV 2))
     restriction ED2    = Nothing --Since this one does not use the assumption with a fresh object
     restriction _      = Nothing

     indirectInference x
        | x `elem` [ CP1,CP2,ED1,ED2 ] = Just PolyProof
        | x `elem` [ ID1,ID2,ID3,ID4 ] = Just DoubleProof
        | otherwise = Nothing

eigenConstraint c suc ant sub
    | c' `occursIn` ant' = Just $ "The constant " ++ show c' ++ " appears not to be fresh, given that this line relies on " ++ show ant'
    | c' `occursIn` suc' = Just $ "The constant " ++ show c' ++ " appears not to be fresh in the other premise " ++ show suc'
    | otherwise = case fromSequent c' of 
                          PC _ -> Nothing
                          PT _ -> Nothing
                          _ -> Just $ "The term " ++ show c' ++ " is not a constant"
    where c'   = applySub sub c
          ant' = applySub sub ant
          suc' = applySub sub suc
          -- XXX : this is not the most efficient way of checking
          -- imaginable.
          occursIn x y = not $ (subst x (static 0) y) =* y

parseFOLogic :: Map String DerivedRule -> Parsec String u [FOLogic]
parseFOLogic ders = 
                do r <- choice (map (try . string) 
                            [ "AS","PR","MP","MTP","MT","DD","DNE"
                            , "DNI", "DN", "S", "ADJ",  "ADD" , "BC"
                            , "CB",  "CD", "ID", "UI", "UD", "EG", "ED"
                            , "D-", "QN"])
                   case r of "AS"   -> return [AX]
                             "PR"   -> return [AX]
                             "MP"   -> return [MP]
                             "MT"   -> return [MT]
                             "DD"   -> return [DD]
                             "DNE"  -> return [DNE]
                             "DNI"  -> return [DNI]
                             "DN"   -> return [DNE,DNI]
                             "CD"   -> return [CP1,CP2]
                             "ID"   -> return [ID1,ID2,ID3,ID4]
                             "ADJ"  -> return [ADJ]
                             "S"    -> return [S1, S2]
                             "ADD"  -> return [ADD1, ADD2]
                             "MTP"  -> return [MTP1, MTP2]
                             "BC"   -> return [BC1, BC2]
                             "CB"   -> return [CB]
                             "UI"   -> return [UI]
                             "UD"   -> return [UD]
                             "EG"   -> return [EG]
                             "ED"   -> return [ED1,ED2]
                             "QN"   -> return [QN1, QN2, QN3, QN4]
                             "D-" -> do rn <- many1 upper
                                        case M.lookup rn ders of
                                            Just r  -> return [DER r]
                                            Nothing -> parserFail "--- Looks like you're citing a derived rule that doesn't exist"

parseFOLProof ::  Map String DerivedRule -> String -> [DeductionLine FOLogic PureLexiconFOL (Form Bool)]
parseFOLProof ders = toDeduction (parseFOLogic ders) folFormulaParser

--------------------
--  3. System QL  --
--------------------
-- A system of first-order logic resembling system QL from PD Magnus'
-- forallx

data ForallxQL = ForallxSL P.ForallxSL | UIX | UEX | EIX | EE1X | EE2X
                    deriving (Show, Eq)

instance Inference ForallxQL PureLexiconFOL where

         ruleOf UIX   = [ GammaV 1 :|-: ss (phi 1 tau)]
                        ∴ GammaV 1 :|-: ss (PBind (All "v") (phi 1))
         ruleOf UEX   = [ GammaV 1  :|-: ss (PBind (All "v") (phi 1))]
                        ∴ GammaV 1 :|-: ss (phi 1 tau)
         ruleOf EIX   = [ GammaV 1 :|-: ss (phi 1 tau)]
                        ∴ GammaV 1 :|-: ss (PBind (Some "v") (phi 1))
         ruleOf EE1X  = [ GammaV 1 :+:  sa (phi 1 tau) :|-: ss (phiS 1)
                        , GammaV 2 :|-: ss (PBind (Some "v") $ phi 1)
                        , sa (phi 1 tau) :|-: ss (phi 1 tau)
                        ] ∴ GammaV 1 :+: GammaV 2 :|-: ss (phiS 1) 
         ruleOf EE2X  = [ GammaV 1 :|-: ss (phiS 1)
                        , sa (phi 1 tau) :|-: ss (phi 1 tau)
                        , GammaV 2 :|-: ss (PBind (Some "v") $ phi 1)
                        ] ∴ GammaV 1 :+: GammaV 2 :|-: ss (phiS 1)

         premisesOf (ForallxSL x) = map liftSequent (premisesOf x)
         
         conclusionOf (ForallxSL x) = liftSequent (conclusionOf x)

         indirectInference (ForallxSL x) = indirectInference x
         indirectInference x  
            | x `elem` [ EE1X,EE2X ] = Just PolyProof
            | otherwise = Nothing

         restriction UIX    = Just (eigenConstraint (SeqT 1) (ss (PBind (All "v") $ phi 1)) (GammaV 1))
         restriction EE1X   = Just (eigenConstraint (SeqT 1) (ss (PBind (Some "v") $ phi 1) :-: ss (phiS 1)) (GammaV 1 :+: GammaV 2))
         restriction EE2X   = Nothing --Since this one does not use the assumption with a fresh object
         restriction _      = Nothing

parseForallxQL ders = try liftProp <|> quantRule
    where liftProp = do r <- P.parseForallxSL ders
                        return (map ForallxSL r)
          quantRule = do r <- choice (map (try . string) ["∀I", "AI", "∀E", "AE", "∃I", "EI", "∃E", "EE"])
                         case r of 
                            "∀I" -> return [UIX]
                            "AI" -> return [UIX]
                            "∀E" -> return [UEX]
                            "AE" -> return [UEX]
                            "∃I" -> return [EIX]
                            "EI" -> return [EIX]
                            "∃E" -> return [EE1X, EE2X]
                            "EE" -> return [EE1X, EE2X]
