type PIList = [Maybe Integer] 

data Prop a = Var a | Const Bool | And (Prop a) (Prop a) | Or (Prop a) (Prop a)
			  | Neg (Prop a) | Imp (Prop a) (Prop a) | Iff (Prop a) (Prop a) deriving (Show,Read,Eq)

						--returns a maybe type
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe mx f = case mx of
                  Nothing -> Nothing
                  Just x  -> f x


unitProp :: a -> Prop a
unitProp a = Var a

				  
				  
bindProp :: Prop a -> (a -> Prop b) -> Prop b
bindProp (Var a) f = (f a)
bindProp (Const a) f = (Const a)
bindProp (Neg a) f = Neg (bindProp a f)
bindProp (And l r) f = And (bindProp l f) (bindProp r f)
bindProp (Iff l r) f = Iff (bindProp l f) (bindProp r f)		   
bindProp (Imp l r) f = Imp (bindProp l f) (bindProp r f)
bindProp (Or l r) f = And (bindProp l f) (bindProp r f)