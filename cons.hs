data List a = Cons a (List a)
            | Nil

f = case Cons 1 Nil of {Nil -> 0; Cons x xs -> x;}