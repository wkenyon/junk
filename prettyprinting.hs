import Text.PrettyPrint

data N = Br Int N N
       | Lf

instance Show N where
           show = show . doc
                where doc (Br i t r) = (int i) $$ (nest 1 (doc t)) $$ (nest 1 (doc r))
	              doc Lf = empty
