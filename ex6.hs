--Ex 5
concat_ex :: [[a]] -> [a]

concat_ex [] = []
concat_ex (xs : xss) = xs : concat_ex xss
