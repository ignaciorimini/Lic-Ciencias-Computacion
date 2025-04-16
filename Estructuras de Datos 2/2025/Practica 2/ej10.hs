-- [[]] ++ xs = xs
-- Tiene sentido si xs es una lista de listas [[a]] o [] -> afirmación falsa.

-- [[]] ++ xs = [xs]
-- Tiene sentido si xs es una lista de listas. Sin embargo, la afirmación es falsa, pues [[]] ++ xs = [[], xs]

-- [[]] ++ xs = [] : xs
-- Tiene sentido si xs es una lista de listas. La afirmación es verdadera.

-- [[]] ++ xs = [[], xs]
-- Tiene sentido si xs es una lista de listas. La afirmación es verdadera.

-- [[]] ++ [xs] = [[], xs]
-- Tiene sentido si xs es una lista [a]. La afirmación es verdadera.

-- [[]] ++ [xs] = [xs]
-- Tiene sentido si xs es una lista [a]. La afirmación es falsa.
