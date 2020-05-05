-- From a given position, scan your way out (spiraling, until an asteroid is found)


data Cell = Asteroid Int Int | Space Int
type Grid = [[Cell]]

scan :: (Int, Int) -> 