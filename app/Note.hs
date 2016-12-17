module Note where

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

noteToInt :: Note -> Int
noteToInt C  = 64
noteToInt Cs = 65
noteToInt D  = 66
noteToInt Ds = 67
noteToInt E  = 68
noteToInt F  = 69
noteToInt Fs = 70
noteToInt G  = 71
noteToInt Gs = 72
noteToInt A  = 73
noteToInt As = 74
noteToInt B  = 75

