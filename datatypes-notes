func a b c =
  case a, b, c of
    1, 2, 3 -> body1
    4, 5, 6 -> body2
    1, 2, 4 -> body3

func a b c =
  case a, b, c of
    Left x, Left y, 3 -> body1
    Right r, Left y, 3 -> body2 -- this should be rearranged :(
    Left x', Left y', 4 -> body3

func a b c =
  case a of
    1 -> 
      case b of
        2 ->
          case c of
            3 -> body1
    1 ->
      case b of
        2 ->
          case c of
            4 -> body3
    4 -> 
      case b of
        5 ->
          case c of
            6 -> body2

Can combining be done by exact equality on the term level only?
Needs to be alpha equality?

1. Combine by equality at the term level (alpha equality)
2. Fancy rearranging for the above case 
look up compiling Agda to case trees
