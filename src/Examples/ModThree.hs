import Automata.DFA

data ModThreeState = Q0 | Q1 | Q2 deriving (Eq, Show)

data BitDigit = Zero | One deriving (Eq, Show)

modThreeTransition :: ModThreeState -> BitDigit -> ModThreeState
modThreeTransition Q0 Zero = Q0
modThreeTransition Q0 One = Q1
modThreeTransition Q1 Zero = Q2
modThreeTransition Q1 One = Q0
modThreeTransition Q2 Zero = Q1
modThreeTransition Q2 One = Q2

-- | Define the DFA that recognizes binary numbers divisible by 3
--
-- >>> isAccepted modThreeDFA [One, Zero, One]  -- 5 in decimal
-- False
--
-- >>> isAccepted modThreeDFA [One, One, Zero]  -- 6 in decimal
-- True
--
-- >>> isAccepted modThreeDFA [Zero]  -- 0 in decimal
-- True
modThreeDFA :: DFA ModThreeState BitDigit
modThreeDFA =
  DFA
    { transition = modThreeTransition,
      initialState = Q0,
      isFinalState = (== Q0)
    }
