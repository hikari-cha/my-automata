module Automata.DFA
  ( DFA (..),
    runDFA,
    isAccepted,
  )
where

data DFA state symbol = DFA
  { transition :: state -> symbol -> state,
    initialState :: state,
    isFinalState :: state -> Bool
  }

runDFA :: DFA state symbol -> [symbol] -> state
runDFA dfa = foldl (transition dfa) (initialState dfa)

isAccepted :: DFA state symbol -> [symbol] -> Bool
isAccepted dfa = isFinalState dfa . runDFA dfa
