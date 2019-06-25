# Sokoban

An implementation of the classic Sokoban game in Haskell. The fully functioning
prototype including one hardcoded map and a very simple terminal UI was build
over the course of two train rides.

# Installation

Stack is needed to run this Sokoban implementation. Clone the project, change
into the root directory of the project and use

```
stack build && stack exec sokoban
```

to first build and the run Sokoban.

# How to play

The classic `wasd` keys are used for navigation, however you have to confirm
each input with the return key for now. The game will stop once you solve the
first map.

# Textual Representation

| Type           | Char |
|----------------|------|
| Wall           | `#`  |
| Player         | `@`  |
| Player on goal | `+`  |
| Box            | `$`  |
| Box on goal    | `*`  |
| Goal           | `.`  |
| Floor          | ` `  |

# Rules

Push all boxes on top of goals. Pulling box is not possible.
