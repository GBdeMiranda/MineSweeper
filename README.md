# Minesweeper Game in Haskell

This project is a Minesweeper game implemented in Haskell. The main modules are `Board.hs`, `Main.hs`, and `Square.hs`.

## Project Description

The Minesweeper game allows users to open, mark, and unmark squares on the board. The game checks for victory conditions and handles invalid commands. The game is initiated by the `main` function in `Main.hs`.

### Main Features

- `Board.hs`: Contains functions and data structures related to the game board.
- `Main.hs`: Contains the main game logic and user interaction.
- `Square.hs`: Contains functions and data structures related to individual squares on the board.

## Compilation and Running Instructions

To compile and run the Minesweeper game, follow these steps:

1. Ensure you have the Haskell compiler (GHC) installed on your system.
2. Open a terminal and navigate to the project directory.
3. Run the following command to compile the project:

   ```
   ghc --make Main.hs
   ```

4. After the compilation is complete, run the executable:

   ```
   ./Main
   ```

## How to Play the Game

The game starts by asking the user to input the number of rows, columns, and mines for the board. After the board is created, the user can interact with the game using the following commands:

- `_ <position>`: Open a square at the specified position (e.g., `A01`, `D44`, `B13`).
- `+ <position>`: Mark a square at the specified position (e.g., `+D92`, `+C04`).
- `- <position>`: Unmark a square at the specified position (e.g., `-D02`, `-C40`).

The game continues until the user either wins by opening all non-mine squares or loses by opening a mine.
