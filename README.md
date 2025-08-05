# Medusa Eats (Enhanced Snake)

A terminal-based Snake game written in Haskell, now simplified: collect only **Extra Food** power-ups and complete **Speed Up** challenges to boost your score. Enjoy smooth differential rendering and non-blocking input handling.

---

## Table of Contents

* [Features](#features)
* [Prerequisites](#prerequisites)
* [Build & Run](#build--run)
* [Controls](#controls)
* [Project Structure](#project-structure)
* [How It Works](#how-it-works)
* [Contributing](#contributing)
* [License](#license)

---

## Features

* **Extra-Food Power-Up (`?`)** — Grants **+10** bonus points and pauses the game briefly to display a notification.
* **Speed-Up Challenge (`!`)** — Grants **+15** points and sets your speed boost counter to **120** moves.
* **Pause Messages** — A 20-step pause (`gamePaused`) shows the `lastMessage` when you collect an item.
* **Differential Rendering** — Only redraws cells that changed, minimizing flicker.
* **Concurrency** — Uses `forkIO` and `STM` (`TVar`) for non-blocking WASD input.

---

## Prerequisites

* **GHC** (Glasgow Haskell Compiler) ≥ 9.6.7
* **Cabal** build tool
* A terminal or console with ANSI cursor support

---

## Build & Run

1. **Clone this repository**

   ```bash
   git clone https://github.com/akshaya-bs/Medusa_Eats-.git
   cd Medusa_Eats-
   ```

2. **Build with Cabal**

   ```bash
   cabal update   # ensure package index is fresh
   cabal build
   ```

3. **Run the game**

   ```bash
   cabal run enhanced-snake
   ```

---

## Controls

| Key     | Action        |
| ------- | ------------- |
| `W`/`w` | Move Up       |
| `A`/`a` | Move Left     |
| `S`/`s` | Move Down     |
| `D`/`d` | Move Right    |
| `Q`/`q` | Quit the game |

---

## Project Structure

```
Medusa_Eats-/
├── app/
│   └── Main.hs          # Game implementation
├── enhanced-snake.cabal # Cabal package file
├── CHANGELOG.md         # Change history
├── LICENSE              # BSD-3-Clause license
└── README.md            # This file
```

---

## How It Works

1. **World State**: A `World` record tracks the snake, food, power-ups, challenges, score, and timers (`invincible`, `speedBoost`, `gamePaused`, `lastMessage`).
2. **Advance Function**: `advance :: World -> Direction -> World` handles movement, eating, spawning items, and pause logic.
3. **Randomness**: A single `StdGen` (`rand`) threads through `randomFreePosition`, `generatePowerUp`, and `generateChallenge`.
4. **Rendering**: `renderWorldDiff` compares `World` snapshots to clear old cells and draw only new ones.
5. **Input Loop**: A separate `forkIO` thread writes to a `TVar Direction` so the main loop never blocks on `getChar`.
6. **Game State**: `toGameState` wraps `World` into `Playing`, `Paused`, or `GameOver` states, determining when to show messages, freeze, or exit.

---

## Contributing

Contributions and improvements are welcome:

1. Fork this repository.
2. Create a feature branch: `git checkout -b feature-name`.
3. Commit your changes: `git commit -m "Add new feature"`.
4. Push to your branch: `git push origin feature-name`.
5. Open a Pull Request to `main`.

---

## License

This project is licensed under the **BSD-3-Clause** License. See [LICENSE](./LICENSE) for details.

