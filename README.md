# FinTrust COBOL
> 💼 A legacy-inspired fintech simulation built in COBOL for modern-day credibility and historic awareness.

FinTrust COBOL is a minimalist COBOL-based finance simulation built to demonstrate the robustness and continued relevance of legacy financial systems. This repo simulates core banking operations like account creation, balance management, ledger summaries, and to demonstrate transaction processing using COBOL, the language still running trillions of dollars in global finance.

> This repo is strictly for educational and demonstrative purposes only. It is in no way, shape, or form a banking institution.

---

## 🏦 Why COBOL?
COBOL (Common Business-Oriented Language) powers most of the world's banking infrastructure. Despite being developed in 1959, it's still heavily used in mainframe environments across banks, insurance companies, and government systems. This project celebrates its resilience while planting my own flag in the fintech space.

---

## 💡 Features
- Create and manage virtual customer accounts
- Simulate deposits, withdrawals, and transfers
- Generate simple account statements
- Maintain a persistent flat file record system
- Clean, modular COBOL source code (tested with GnuCOBOL)

---

## 🧰 Requirements
- GnuCOBOL (install via `sudo apt install open-cobol` or `gnucobol`)
- Make or shell script (optional for automation)
- A love for old-school elegance 🖥️

---

## 🔧 Running the Project
To compile a module:
```bash
cobc -x -o bin/mainmenu src/mainmenu.cbl
```

To run:
```bash
./bin/mainmenu
```
