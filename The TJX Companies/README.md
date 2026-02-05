---

# 📘 **The TJX Companies: FX Risk Hedging Model**

A Monte‑Carlo–driven financial model that simulates **foreign‑exchange risk**, **hedge effectiveness**, and **profit volatility** for The TJX Companies.  
This project demonstrates practical skills in **financial modeling**, **risk analytics**, **scenario design**, and **VBA automation** — all wrapped into a clean, reproductible Excel engine.

---

## 📌 **Project Overview**

TJX sources a significant portion of its merchandise internationally, exposing the company to FX fluctuations (primarily EUR/USD, GBP/USD, CAD/USD, AUD/USD, and PLN/EUR).  
This model quantifies how FX volatility affects:

- Merchandise cost  
- Freight cost  
- Hedge performance  
- Net profit  
- Margin stability  
- Simulated Net Profit (95% VaR)

The engine runs **1,000+ Monte Carlo simulations** per scenario and produces a full distribution of profit outcomes.

---

## 🎯 **Key Features**

### **✔ Monte Carlo FX Simulation**
- FX rates generated using a normal distribution  
- Mean = scenario spot rate  
- Standard deviation = spot × volatility  
- Supports six macroeconomic scenarios

### **✔ Dynamic Hedging Logic**
- Hedge ratio applied to total FX‑sensitive cost  
- Hedge payoff calculated using scenario‑specific hedge rates  
- Eliminates over‑hedging distortions

### **✔ Freight Stress Modeling**
- Freight multipliers scale with macro conditions  
- Captures supply‑chain inflation and crisis effects

### **✔ Simulated Net Profit at 95% VaR**
Automatically calculates the 5th‑percentile profit outcome across all trials.

---

## 🧮 **Scenario Framework**

The model includes six pre‑built macroeconomic environments:

| Scenario | Volatility | FX Spot | Hedge Ratio | Hedge Rate | Freight Mult |
|---------|------------|---------|-------------|------------|--------------|
| Low Volatility | 0.06 | 1.15 | 0.40 | 1.16 | 1.75 |
| Stable Market | 0.10 | 1.165 | 0.60 | 1.17 | 2.00 |
| Baseline | 0.12 | 1.18 | 0.75 | 1.18 | 2.25 |
| Inflationary | 0.15 | 1.21 | 0.85 | 1.22 | 2.60 |
| Market Crisis | 0.28 | 1.23 | 0.90 | 1.25 | 3.20 |
| Black Swan | 0.35 | 1.26 | 0.95 | 1.30 | 3.50 |

These scenarios reflect realistic forward‑market behavior, supply‑chain stress, and treasury hedging practices.

---

## ⚙️ **VBA Automation**

The model includes a custom VBA module:

- Builds a clean “TJX_Terminal” output sheet  
- Generates simulation trials  
- Applies the corrected profit formula  
- Computes VaR  
- Formats results for readability  

This ensures the model is **repeatable**, **auditable**, and **easy to extend**.

---

## 📊 **Outputs**

Each run produces:

- FX simulation paths  
- Profit distribution  
- Margin distribution  
- Simulated Net Profit at 95% VaR  
- Scenario‑specific summary metrics  
- A clean dashboard for presentation

---

## 🧠 **Skills Demonstrated**

- Financial modeling (FX, hedging, VaR)  
- Monte Carlo simulation  
- VBA automation  
- Scenario design & stress testing  
- Risk analytics  
- Data visualization  
- Technical documentation  

## 🚀 **Future Enhancements**

Potential extensions include:

- Multi‑currency exposure modeling  
- Hedge effectiveness metrics  
- Forward‑curve–based hedge rate generator  
- Freight cost seasonality  
- Correlated FX paths  

---

 it.
