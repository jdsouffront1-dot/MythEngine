# **MythEngine**

### *Experimental Framework for Information–Coherence Dynamics*

**MythEngine** is an experimental computational framework for studying how information systems evolve through cycles of coherence, entropy, and causal reconstruction.
It models reality as a dynamic network of interacting informational units and measures how stability and structure emerge from recursive feedback processes.

---

## **1. Overview**

MythEngine operates as a multi-phase research system.
Each phase represents a computational experiment focused on a specific property of informational dynamics — from geometric structure formation to temporal coherence analysis.

The system quantifies how patterns of information self-organize, stabilize, and propagate over time using formal measures of coherence, entropy, and causal flow.

---

## **2. Core Concepts**

* **Meaning Density (MD):** Information intensity per local element or field.
* **Φ (Phi):** Degree of global integration or system-wide coherence.
* **ε (Epsilon):** Entropic divergence — deviation from equilibrium.
* **CRI (Causal Reconstruction Index):** Directional coherence flow between variables.
* **TSI (Temporal Stability Index):** Persistence of coherence patterns across time.

These metrics provide a consistent mathematical basis for analyzing how informational structures balance order and uncertainty through iterative processes.

---

## **3. Implementation**

The framework is implemented in **Python** with modular, reproducible phases.
Each phase performs data transformations, analytical computations, and visualization of results.
Execution is coordinated via **PowerShell scripts** to ensure reproducibility and sequential coherence between phases.

Recent phases include:



All intermediate and final data are stored in structured `.csv` files within the `/data` directory.

---

## **4. Research Scope**

**MythEngine** supports research in the following areas:

* **Information physics:** coherence and entropy as structural invariants.
* **Complex systems:** modeling emergent stability in feedback networks.
* **Computational neuroscience:** information integration and causal flow.
* **Artificial intelligence:** coherence-based adaptive computation.
* **System theory and control:** recursive stabilization mechanisms.

---

## **5. Repository Structure**

```
/data                # Input/output datasets for each phase  
/scripts             # PowerShell orchestration and automation  
/notebooks           # Analysis and visualization notebooks  
/phaseXX_*.py        # Core computational modules for each phase  
README.md            # Project overview and usage guide
```

---

## **6. Usage**

### **Setup**

```bash
# Create and activate virtual environment
python -m venv .venv
.\.venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### **Run a Phase**

```powershell
# Execute a specific phase script
& .\scripts\run_phase25.ps1
```

Each PowerShell script will:

1. Activate the environment
2. Run the corresponding Python module
3. Save output data and visualizations under `/data`

### **Output**

* Numeric results (e.g., coherence indices, variances) → `.csv`
* Visual summaries (field maps, phase plots) → `.png`

---

## **7. Current Development Goals**

* Extend causal reconstruction analysis to multi-variable systems.
* Integrate temporal stability metrics with adaptive feedback models.
* Develop a unified dashboard for cross-phase visualization.

---

## **8. Citation**

If you use or reference MythEngine in research, please cite:

```
Souffront, J. (2025). MythEngine: Experimental Framework for Information–Coherence Dynamics.
```

---




---

Author

Juan D. Souffront
Independent Researcher | MythEngine Project Lead
📧 souffrontjuan@gmail.com


