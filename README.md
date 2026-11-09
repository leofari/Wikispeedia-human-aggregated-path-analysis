# Wikispeedia: Human vs Structural Navigation in Wikipedia

**Author:** Leo Farina  
**Type:** Independent research project  
**Tools:** R, igraph, tidyverse

---

## Overview

This project studies how humans navigate Wikipedia compared to how the network is structured by hyperlinks.

Using data from the **Wikispeedia** game, where users attempt to navigate from one Wikipedia article to another using only hyperlinks, I construct two parallel networks:

- **Structural network** — the true Wikipedia hyperlink graph  
- **Human network** — a weighted graph derived from observed user click behavior  

By comparing these networks using **category-based metrics**, this project asks:

> *How does human wayfinding differ from the objective structure of the network?*

Rather than focusing on shortest paths or individual users, this analysis examines **aggregate navigation behavior** at the population level.

---

## Core idea

Wikipedia articles belong to semantic **categories** (e.g., *Biology*, *History*, *Music*).  
These categories provide a natural semantic reference frame for studying navigation.

From this, I define two metrics:

### 1. Retention  
The probability that after *n* clicks, a user remains within the same semantic category as the starting article.

> “Do humans stay within topic areas, or do they jump across them?”

### 2. Connectivity  
The expected number of categories associated with the article reached after *n* clicks.

> “How broadly does navigation expose users to different topics?”

These are computed for both:
- the **structural graph** (uniform traversal of hyperlinks)
- the **human graph** (weighted by observed click probabilities)

This produces directly comparable human vs structural statistics at 1 and 2 steps.

---

## What this project finds

The main results are:

- **Human navigation has lower and more variable category retention** than the structural network.
- **Humans exit semantic regions more often**, even when the network structure supports staying inside them.
- **Connectivity homogenizes more slowly** in human navigation than in the structural graph.
- **Quit events** tend to occur inside coherent semantic regions, suggesting the presence of *semantic dead-ends* rather than abrupt topic boundaries.

These results indicate that human navigation reflects **local salience and cognitive heuristics**, not just the topology of the hyperlink network.

Full details and figures are in the paper.

---

## Repository contents
```
.
├── wikispeedia_analysis_farina.pdf
├── wikispeedia.R
└── README.md
```

---

## Data

The data come from:

**Wikispeedia — Human Wayfinding in Information Networks**  
Robert West & Jure Leskovec (2012)  
Stanford SNAP Dataset Collection  

Download here:  
https://snap.stanford.edu/data/wikispeedia.html

You will need the following files from the dataset:

- `links.tsv`
- `articles.tsv`
- `categories.tsv`
- `paths_finished.tsv`
- `paths_unfinished.tsv`

Place them in a folder called:

`wikispeedia_paths-and-graph/`

in the same directory as the R script.

---

## How to run

1. Install required R packages:
```r
install.packages(c("readr","dplyr","igraph","tidyr","ggplot2"))
```
2. Download the Wikispeedia dataset from Stanford SNAP.

3. Place the .tsv files in wikispeedia_paths-and-graph/.

4. Run:
```r
source("wikispeedia.R")
```

The script will:

- Build all graphs
- Compute all metrics
- Generate all figures used in the paper

Note: Some steps (path parsing and graph construction) are computationally expensive and may take several minutes.


## Methodological note

Human paths are decomposed into local click transitions and aggregated into a global weighted transition graph.
This means the analysis captures population-level navigation tendencies, not individual goal-directed paths.

This design choice allows direct comparison between:

- uniform traversal of the structural graph
- empirically observed human traversal

but does not model user-specific strategies.

## Citation

If you use this work or build on it, please cite:

West, R., & Leskovec, J. (2012).
Human Wayfinding in Information Networks.
Proceedings of WWW 2012.


## License

This repository contains only original analysis code and writing.
The Wikispeedia dataset is distributed separately by Stanford SNAP and remains under their license.
