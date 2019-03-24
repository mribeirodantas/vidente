# vidente

vidente is an R package that contains tools to parse and preprocess the Surveillance, Epidemiology, and End Results (SEER) Program database. There are other R packages with similar goals, but they're either too limited or too focused in one way of parsing/preprocessing/analyzing SEER data. vidente has been developed to make sure it will be useful to anyone willing to analyze SEER data, regardless of technical knowledge.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. I do not consider it to be ready for production yet, even though I already use it in my analyses.

### Installing

Download a [version](https://github.com/mribeirodantas/vidente/releases) of vidente, decompress the compressed file, change into the created directory, and run in your shell:
```
R CMD INSTALL --preclean --no-multiarch --with-keep.source .
```
After that, you should be able to load it in R by typing:
```
library(vidente)
```

You can also clone this repository and run the R command above inside the cloned directory.

## Contributing

Contributions are very welcomed (more details [here](.github/CONTRIBUTING.md)) and feel free to reach out by e-mail if you want to discuss something with me.

## Maintainer

* **Marcel Ribeiro Dantas** (marcel.ribeiro-dantas curie.fr)

## Authors
* **Marcel Ribeiro Dantas**
* **Hervé Isambert**

## License

This project is licensed under the General Public License version 3 or any other version of this license released later. See the [LICENSE](LICENSE) file for details
