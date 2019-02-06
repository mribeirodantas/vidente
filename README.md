# vidente

vidente is an R package that contains tools to parse and analyze the Surveillance, Epidemiology, and End Results (SEER) Program database.
Though there are other R packages with similar goals, they're either too limited or too focused in one way of parsing/analyzing it.
vidente has been developed making sure it will be useful to anyone willing to analyze SEER data, or at least parse its ASCII data files.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

TODO

```
Give examples
```

### Installing

Download a [version](https://github.com/mribeirodantas/vidente/releases) of vidente, decompress the compressed filed, change into the created directory, and run in your shell:
```
R CMD INSTALL --preclean --no-multiarch --with-keep.source .
```
After that, you should be able to load it in R by typing:
```
library(vidente)
```

You can also clone this repository and run the two commands above inside the cloned directory.

## Contributing

Contributions are very welcomed and feel free to reach out by e-mail if you want to discuss something with me.

## Maintainer/Authors

* **Marcel Ribeiro Dantas** (marcel.ribeiro-dantas@curie.fr)

## License

This project is licensed under the General Public License version 2 or any other later released version fo this license. See the [LICENSE.md](LICENSE.md) file for details
