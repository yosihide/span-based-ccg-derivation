# span-based-ccg-derivation
This code converts a CCG derivation into a span-based representation (SBR).
The SBR is a representation suitable for neural span-based parsing models.
This is based on [A New Representation for Span-based CCG Parsing](https://aclanthology.org/2021.emnlp-main.826/) (EMNLP 2021 short paper).

## Installation
To use this code, you need to install [SBCL](http://www.sbcl.org/) and [cl-ppcre](https://edicl.github.io/cl-ppcre/).
The following command installs SBCL (Ubuntu):
 ```
 apt-get install sbcl
 ```
Create a directory `common-lisp` under your home directory and run the following commands:
```
~/common-lisp$ git clone https://github.com/edicl/cl-ppcre
~/common-lisp$ git clone https://github.com/yosihide/span-based-ccg-derivation
```


## Sample
The following is a sample command of converting CCGbank auto file into SBR file.
The resulting SBR file is in the Penn Treebank (PTB) format.
You can train a parsing model using SBR files (training and development data files) and any off-the-shelf span-based constituency parser such as [Berkeley Neural Parser](https://github.com/nikitakit/self-attentive-parser).
```
cat training.auto | ~/common-lisp/span-based-ccg-derivation/scripts/auto2sbd > training.sbd
```
[`auto2sbd`](scripts/auto2sbd) is a sbcl script file, and you may need to edit the first line to specify the correct path for `sbcl`.

To convert SBR file, usually an output of the span-based parser, into a CCGbank auto file, run the following command:
```
cat test.sbd | ~/common-lisp/span-based-ccg-derivation/scripts/sbd2auto > test.auto
```
[`sbd2auto`](scripts/sbd2auto) is also a sbcl script file.
