
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Final\_Brain\_Seg

The goal of Final\_Brain\_Seg is the results for Brain Extraction for CT
Imaging.

## Original Code

The original code for the paper is located at
<https://github.com/muschellij2/CT_BET/blob/master/Skull_Strip_Paper/CT_Skull_Strip_Example.R>
and
<https://github.com/muschellij2/CT_BET/blob/master/Skull_Strip_Paper/CT_Skull_Strip_Example.sh>.

## Updated Code

The `ichseg` package (<https://github.com/muschellij2/ichseg>) has
`CT_Skull_Strip` and `CT_Skull_Strip_robust`, which perform the steps
for brain extraction. The robust methodology is unpublished but more
robust than the original methodology, especially when neck slices are
present.

The `CT_Skull_Stripper` function wraps these 2 with the logical flag
`robust` for easier comparison.

# Links for Brain Extraction for CT

Additional methods have been coming out with CT extraction

  - <https://www.nitrc.org/projects/demon>
  - <https://github.com/aqqush/CT_BET>
  - <https://github.com/WuChanada/StripSkullCT>
