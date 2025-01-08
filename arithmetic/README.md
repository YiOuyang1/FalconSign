# Arithmetic Module README

This section provides a brief overview of the key modules and IP cores used in this design, along with their configurations.

## Vivado 2022.2 IP Configuration
`floating_point_add_2pip`

- Precision:  Double

- Optimizations:  Low Latency

- Interface Options:  NonBlocking and 2 cycle latency

`floating_point_sub_2pip`

- Precision:  Double

- Optimizations:  Low Latency

- Interface Options:  NonBlocking and 2 cycle latency


## Non-Open-Source Modules
Due to licensing or code review restrictions, the following modules are not included in this release:

`fpr_mul_opt_2pipe`

- Describe:This module is an optimized version of a pipelined floating-point multiplier with a latency of 2 cycles.
  
`fpr_mul_const_opt_2pipe`

- Describe:This module is an optimized version of a pipelined floating-point multiplier with a latency of 2 cycles, where one input is a constant.

`counter_ce`

- Describe:counter

`rbsh`

- Describe:This module performs circular shift operations
