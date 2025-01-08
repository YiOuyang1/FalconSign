# Sampler Module README

This section provides a brief overview of the key modules and IP cores used in this design, along with their configurations.

## Vivado 2022.2 IP Configuration
`fp_flt2i_int32_s`

- Precision of Input:  Double

- Precision of Result:  Int32

- Optimizations:  

- Interface Options:  NonBlocking and 1 cycle latency

`fp_i2flt_int32_s`

- Precision of Input:  Int32

- Precision of Result:  Double

- Optimizations:  

- Interface Options:  NonBlocking and 1 cycle latency

`fp_flt2i_int64_s`

- Precision of Input:  Double

- Precision of Result:  Custom (Integer Width = 64, Fraction Width = 0)

- Optimizations:  

- Interface Options:  NonBlocking and 1 cycle latency

`fp_i2flt_int64_s`

- Precision of Input:  Int64

- Precision of Result:  Double

- Optimizations:  

- Interface Options:  NonBlocking and 1 cycle latency

`fp_sub_s`

- Precision:  Double

- Optimizations:  High Speed and Full Usage

- Interface Options:  NonBlocking and 2 cycle latency

`fp_mult_s`

- Precision:  Double

- Optimizations:  High Speed and Full Usage

- Interface Options:  NonBlocking and 2 cycle latency

